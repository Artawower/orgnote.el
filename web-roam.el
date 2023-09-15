;; web-roam.el --- Show git blame info about current line           -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Artur Yaroshenko

;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/Artawower/web-roam.el
;; Package-Requires: ((emacs "27.1"))
;; Version: v0.8.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provides functionality for syncing org roam notes with external service - Org Note
;; For more detail check https://github.com/Artawower/orgnote project.
;; This is an early alpha version of this app, it may have many bugs and problems, try to back up your notes before using it.
;;; Code:

(require 'json)
(require 'cl)

(defcustom web-roam-execution-script "orgnote-cli"
  "Bin command from cli to execute external script."
  :group 'web-roam
  :type 'string)

(defcustom web-roam-debug-p nil
  "Enable debug mode for better logging."
  :group 'web-roam
  :type 'boolean)

(defcustom web-roam-configuration-file-path "~/.config/orgnote/config.json"
  "Path to configuration file for Org Note."
  :group 'web-roam
  :type 'string)

(defconst web-roam--orgnote-log-buffer "*Web roam. Org Note log*"
  "The name of Org Note buffer that run in background.")

(defconst web-roam--available-commands '("publish" "publish-all" "load" "sync")
  "Available commands for Org Note.")

(defun web-roam--normalize-path (path)
  "Normalize file PATH.  Shield spaces."
  (replace-regexp-in-string " " "\  " path))

(defun web-roam--pretty-log (format-text &rest args)
  "Pretty print FORMAT-TEXT with ARGS."
  (message (concat "[web-roam.el] " format-text) args))

(defun web-roam--handle-cmd-result (process signal &optional cmd)
  "Handle result from shell stdout by PROCESS and SIGNAL.

CMD - optional external command for logging."
  (when (memq (process-status process) '(exit signal))
    (web-roam--pretty-log "Completely done.")
    (shell-command-sentinel process signal)
    (when cmd
      (with-current-buffer web-roam--orgnote-log-buffer
        (setq buffer-read-only nil)
        (goto-char (point-max))
        (insert "last command: " cmd)
        (setq buffer-read-only t)))))

(defun web-roam--execute-async-cmd (cmd)
  "Execute async CMD."
  (add-to-list 'display-buffer-alist
               `(,web-roam--orgnote-log-buffer display-buffer-no-window))

  (let* ((output-buffer (get-buffer-create web-roam--orgnote-log-buffer))
         (final-cmd (if web-roam-debug-p (concat cmd " --debug") cmd))
         (proc (progn
                 (async-shell-command cmd output-buffer output-buffer)
                 (get-buffer-process output-buffer))))
    
    (when (process-live-p proc)
      (lexical-let ((fcmd final-cmd))
        (set-process-sentinel proc (lambda (process event)
                                     (web-roam--handle-cmd-result process event fcmd)))))))

(defun web-roam--org-file-p ()
  "Return t when current FILE-NAME is org file."
  (and (buffer-file-name)
       (equal (file-name-extension (buffer-file-name)) "org")))

(defun web-roam--read-configurations (cmd)
  "Read config files for CMD to remote server.
The default config file path is ~/.config/orgnote/config.json.
With next schema:
[
  {
    \"name\": \"any alias for pretty output\",
    \"remoteAddress\": \"server address\",
    \"token\": \"token (should be generated by remote server)\"
  }
Also you are free to use array of such objects instead of single object."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (json-read-file web-roam-configuration-file-path))
         (name-to-config (make-hash-table :test 'equal))
         (server-names '()))

    (if (= (length json) 1)
        (car json)
      (dolist (conf json)
        (puthash (gethash "name" conf) conf name-to-config)
        (push (gethash "name" conf) server-names))

      (gethash (completing-read (format "Choose server for %s: " cmd) server-names) name-to-config))))

(defun web-roam--execute-command (cmd &optional args)
  "Execute command CMD via string ARGS.
CMD could be publish and publish-all"

  (unless (member cmd web-roam--available-commands)
    (error (format "[web-roam.el] Unknown command %s" cmd)))

  (unless (file-exists-p web-roam-configuration-file-path)
    (web-roam--pretty-log "Configuration file %s not found" web-roam-configuration-file-path))

  (let* ((config (web-roam--read-configurations cmd))
         (remote-address (gethash "remoteAddress" config))
         (token (gethash "token" config))
         (remote-address-cli (if remote-address (concat " --remote-address " remote-address) ""))
         (token-cli (if token (concat " --token " token) ""))
         (args (or args "")))
    (web-roam--execute-async-cmd
     (concat web-roam-execution-script
             (format " %s %s%s %s"
                     cmd
                     remote-address-cli
                     token-cli
                     args)))))

;;;###autoload
(defun web-roam-install-dependencies ()
  "Install necessary dependencies for Org Note.
Node js 14+ version is required."
  (interactive)
  (web-roam--execute-async-cmd "npm install -g orgnote-cli"))

;;;###autoload
(defun web-roam-publish-file ()
  "Publish current opened file to Org Note service."
  (interactive)
  (when (web-roam--org-file-p)
    (web-roam--execute-command "publish" (web-roam--normalize-path (buffer-file-name)))))

;;;###autoload
(defun web-roam-publish-all ()
  "Publish all files to Org Note service."
  (interactive)
  (web-roam--execute-command "publish-all"))

;;;###autoload
(defun web-roam-load ()
  "Load notes from remote."
  (interactive)
  (web-roam--execute-command "load"))

;;;###autoload
(defun web-roam-sync ()
  "Sync all files with Org Note service."
  (interactive)
  (web-roam--execute-command "sync"))

;;;###autoload
(define-minor-mode web-roam-sync-mode
  "Web roam syncing mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When `web-roam-sync-mode' is enabled, after save org mode files will
be synced with remote service."
  :init-value nil
  :global nil
  :lighter nil
  :group 'web-roam
  (if web-roam-sync-mode
      (when (web-roam--org-file-p)
        (add-hook 'before-save-hook #'web-roam-publish-file nil t))
    (remove-hook 'before-save-hook #'web-roam-publish-file t)))

(provide 'web-roam)
;;; web-roam.el ends here
