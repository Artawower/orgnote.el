;;; blamer.el --- Show git blame info about current line           -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Artur Yaroshenko

;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/Artawower/web-roam.el
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.0.1

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
;; This package provides functionality for syncing org roam notes with external service - second brain
;;; Code:

(defcustom web-roam-execution-script "second-brain-publisher"
  "Bin command from cli to execute external script."
  :group 'web-roam
  :type 'string)

(defconst web-roam--async-buffer-name "*Second Brain Async Command*"
  "The name of second brain buffer that run in background.")

(defun web-roam--normalize-path (path)
  "Normalize file PATH. Shield spaces."
  (replace-regexp-in-string " " "\\\\\  " path))

(defun web-roam--handle-cmd-result (process signal)
  "Handle result from shell stdout by PROCESS and SIGNAL."
  (when (memq (process-status process) '(exit signal))
    (message "Completely done.")
    (shell-command-sentinel process signal)))

(defun web-roam--execute-async-cmd (cmd)
  "Execute async CMD."
  (add-to-list 'display-buffer-alist
  '("\\*Second Brain Async Command\\*.*" display-buffer-no-window))

  (let* ((output-buffer (generate-new-buffer web-roam--async-buffer-name))
         (proc (progn
                 (async-shell-command cmd output-buffer)
                 (get-buffer-process output-buffer))))
    (when (process-live-p proc)
      (set-process-sentinel proc 'web-roam--handle-cmd-result))))

;;;###autoload
(defun web-roam-install-dependencies ()
  "Install necessary dependencies for second brain.
Node js 14+ version is required."
  (interactive)
  (web-roam--execute-async-cmd "npm install -g second-brain-publisher"))

;;;###autoload
(defun web-roam-publish-file ()
  "Publish current opened file to second brain service."
  (interactive)
  (when (web-roam--org-file-p)
    (web-roam--execute-async-cmd
     (format "second-brain-publisher publish %s"
             (web-roam--normalize-path (buffer-file-name))))))

;;;###autoload
(defun web-roam-publish-all ()
  "Publish all files to second brain service."
  (interactive)
  (web-roam--execute-async-cmd
   "second-brain-publisher publish-all"))

(defun web-roam--org-file-p ()
  "Return t when current FILE-NAME is org file."
  (and (buffer-file-name)
       (equal (file-name-extension (buffer-file-name)) "org")))

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
  (if (web-roam--org-file-p)
      (message "Org file")
    (message "Current file is not a org file")))

(provide 'web-roam)
;;; web-roam.el ends here
