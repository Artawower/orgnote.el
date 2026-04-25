;;; orgnote.el --- Sync org-roam notes with OrgNote app           -*- lexical-binding: t; -*-

;; Author: Artur Yaroshenko <artawower@protonmail.com>
;; URL: https://github.com/Artawower/orgnote.el
;; Package-Requires: ((emacs "29.1") (tomlparse "1.0.0") (websocket "1.15"))
;; Version: 0.14.0
;; Copyright (C) 2023 Artur Yaroshenko

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
;; This package provides functionality for syncing org-roam notes and plain org
;; files with external app - OrgNote and vice versa.
;; For more detail check https://github.com/Artawower/orgnote project.

;;; Code:

(require 'json)
(require 'tomlparse)
(require 'cl-lib)
(require 'websocket)

(defgroup orgnote nil
  "Sync org-roam notes with OrgNote app."
  :group 'org)

(defcustom orgnote-execution-script "orgnote-cli"
  "Bin command from cli to execute external script."
  :group 'orgnote
  :type 'string)

(defcustom orgnote-debug-p nil
  "Enable debug mode for better logging."
  :group 'orgnote
  :type 'boolean)

(defcustom orgnote-configuration-file-path "~/.config/orgnote/config.toml"
  "Path to configuration file for Org Note."
  :group 'orgnote
  :type 'string)

(defconst orgnote--orgnote-log-buffer "*Orgnote. Org Note log*"
  "The name of Org Note buffer that run in background.")

(defconst orgnote--available-commands '("sync" "validate-config")
  "Available commands for orgnote-cli.")

(defconst orgnote-share-command "preview note"
  "Command to execute in OrgNote for sharing content.")

(defconst orgnote-enable-roam-sync-p nil
  "Execute `org-roam-db-sync' after note received from remote server.")

(defvar orgnote-note-received-hook nil
  "Hook run after note received from remote server.")

(defvar orgnote-after-sync-hook nil
  "Hook run after autosync completes successfully.
Functions in this hook are called with no arguments.")

(defvar orgnote--autosync-process nil
  "Current autosync process or nil if none running.")

(defvar orgnote--autosync-timer nil
  "Timer used for debouncing autosync triggers.")

(defconst orgnote--config-key-name "name"
  "Config key for account name.")

(defconst orgnote--config-key-token "token"
  "Config key for API token.")

(defconst orgnote--config-key-remote "remoteAddress"
  "Config key for remote server address.")

(defconst orgnote--config-key-root "rootFolder"
  "Config key for root folder path.")

(defconst orgnote--config-key-client "clientAddress"
  "Config key for client/frontend address.")

(defconst orgnote--config-key-ws "wsAddress"
  "Config key for WebSocket address (optional, overrides derived URL).")

(defvar orgnote--ws-connections (make-hash-table :test 'equal)
  "Hash table mapping account names to connection plists.
Each entry is (:ws <connection> :config <config-hashtable>).")

(defvar orgnote--ws-reverse-map (make-hash-table :test 'eq)
  "Hash table mapping websocket objects to account names for O(1) lookup.")

(defvar orgnote--ws-reconnect-timers (make-hash-table :test 'equal)
  "Hash table mapping account names to reconnect timers.")

(defcustom orgnote-ws-reconnect-delay 5
  "Seconds to wait before reconnecting WebSocket after disconnect."
  :type 'integer
  :group 'orgnote)

(defcustom orgnote-autosync-delay 1.0
  "Seconds to wait after save before triggering autosync.
This debounce prevents multiple syncs during rapid saves."
  :type 'float
  :group 'orgnote)

(defun orgnote--pretty-log (format-text &rest args)
  "Pretty print FORMAT-TEXT with ARGS."
  (apply #'message (concat "[orgnote.el] " format-text) args))

(defconst orgnote--config-parsers
  '(("toml" . orgnote--parse-toml-config)
    ("json" . orgnote--parse-json-config))
  "Alist mapping file extensions to config parser functions.")

(defun orgnote--parse-config-file (path)
  "Parse configuration file at PATH using appropriate parser based on extension."
  (let* ((ext (orgnote--config-file-extension path))
         (parser (alist-get ext orgnote--config-parsers nil nil #'string=)))
    (unless parser
      (user-error "[orgnote.el] Unsupported configuration format: %s" ext))
    (when (and (string= ext "json")
               (file-exists-p path))
      (orgnote--pretty-log "Configuration file %s uses deprecated JSON format" path))
    (funcall parser path)))

(defun orgnote--split-execution-script ()
  "Split orgnote-execution-script into program and base arguments list.
Handles cases like 'bun run orgnote-cli' correctly."
  (if (string-match-p " " orgnote-execution-script)
      (split-string-and-unquote orgnote-execution-script)
    (list orgnote-execution-script)))

(defun orgnote--handle-cmd-result (process signal &optional args callback)
  "Handle result from shell stdout by PROCESS and SIGNAL.

ARGS - plist with :command for logging.
CALLBACK - optional callback function."
  (when (memq (process-status process) '(exit signal))
    (orgnote--pretty-log "Completely done.")
    (shell-command-sentinel process signal)
    (when callback
      (funcall callback))
    (when args
      (with-current-buffer orgnote--orgnote-log-buffer
        (setq buffer-read-only nil)
        (goto-char (point-max))
        (insert "last command: " (string-join (plist-get args :command) " "))
        (setq buffer-read-only t)))))

(defun orgnote--execute-async-cmd (program args &optional callback)
  "Execute PROGRAM with ARGS asynchronously.
CALLBACK is invoked after command completion.
Uses make-process to avoid shell injection."
  (add-to-list 'display-buffer-alist
               `(,orgnote--orgnote-log-buffer display-buffer-no-window))
  (let* ((output-buffer (get-buffer-create orgnote--orgnote-log-buffer))
         (full-args (if orgnote-debug-p
                        (append args '("--debug"))
                      args)))
    (make-process
     :name "orgnote-cmd"
     :buffer output-buffer
     :command (cons program full-args)
     :sentinel (lambda (process event)
                 (orgnote--handle-cmd-result process event
                                             (list :command (cons program full-args))
                                             callback))
     :noquery t)))

(defun orgnote--org-file-p ()
  "Return t when current FILE-NAME is org file."
  (and (buffer-file-name)
       (equal (file-name-extension (buffer-file-name)) "org")))

(defun orgnote--config-file-extension (path)
  "Return config file extension for PATH in lowercase."
  (downcase (or (file-name-extension path) "")))

(defun orgnote--config-get (key config)
  "Get KEY from CONFIG, supporting hash tables and alists."
  (let ((raw-value (cond
                    ((hash-table-p config)
                     (or (gethash key config)
                         (gethash (intern key) config)))
                    ((listp config)
                     (or (condition-case nil
                             (alist-get key config nil nil #'string=)
                           (wrong-type-argument nil))
                         (alist-get (intern key) config)))
                    (t nil))))
    (cond
     ((and (vectorp raw-value) (= (length raw-value) 1))
      (aref raw-value 0))
     ((and (listp raw-value) (= (length raw-value) 1) (stringp (car raw-value)))
      (car raw-value))
     (t raw-value))))

(defun orgnote--ensure-list (value)
  "Ensure VALUE is returned as a list."
  (cond
   ((null value) nil)
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t (list value))))

(defun orgnote--config-list-from-map (raw)
  "Return config list from RAW map (alist or hash table)."
  (let ((keys '("accounts" "account" "servers" "server" "configs" "config"
                "root" "roots")))
    (catch 'found
      (dolist (key keys)
        (let ((value (orgnote--config-get key raw)))
          (when value
            (throw 'found (orgnote--ensure-list value)))))
      nil)))

(defun orgnote--config-list-p (value)
  "Return t if VALUE look like a list of config entries."
  (catch 'found
    (dolist (elem value)
      (when elem
        (when (or (hash-table-p elem)
                  (and (listp elem) (consp (car elem))))
          (throw 'found t))))
    nil))

(defun orgnote--config-list-from-raw (raw)
  "Return config list from RAW data."
  (cond
   ((hash-table-p raw)
    (or (orgnote--config-list-from-map raw)
        (list raw)))
   ((vectorp raw) (append raw nil))
   ((listp raw)
    (let ((from-map (orgnote--config-list-from-map raw)))
      (cond
       ((orgnote--config-list-p raw) raw)
       (from-map from-map)
       (t (list raw)))))
   (t (list raw))))

(defun orgnote--parse-json-config (path)
  "Parse JSON config at PATH."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string))
    (json-read-file path)))

(defun orgnote--parse-toml-config (path)
  "Parse TOML config at PATH."
  (if (fboundp 'tomlparse-file)
      (tomlparse-file path)
    (user-error "[orgnote.el] TOML parser not available; install tomlparse and TOML tree-sitter grammar")))

(defun orgnote--read-all-configs ()
  "Return list of all configured accounts without prompting."
  (let ((raw-config (orgnote--parse-config-file orgnote-configuration-file-path)))
    (orgnote--config-list-from-raw raw-config)))

(defun orgnote--get-config-for-file (file-path)
  "Return configuration whose rootFolder contains FILE-PATH.
Returns nil if no matching configuration found."
  (seq-find (lambda (config)
               (when-let ((root (orgnote--config-get orgnote--config-key-root config)))
                 (file-in-directory-p file-path root)))
             (orgnote--read-all-configs)))

(defun orgnote--file-in-config-dir-p ()
  "Return non-nil if current buffer file is in any configured rootFolder."
  (when-let ((file (buffer-file-name)))
    (orgnote--get-config-for-file file)))

(defun orgnote--autosync-cancel-timer ()
  "Cancel pending autosync timer if any."
  (when (timerp orgnote--autosync-timer)
    (cancel-timer orgnote--autosync-timer)
    (setq orgnote--autosync-timer nil)))

(defun orgnote--autosync-cancel-process ()
  "Kill running autosync process if any."
  (when (process-live-p orgnote--autosync-process)
    (delete-process orgnote--autosync-process))
  (setq orgnote--autosync-process nil))

(defun orgnote--autosync-sentinel (process _event)
  "Handle autosync PROCESS completion."
  (when (memq (process-status process) '(exit signal))
    (setq orgnote--autosync-process nil)
    (when (eq (process-status process) 'exit)
      (run-hooks 'orgnote-after-sync-hook))))

(defun orgnote--prepare-log-buffer ()
  "Prepare log buffer for new output, preventing unbounded growth."
  (let ((buffer (get-buffer-create orgnote--orgnote-log-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    buffer))

(defun orgnote--autosync-execute (config)
  "Execute sync for CONFIG, killing any previous process.
Uses make-process to avoid shell injection vulnerabilities."
  (let ((account-name (orgnote--config-get orgnote--config-key-name config)))
    (orgnote--autosync-cancel-process)
    (let* ((log-buffer (orgnote--prepare-log-buffer))
           (script-parts (orgnote--split-execution-script))
           (program (car script-parts))
           (base-args (cdr script-parts))
           (args (append base-args (list "sync" (concat "--account=" account-name)))))
      (when orgnote-debug-p
        (setq args (append args '("--debug"))))
      (setq orgnote--autosync-process
            (make-process
             :name "orgnote-autosync"
             :buffer log-buffer
             :command (cons program args)
             :sentinel #'orgnote--autosync-sentinel
             :noquery t)))))

(defun orgnote--autosync-trigger ()
  "Trigger debounced autosync for current buffer's account."
  (when-let* ((file (buffer-file-name))
              (config (orgnote--get-config-for-file file)))
    (orgnote--autosync-cancel-timer)
    (setq orgnote--autosync-timer
          (run-with-idle-timer orgnote-autosync-delay nil
                               #'orgnote--autosync-execute config))))

(defun orgnote--get-config-with-context (&optional context)
  "Get config automatically based on current buffer file or prompt.
If current buffer file matches a rootFolder, use that config.
Otherwise, if single config exists, use it.
CONTEXT is used for prompting if needed."
  (or (orgnote--file-in-config-dir-p)
      (let ((configs (orgnote--read-all-configs)))
        (if (= (length configs) 1)
            (car configs)
          (orgnote--prompt-for-config (or context "operation"))))))

(defun orgnote--prompt-for-config (context)
  "Prompt user to select a config for CONTEXT."
  (let* ((configs (orgnote--read-all-configs))
         (name-to-config (make-hash-table :test 'equal))
         (server-names '()))
    (dolist (conf configs)
      (let ((name (orgnote--config-get orgnote--config-key-name conf)))
        (when name
          (puthash name conf name-to-config)
          (push name server-names))))
    (gethash (completing-read (format "Choose account for %s: " context)
                              (nreverse server-names))
             name-to-config)))

(defun orgnote--execute-sync-command (&optional args callback)
  "Execute sync command with optional ARGS.
Auto-selects config based on current buffer file path.
CALLBACK is invoked after command completion."
  (unless (file-exists-p orgnote-configuration-file-path)
    (user-error "[orgnote.el] Configuration file %s not found" orgnote-configuration-file-path))
  (let* ((config (orgnote--get-config-with-context "sync"))
         (account-name (orgnote--config-get orgnote--config-key-name config))
         (script-parts (orgnote--split-execution-script))
         (program (car script-parts))
         (base-args (cdr script-parts))
         (normalized-args (if (listp args) args (when args (list args))))
         (full-args (append base-args (list "sync" (concat "--account=" account-name)) normalized-args)))
    (orgnote--execute-async-cmd program full-args callback)))

(defun orgnote--after-receive-notes ()
  "Run hook after sync completes."
  (when (and orgnote-enable-roam-sync-p (fboundp 'org-roam-db-sync))
    (org-roam-db-sync))
  (run-hooks 'orgnote-note-received-hook))

;;;###autoload
(defun orgnote-install-dependencies ()
  "Install orgnote-cli globally using npm."
  (interactive)
  (orgnote--execute-async-cmd "npm" '("install" "-g" "orgnote-cli")))

;;;###autoload
(defun orgnote-sync ()
  "Sync all files with Org Note service.
Auto-selects account based on current buffer file path."
  (interactive)
  (orgnote--execute-sync-command nil #'orgnote--after-receive-notes))

;;;###autoload
(defun orgnote-force-sync ()
  "Force sync all files with Org Note service.
Clears cache before sync."
  (interactive)
  (orgnote--execute-sync-command "--force" #'orgnote--after-receive-notes))

;;;###autoload
(defun orgnote-open-configuration ()
  "Edit configuration file for Org Note."
  (interactive)
  (find-file orgnote-configuration-file-path))

;;;###autoload
(defun orgnote-open-cache-store ()
  "Open cache store for Org Note.
Auto-selects account based on current buffer file path."
  (interactive)
  (let* ((config (orgnote--get-config-with-context "reading cache"))
         (account-name (orgnote--config-get orgnote--config-key-name config)))
    (find-file (format "~/.config/orgnote/store-%s.json" account-name))))

;;;###autoload
(defun orgnote-open-log-buffer ()
  "Open OrgNote log buffer."
  (interactive)
  (switch-to-buffer orgnote--orgnote-log-buffer))

(defun orgnote--get-orgnote-url ()
  "Get the OrgNote frontend URL from the configuration.
Auto-selects account based on current buffer file path."
  (let* ((config (orgnote--get-config-with-context "preview"))
         (client-address (orgnote--config-get orgnote--config-key-client config)))
    (if client-address
        client-address
      (user-error "[orgnote.el] clientAddress must be configured in config file"))))

(defun orgnote--build-preview-url (content)
  "Build the preview URL for CONTENT."
  (let* ((base-url (orgnote--get-orgnote-url))
         (payload `((command . ,orgnote-share-command)
                    (data . ((text . ,content)))))
         (json (json-encode payload))
         (encoded (url-hexify-string json)))
    (format "%s/panes?execute=%s" base-url encoded)))

;;;###autoload
(defun orgnote-share-current-buffer ()
  "Share current Org buffer with OrgNote application.
Opens the buffer content in OrgNote using the preview note command."
  (interactive)
  (unless (orgnote--org-file-p)
    (user-error "Not in an Org buffer"))
  (condition-case err
      (let ((url (orgnote--build-preview-url
                  (buffer-substring-no-properties (point-min) (point-max)))))
        (browse-url url)
        (orgnote--pretty-log "Sharing buffer with OrgNote: %s" url))
    (error (orgnote--pretty-log "Failed to share buffer: %s" (error-message-string err)))))

;;;###autoload
(defun orgnote-share-region ()
  "Share selected region with OrgNote application."
  (interactive)
  (unless (use-region-p)
    (user-error "No region selected"))
  (condition-case err
      (let ((url (orgnote--build-preview-url
                  (buffer-substring-no-properties (region-beginning) (region-end)))))
        (browse-url url)
        (orgnote--pretty-log "Sharing region with OrgNote"))
    (error (orgnote--pretty-log "Failed to share region: %s" (error-message-string err)))))

;;;###autoload
(define-minor-mode orgnote-autosync-mode
  "Automatically sync org files with OrgNote on save.

When enabled for a buffer, triggers sync after each save if the
file is located within a configured rootFolder. Uses debouncing
to prevent excessive sync operations during rapid saves.

The hook `orgnote-after-sync-hook' runs after each successful sync."
  :init-value nil
  :local t
  :lighter " OrgNote"
  :group 'orgnote
  (if orgnote-autosync-mode
      (add-hook 'after-save-hook #'orgnote--autosync-trigger nil t)
    (remove-hook 'after-save-hook #'orgnote--autosync-trigger t)
    (orgnote--autosync-cancel-timer)))

(defun orgnote-autosync--maybe-enable ()
  "Enable `orgnote-autosync-mode' if current buffer qualifies."
  (when (and (orgnote--org-file-p)
             (orgnote--file-in-config-dir-p))
    (orgnote-autosync-mode 1)))

(defun orgnote--ws-build-url (config)
  "Build WebSocket URL for CONFIG.
Uses wsAddress if set, otherwise derives from remoteAddress."
  (let* ((ws-address (orgnote--config-get orgnote--config-key-ws config))
         (remote (orgnote--config-get orgnote--config-key-remote config))
         (token (orgnote--config-get orgnote--config-key-token config)))
    (unless token
      (user-error "[orgnote.el] token not configured"))
    (if ws-address
        (format "%s?token=%s" (replace-regexp-in-string "^http" "ws" ws-address) token)
      (unless remote
        (user-error "[orgnote.el] remoteAddress not configured"))
      (let* ((ws-scheme (replace-regexp-in-string "^http" "ws" remote))
             (host (if (string-match "\\`wss?://\\([^/]+\\)" ws-scheme)
                       (match-string 1 ws-scheme)
                     ws-scheme)))
        (format "wss://%s/ws/events?token=%s" host token)))))

(defun orgnote--ws-parse-payload (frame)
  "Parse JSON payload from FRAME, returning type or nil."
  (condition-case err
      (let ((payload (json-parse-string (websocket-frame-payload frame)
                                        :object-type 'hash-table)))
        (when (hash-table-p payload)
          (let ((type (gethash "type" payload)))
            (when (and type (stringp type))
              type))))
    (error nil)))

(defun orgnote--ws-on-message (ws _frame)
  "Handle WebSocket message for WS connection."
  (let ((type (orgnote--ws-parse-payload _frame)))
    (when (and type (string= type "sync"))
      (when-let* ((account-name (orgnote--ws-get-account ws))
                  (entry (gethash account-name orgnote--ws-connections))
                  (config (plist-get entry :config)))
        (orgnote--pretty-log "Received sync event for account: %s" account-name)
        (orgnote--autosync-execute config)))))

(defun orgnote--ws-on-close (ws)
  "Handle WebSocket close for WS."
  (when-let* ((account-name (orgnote--ws-get-account ws))
              (entry (gethash account-name orgnote--ws-connections))
              (config (plist-get entry :config)))
    (remhash account-name orgnote--ws-connections)
    (remhash ws orgnote--ws-reverse-map)
    (orgnote--pretty-log "WebSocket closed for account: %s" account-name)
    (when orgnote-autosync-global-mode
      (orgnote--ws-schedule-reconnect config))))

(defun orgnote--ws-get-account (ws)
  "Get account name for WebSocket WS from reverse map."
  (gethash ws orgnote--ws-reverse-map))

(defun orgnote--ws-on-error (_ws action err)
  "Handle WebSocket ERR for ACTION."
  (let ((err-msg (cond
                  ((stringp err) err)
                  (t (format "%s" err)))))
    (orgnote--pretty-log "WebSocket error during %s: %s" action err-msg)))

(defun orgnote--ws-open-p (account-name)
  "Return non-nil if WebSocket for ACCOUNT-NAME is open."
  (when-let ((entry (gethash account-name orgnote--ws-connections)))
    (when-let ((ws (plist-get entry :ws)))
      (websocket-openp ws))))

(defun orgnote--ws-register (account-name ws config)
  "Register WebSocket WS for ACCOUNT-NAME with cached CONFIG."
  (puthash account-name (list :ws ws :config config) orgnote--ws-connections)
  (puthash ws account-name orgnote--ws-reverse-map))

(cl-defun orgnote--ws-connect (config)
  "Connect to WebSocket for CONFIG.
Returns the WebSocket connection or nil on failure."
  (let ((account-name (orgnote--config-get orgnote--config-key-name config)))
    (when (orgnote--ws-open-p account-name)
      (cl-return-from orgnote--ws-connect
        (plist-get (gethash account-name orgnote--ws-connections) :ws)))
    (condition-case err
        (let* ((ws-url (orgnote--ws-build-url config))
               (ws (websocket-open
                    ws-url
                    :on-message #'orgnote--ws-on-message
                    :on-close #'orgnote--ws-on-close
                    :on-error #'orgnote--ws-on-error)))
          (orgnote--ws-register account-name ws config)
          (orgnote--pretty-log "WebSocket connected for account: %s" account-name)
          ws)
      (error
       (orgnote--pretty-log "WebSocket connection failed: %s" (error-message-string err))
       nil))))

(defun orgnote--ws-cancel-reconnect-timer (account-name)
  "Cancel reconnect timer for ACCOUNT-NAME if exists."
  (when-let ((timer (gethash account-name orgnote--ws-reconnect-timers)))
    (cancel-timer timer)
    (remhash account-name orgnote--ws-reconnect-timers)))

(defun orgnote--ws-disconnect (account-name)
  "Disconnect WebSocket for ACCOUNT-NAME."
  (orgnote--ws-cancel-reconnect-timer account-name)
  (when-let ((entry (gethash account-name orgnote--ws-connections)))
    (let ((ws (plist-get entry :ws)))
      (when (and ws (websocket-openp ws))
        (websocket-close ws))
      (remhash ws orgnote--ws-reverse-map)))
  (remhash account-name orgnote--ws-connections))

(defun orgnote--ws-disconnect-all ()
  "Disconnect all WebSocket connections."
  (let ((accounts (hash-table-keys orgnote--ws-connections)))
    (dolist (account-name accounts)
      (orgnote--ws-disconnect account-name))))

(defun orgnote--ws-schedule-reconnect (config)
  "Schedule WebSocket reconnect for CONFIG."
  (let ((account-name (orgnote--config-get orgnote--config-key-name config)))
    (orgnote--ws-cancel-reconnect-timer account-name)
    (puthash account-name
             (run-with-timer orgnote-ws-reconnect-delay nil
                             #'orgnote--ws-reconnect config)
             orgnote--ws-reconnect-timers)))

(defun orgnote--ws-reconnect (config)
  "Attempt to reconnect WebSocket for CONFIG."
  (let ((account-name (orgnote--config-get orgnote--config-key-name config)))
    (orgnote--ws-cancel-reconnect-timer account-name)
    (when orgnote-autosync-global-mode
      (orgnote--ws-connect config))))

(defun orgnote--ws-connect-all ()
  "Connect WebSocket for all configured accounts."
  (dolist (config (orgnote--read-all-configs))
    (orgnote--ws-connect config)))

;;;###autoload
(define-globalized-minor-mode orgnote-autosync-global-mode
  orgnote-autosync-mode
  orgnote-autosync--maybe-enable
  :group 'orgnote
  (when orgnote-autosync-global-mode
    (orgnote--ws-connect-all))
  (unless orgnote-autosync-global-mode
    (orgnote--ws-disconnect-all)))

(provide 'orgnote)
;;; orgnote.el ends here
