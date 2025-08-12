;;; mail-assistant-core.el --- Core mail assistant functions for Gnus integration

(require 'gnus)
(require 'gnus-sum)
(require 'gnus-art)
(require 'json)
(require 'org)

(defvar mail-assistant-enable-auto-process t
  "Whether to automatically process articles when viewing them.")

(defvar mail-assistant-org-file "~/org/mail-tasks.org"
  "Path to the org file for mail tasks.")

(defun mail-assistant-get-config-value (key)
  "Get a configuration value from config.ini using mail-analyze."
  (let ((output (shell-command-to-string 
                 (format "mail-analyze --get-config %s 2>/dev/null" key))))
    (string-trim output)))

(defun mail-assistant-get-org-file ()
  "Get the org file path from config or use default."
  (or mail-assistant-org-file
      (let ((org-file (mail-assistant-get-config-value "org_file")))
        (if (string-empty-p org-file)
            (expand-file-name "~/org/mail-tasks.org")
          (expand-file-name org-file)))))

(defun mail-assistant-message-already-processed-p (message-id org-file)
  "Check if MESSAGE-ID already exists in ORG-FILE."
  (when (file-exists-p org-file)
    (with-temp-buffer
      (insert-file-contents org-file)
      (goto-char (point-min))
      (search-forward (concat ":MESSAGE_ID: " message-id) nil t))))

(defun mail-assistant-store-gnus-location ()
  "Store current Gnus group and article number for later access."
  (when (and gnus-newsgroup-name gnus-current-article)
    (list gnus-newsgroup-name gnus-current-article)))

(defun mail-assistant-goto-message-by-id (message-id)
  "Navigate to a specific message in Gnus using MESSAGE-ID.
First tries to get GNUS_GROUP from org properties, then opens the message.
Returns t if successful, nil if not found."
  (let ((group (org-entry-get nil "GNUS_GROUP")))
    (when group
      ;; Open Gnus if not already open
      (gnus)
      ;; Open the group
      (gnus-group-read-group nil t group)
      ;; Try to find the message
      (if (gnus-summary-goto-article message-id nil t)
          (progn
            ;; Show the article
            (gnus-summary-show-article)
            t)
        ;; If not found, try refer-article
        (if (gnus-summary-refer-article message-id)
            (progn
              (gnus-summary-show-article)
              t)
          nil)))))

(defun mail-assistant-process-article ()
  "Process current Gnus article and extract action items."
  (when (and mail-assistant-enable-auto-process
             gnus-article-buffer
             (get-buffer gnus-article-buffer))
    (let* ((message-id (mail-header-id (gnus-summary-article-header)))
           (from (mail-header-from (gnus-summary-article-header)))
           (subject (mail-header-subject (gnus-summary-article-header)))
           (date (mail-header-date (gnus-summary-article-header)))
           (group gnus-newsgroup-name)
           (org-file (mail-assistant-get-org-file)))
      
      ;; Skip if already processed
      (unless (mail-assistant-message-already-processed-p message-id org-file)
        (let* ((raw-article (with-current-buffer gnus-article-buffer
                              (buffer-substring-no-properties (point-min) (point-max))))
               (temp-file (make-temp-file "mail-assistant-" nil ".txt"))
               json-response actions)
          
          ;; Store Gnus location
          (mail-assistant-store-gnus-location)
          
          ;; Write article to temp file
          (with-temp-file temp-file
            (insert raw-article))
          
          ;; Call mail-analyze to extract actions
          (setq json-response 
                (shell-command-to-string 
                 (format "mail-analyze --extract-actions < %s" temp-file)))
          
          ;; Clean up temp file
          (delete-file temp-file)
          
          ;; Parse JSON response
          (setq actions (condition-case nil
                            (let ((json-object-type 'alist)
                                  (json-array-type 'list))
                              (json-read-from-string json-response))
                          (error nil)))
          
          ;; Append to org file if we got actions
          (when actions
            (let ((action-items (cdr (assoc 'actions actions))))
              (when action-items
                (mail-assistant-append-to-org 
                 org-file message-id group from subject date action-items)))))))))

(defun mail-assistant-append-to-org (org-file message-id group from subject date action-items)
  "Append task entry to org file."
  (with-current-buffer (find-file-noselect org-file t)
    (goto-char (point-max))
    (insert "\n* TODO " subject "\n")
    (insert ":PROPERTIES:\n")
    (insert ":MESSAGE_ID: " message-id "\n")
    (insert ":GNUS_GROUP: " group "\n")
    (insert ":FROM: " from "\n")
    (insert ":DATE: " date "\n")
    (insert ":END:\n\n")
    (insert "** Action Items\n")
    (dolist (action action-items)
      (insert "- [ ] " action "\n"))
    (insert "\n** My Response\n")
    (insert "#+BEGIN_SRC text\n")
    (insert "(Fill in your response here)\n")
    (insert "#+END_SRC\n\n")
    (insert (format "[[elisp:(mail-assistant-open-original \"%s\")][Open Original Email]]\n" 
                    message-id))
    (insert (format "[[elisp:(mail-assistant-reply-all \"%s\")][Reply All to this email]]\n" 
                    message-id))
    (save-buffer)))

(defun mail-assistant-open-original (message-id)
  "Open original email in Gnus using MESSAGE-ID."
  (interactive "sMessage-ID: ")
  (if (mail-assistant-goto-message-by-id message-id)
      (progn
        (switch-to-buffer (get-buffer "*Summary*"))
        (message "Original email opened in Gnus"))
    (message "Could not find message with ID: %s" message-id)))

(defun mail-assistant-open-original-at-point ()
  "Open original email from org entry at point."
  (interactive)
  (let ((message-id (mail-assistant-get-message-id-from-org)))
    (if message-id
        (if (mail-assistant-goto-message-by-id message-id)
            (progn
              (switch-to-buffer (get-buffer "*Summary*"))
              (message "Original email opened in Gnus"))
          (message "Could not find message with ID: %s" message-id))
      (message "No MESSAGE_ID found at current org entry"))))

(defun mail-assistant-reply-all (message-id)
  "Open reply-all for the given message-id."
  (interactive "sMessage-ID: ")
  ;; First open the original message
  (if (mail-assistant-goto-message-by-id message-id)
      (progn
        ;; Open reply-all with original
        (gnus-summary-wide-reply-with-original 1)
        ;; Move point to message body (after headers)
        (message-goto-body))
    (message "Could not find message with ID: %s" message-id)))

(defun mail-assistant-process-article-interactive ()
  "Manually process current article even if already processed."
  (interactive)
  (let ((mail-assistant-enable-auto-process t))
    (when (and gnus-article-buffer
               (get-buffer gnus-article-buffer))
      (let* ((message-id (mail-header-id (gnus-summary-article-header)))
             (from (mail-header-from (gnus-summary-article-header)))
             (subject (mail-header-subject (gnus-summary-article-header)))
             (date (mail-header-date (gnus-summary-article-header)))
             (group gnus-newsgroup-name)
             (raw-article (with-current-buffer gnus-article-buffer
                            (buffer-substring-no-properties (point-min) (point-max))))
             (temp-file (make-temp-file "mail-assistant-" nil ".txt"))
             json-response actions)
        
        ;; Store Gnus location
        (mail-assistant-store-gnus-location)
        
        ;; Write article to temp file
        (with-temp-file temp-file
          (insert raw-article))
        
        ;; Call mail-analyze to extract actions
        (setq json-response 
              (shell-command-to-string 
               (format "mail-analyze --extract-actions < %s" temp-file)))
        
        ;; Clean up temp file
        (delete-file temp-file)
        
        ;; Parse JSON response
        (setq actions (condition-case nil
                          (let ((json-object-type 'alist)
                                (json-array-type 'list))
                            (json-read-from-string json-response))
                        (error nil)))
        
        ;; Append to org file if we got actions
        (if actions
            (let ((action-items (cdr (assoc 'actions actions)))
                  (org-file (mail-assistant-get-org-file)))
              (if action-items
                  (progn
                    (mail-assistant-append-to-org 
                     org-file message-id group from subject date action-items)
                    (message "Email processed and added to %s" org-file))
                (message "No action items found in this email")))
          (message "Failed to process email"))))))

(defun mail-assistant-get-message-id-from-org ()
  "Get MESSAGE_ID property from current org heading."
  (org-entry-get nil "MESSAGE_ID"))

(defun mail-assistant-extract-response-from-org ()
  "Extract text between #+BEGIN_SRC and #+END_SRC under My Response."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t t) (point)))
          response-text)
      (when (re-search-forward "^\\*\\* My Response" end t)
        (when (re-search-forward "^#\\+BEGIN_SRC text" end t)
          (forward-line)
          (let ((src-start (point)))
            (when (re-search-forward "^#\\+END_SRC" end t)
              (forward-line -1)
              (end-of-line)
              (setq response-text (buffer-substring-no-properties src-start (point)))))))
      response-text)))

(defun mail-assistant-get-original-email (message-id)
  "Get original email from Gnus using message-id."
  (let ((original-buffer (current-buffer))
        (group (org-entry-get nil "GNUS_GROUP"))
        email-content)
    (save-window-excursion
      (if group
          ;; Use group from org properties
          (progn
            (gnus)
            (gnus-group-read-group nil t group)
            (when (gnus-summary-goto-article message-id nil t)
              (gnus-summary-select-article)
              (with-current-buffer gnus-article-buffer
                (setq email-content (buffer-substring-no-properties (point-min) (point-max))))))
        ;; Fallback to searching
        (gnus)
        (when (and gnus-newsgroup-name
                   (gnus-summary-goto-article message-id nil t))
          (gnus-summary-select-article)
          (with-current-buffer gnus-article-buffer
            (setq email-content (buffer-substring-no-properties (point-min) (point-max)))))))
    (switch-to-buffer original-buffer)
    email-content))

(defun mail-assistant-draft-reply-from-org ()
  "Generate draft reply using LLM based on org entry."
  (let ((message-id (mail-assistant-get-message-id-from-org))
        (response-text (mail-assistant-extract-response-from-org))
        original-email draft-output)
    (when (and message-id response-text)
      ;; Get original email
      (setq original-email (mail-assistant-get-original-email message-id))
      (when original-email
        ;; Save to temp files
        (let ((email-file (make-temp-file "mail-original-" nil ".txt"))
              (response-file (make-temp-file "mail-response-" nil ".txt")))
          (with-temp-file email-file
            (insert original-email))
          (with-temp-file response-file
            (insert response-text))
          ;; Call mail-draft-reply
          (setq draft-output
                (shell-command-to-string
                 (format "mail-draft-reply --original %s --response %s"
                         email-file response-file)))
          ;; Clean up temp files
          (delete-file email-file)
          (delete-file response-file)
          draft-output)))))

(defun mail-assistant-insert-draft-and-reply ()
  "Generate draft reply and insert into Gnus reply window."
  (interactive)
  (let ((draft (mail-assistant-draft-reply-from-org))
        (message-id (mail-assistant-get-message-id-from-org)))
    (if (and draft message-id)
        (progn
          ;; Open reply window
          (mail-assistant-reply-all message-id)
          ;; Insert draft
          (insert draft)
          (message "Draft inserted, review and send"))
      (message "Could not generate draft - check MESSAGE_ID and response text"))))

(defun mail-assistant-toggle-auto-process ()
  "Toggle automatic processing of articles."
  (interactive)
  (setq mail-assistant-enable-auto-process (not mail-assistant-enable-auto-process))
  (message "Mail assistant auto-process %s" 
           (if mail-assistant-enable-auto-process "enabled" "disabled")))

;; Add to gnus article prepare hook
(add-hook 'gnus-article-prepare-hook 'mail-assistant-process-article)

;; Bind keys in gnus summary mode
(with-eval-after-load 'gnus-sum
  (define-key gnus-summary-mode-map (kbd "C-c m p") 'mail-assistant-process-article-interactive)
  (define-key gnus-summary-mode-map (kbd "C-c m t") 'mail-assistant-toggle-auto-process))

;; Bind keys in org-mode
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c m o") 'mail-assistant-open-original-at-point)
  (define-key org-mode-map (kbd "C-c m r") 'mail-assistant-insert-draft-and-reply))

(provide 'mail-assistant-core)

;;; mail-assistant-core.el ends here
