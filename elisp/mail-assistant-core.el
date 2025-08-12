;;; mail-assistant-core.el --- Core mail assistant functions for Gnus integration

(require 'gnus)
(require 'gnus-sum)
(require 'gnus-art)
(require 'json)

(defun mail-assistant-get-config-value (key)
  "Get a configuration value from config.ini using mail-analyze."
  (let ((output (shell-command-to-string 
                 (format "mail-analyze --get-config %s 2>/dev/null" key))))
    (string-trim output)))

(defun mail-assistant-get-org-file ()
  "Get the org file path from config or use default."
  (let ((org-file (mail-assistant-get-config-value "org_file")))
    (if (string-empty-p org-file)
        (expand-file-name "~/org/mail-tasks.org")
      (expand-file-name org-file))))

(defun mail-assistant-process-article ()
  "Process current Gnus article and extract action items."
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
        (let ((action-items (cdr (assoc 'actions actions)))
              (org-file (mail-assistant-get-org-file)))
          (when action-items
            (mail-assistant-append-to-org 
             org-file message-id group from subject date action-items)))))))

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
    (insert (format "[[elisp:(mail-assistant-reply-all \"%s\")][Reply All to this email]]\n" 
                    message-id))
    (save-buffer)))

(defun mail-assistant-reply-all (message-id)
  "Switch to Gnus and open reply-all for the given message-id."
  (interactive "sMessage-ID: ")
  ;; Switch to Gnus
  (gnus)
  ;; Try to find the article
  (when (and gnus-newsgroup-name
             (gnus-summary-goto-article message-id nil t))
    ;; Open reply-all with original
    (gnus-summary-wide-reply-with-original 1)
    ;; Move point to message body (after headers)
    (message-goto-body)))

;; Add to gnus article prepare hook
(add-hook 'gnus-article-prepare-hook 'mail-assistant-process-article)

(provide 'mail-assistant-core)

;;; mail-assistant-core.el ends here
