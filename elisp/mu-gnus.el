;;; mu-gnus.el --- Integration between Gnus and mail database -*- lexical-binding: t -*-

;;; Commentary:
;; This package provides integration between Gnus and the mail database system.

;;; Code:

(require 'gnus)
(require 'gnus-sum)
(require 'json)

(defgroup mu-gnus nil
  "Integration between Gnus and mail database."
  :group 'gnus)

(defcustom mu-gnus-db-command "mail-db"
  "Path to the mail-db command."
  :type 'string
  :group 'mu-gnus)

(defcustom mu-gnus-extract-command "mail-extract"
  "Path to the mail-extract command."
  :type 'string
  :group 'mu-gnus)

(defcustom mu-gnus-classify-command "python3 src/python/mail-classify.py"
  "Command to classify emails."
  :type 'string
  :group 'mu-gnus)

(defcustom mu-gnus-debug nil
  "Enable debug output."
  :type 'boolean
  :group 'mu-gnus)

(defun mu-gnus-get-message-id ()
  "Get the Message-ID of the current article in Gnus summary."
  (when (and (boundp 'gnus-summary-buffer)
             gnus-summary-buffer
             (buffer-live-p gnus-summary-buffer))
    (with-current-buffer gnus-summary-buffer
      (let ((header (gnus-summary-article-header)))
        (when header
          (mail-header-id header))))))

(defun mu-gnus-get-article-file ()
  "Save current article to temporary file and return the filename."
  (let ((temp-file (make-temp-file "gnus-article-" nil ".eml")))
    (gnus-summary-save-article-file temp-file)
    temp-file))

(defun mu-gnus-parse-db-json (json-str)
  "Parse JSON output from mail-db command."
  (condition-case err
      (let ((json-array-type 'list)
            (json-object-type 'alist))
        (json-read-from-string json-str))
    (error
     (when mu-gnus-debug
       (message "JSON parse error: %s" err))
     nil)))

(defun mu-gnus-format-reply-entry (entry)
  "Format a database entry for display in org-mode format."
  (let ((message-id (cdr (assoc 'message_id entry)))
        (from (cdr (assoc 'from_addr entry)))
        (subject (cdr (assoc 'subject entry)))
        (date (cdr (assoc 'date entry))))
    (format "* TODO Reply to: %s\n  :PROPERTIES:\n  :MESSAGE_ID: %s\n  :END:\n  From: %s\n  Date: %s\n"
            subject
            message-id
            from
            (format-time-string "%Y-%m-%d %a %H:%M" (seconds-to-time date)))))

(defun mu-gnus-add-to-db ()
  "Add current message to the mail database."
  (interactive)
  (let* ((message-id (mu-gnus-get-message-id))
         (article-file (mu-gnus-get-article-file))
         (from-cmd (format "%s --header From < %s"
                          mu-gnus-extract-command
                          (shell-quote-argument article-file)))
         (subject-cmd (format "%s --header Subject < %s"
                             mu-gnus-extract-command
                             (shell-quote-argument article-file)))
         (classify-cmd (format "%s < %s"
                              mu-gnus-classify-command
                              (shell-quote-argument article-file)))
         (from (string-trim (shell-command-to-string from-cmd)))
         (subject (string-trim (shell-command-to-string subject-cmd)))
         (classification (string-trim (shell-command-to-string classify-cmd)))
         (db-cmd (format "%s add %s --from %s --subject %s --classification %s"
                        mu-gnus-db-command
                        (shell-quote-argument message-id)
                        (shell-quote-argument from)
                        (shell-quote-argument subject)
                        (shell-quote-argument classification))))
    (when (file-exists-p article-file)
      (shell-command db-cmd)
      (delete-file article-file)
      (message "Added to database: %s" message-id))))

(defun mu-gnus-mark-replied ()
  "Mark current message as replied in the database."
  (interactive)
  (let* ((message-id (mu-gnus-get-message-id))
         (cmd (format "%s update %s --replied 1"
                     mu-gnus-db-command
                     (shell-quote-argument message-id))))
    (shell-command cmd)
    (message "Marked as replied: %s" message-id)))

(defun mu-gnus-show-needs-reply ()
  "Show messages that need replies in org-mode format."
  (interactive)
  (let* ((cmd (format "%s query --needs-reply --not-replied"
                     mu-gnus-db-command))
         (output (shell-command-to-string cmd))
         (entries (mu-gnus-parse-db-json output))
         (buffer (get-buffer-create "*Needs Reply*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "#+TITLE: Emails Needing Reply\n\n")
      (dolist (entry entries)
        (insert (mu-gnus-format-reply-entry entry)))
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

(provide 'mu-gnus)

;;; mu-gnus.el ends here
