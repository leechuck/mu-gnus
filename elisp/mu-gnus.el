;;; mu-gnus.el --- Gnus integration for mail processing tools -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Author: Mail Assistant
;; Keywords: mail, gnus, org
;; Version: 0.1.0

;;; Commentary:

;; This package provides Gnus integration for mail classification,
;; org-mode conversion, and reply tracking using external mail processing tools.

;;; Code:

(require 'gnus)
(require 'gnus-sum)
(require 'gnus-art)
(require 'org-capture)
(require 'json)

(defgroup mu-gnus nil
  "Gnus integration for mail processing tools."
  :group 'gnus
  :prefix "mu-gnus-")

(defcustom mu-gnus-classify-command "python3 src/python/mail-classify.py"
  "Path to the mail-classify.py script."
  :type 'string
  :group 'mu-gnus)

(defcustom mu-gnus-to-org-command "python3 src/python/mail-to-org.py"
  "Path to the mail-to-org.py script."
  :type 'string
  :group 'mu-gnus)

(defcustom mu-gnus-db-command "bin/mail-db"
  "Path to the mail-db executable."
  :type 'string
  :group 'mu-gnus)

(defcustom mu-gnus-extract-command "bin/mail-extract"
  "Path to the mail-extract executable."
  :type 'string
  :group 'mu-gnus)

(defcustom mu-gnus-org-capture-key "m"
  "Org-capture template key to use for email capture."
  :type 'string
  :group 'mu-gnus)

(defcustom mu-gnus-debug nil
  "Enable debug output."
  :type 'boolean
  :group 'mu-gnus)

(defun mu-gnus-get-article-buffer ()
  "Get the current article buffer content as string."
  (with-current-buffer gnus-article-buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun mu-gnus-get-message-id ()
  "Get the Message-ID of the current article."
  (with-current-buffer gnus-summary-buffer
    (let ((header (gnus-summary-article-header)))
      (when header
        (mail-header-id header)))))

(defun mu-gnus-get-article-file ()
  "Save current article to temp file and return the filename."
  (let ((temp-file (make-temp-file "mu-gnus-article-" nil ".eml")))
    (with-current-buffer gnus-article-buffer
      (write-region (point-min) (point-max) temp-file nil 'silent))
    temp-file))

(defun mu-gnus-classify ()
  "Classify the current email and add X-Label header."
  (interactive)
  (unless (gnus-summary-article-number)
    (error "No article selected"))
  
  (let* ((temp-file (mu-gnus-get-article-file))
         (cmd (format "%s %s" mu-gnus-classify-command temp-file))
         (classification nil))
    
    (when mu-gnus-debug
      (message "Running: %s" cmd))
    
    (unwind-protect
        (progn
          (setq classification (string-trim (shell-command-to-string cmd)))
          
          (when mu-gnus-debug
            (message "Classification result: %s" classification))
          
          ;; Validate classification
          (unless (member classification '("important" "newsletter" "social" "automated"))
            (error "Invalid classification: %s" classification))
          
          ;; Add X-Label header to the article
          (gnus-summary-edit-article)
          (goto-char (point-min))
          (if (re-search-forward "^X-Label:" nil t)
              (progn
                (beginning-of-line)
                (kill-line 1))
            (re-search-forward "^$" nil t)
            (beginning-of-line))
          (insert (format "X-Label: %s\n" classification))
          (gnus-summary-edit-article-done)
          
          (message "Article classified as: %s" classification))
      
      ;; Clean up temp file
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(defun mu-gnus-to-org ()
  "Convert current email to org format and capture it."
  (interactive)
  (unless (gnus-summary-article-number)
    (error "No article selected"))
  
  (let* ((temp-file (mu-gnus-get-article-file))
         (cmd (format "%s --scheduled < %s" mu-gnus-to-org-command temp-file))
         (org-content nil))
    
    (when mu-gnus-debug
      (message "Running: %s" cmd))
    
    (unwind-protect
        (progn
          (setq org-content (shell-command-to-string cmd))
          
          (when mu-gnus-debug
            (message "Org content length: %d" (length org-content)))
          
          ;; Check if org-capture template exists
          (unless (assoc mu-gnus-org-capture-key org-capture-templates)
            ;; Create a default template if it doesn't exist
            (add-to-list 'org-capture-templates
                         `(,mu-gnus-org-capture-key
                           "Email"
                           entry
                           (file+headline org-default-notes-file "Emails")
                           "%i"
                           :empty-lines 1)))
          
          ;; Capture the org content
          (let ((org-capture-initial org-content))
            (org-capture nil mu-gnus-org-capture-key))
          
          (message "Email captured to org"))
      
      ;; Clean up temp file
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(defun mu-gnus-parse-db-json (json-str)
  "Parse JSON output from mail-db query."
  (condition-case err
      (let ((json-array-type 'list)
            (json-object-type 'alist))
        (json-read-from-string json-str))
    (error
     (when mu-gnus-debug
       (message "JSON parse error: %s" err))
     nil)))

(defun mu-gnus-format-reply-entry (entry)
  "Format a single reply entry for display."
  (let ((message-id (cdr (assoc 'message_id entry)))
        (from (cdr (assoc 'from_addr entry)))
        (subject (cdr (assoc 'subject entry)))
        (date (cdr (assoc 'date entry))))
    (format "* TODO Reply to: %s\n  From: %s\n  Date: %s\n  Message-ID: %s\n"
            (or subject "No subject")
            (or from "Unknown")
            (if date (format-time-string "%Y-%m-%d %H:%M" date) "Unknown")
            (or message-id "Unknown"))))

(defun mu-gnus-check-replies ()
  "Query mail-db for messages needing replies and show in buffer."
  (interactive)
  (let* ((cmd (format "%s query --needs-reply --not-replied" mu-gnus-db-command))
         (output (shell-command-to-string cmd))
         (buffer-name "*Mu-Gnus Pending Replies*"))
    
    (when mu-gnus-debug
      (message "Running: %s" cmd)
      (message "Output: %s" output))
    
    ;; Parse JSON output
    (let ((entries (mu-gnus-parse-db-json output)))
      
      (with-current-buffer (get-buffer-create buffer-name)
        (erase-buffer)
        (org-mode)
        (insert "#+TITLE: Emails Needing Reply\n")
        (insert (format "#+DATE: %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))
        
        (if entries
            (progn
              (insert (format "Found %d email(s) needing reply:\n\n" (length entries)))
              (dolist (entry entries)
                (insert (mu-gnus-format-reply-entry entry))
                (insert "\n")))
          (insert "No emails needing reply.\n"))
        
        (goto-char (point-min))
        (display-buffer (current-buffer))
        (message "Found %d email(s) needing reply" (length entries))))))

(defun mu-gnus-add-to-db ()
  "Add current email to the mail database."
  (interactive)
  (unless (gnus-summary-article-number)
    (error "No article selected"))
  
  (let* ((temp-file (mu-gnus-get-article-file))
         (message-id (mu-gnus-get-message-id))
         extract-cmd
         from-addr
         subject
         classification
         add-cmd)
    
    (unwind-protect
        (progn
          ;; Extract from address
          (setq extract-cmd (format "%s --header From < %s" mu-gnus-extract-command temp-file))
          (setq from-addr (string-trim (shell-command-to-string extract-cmd)))
          
          ;; Extract subject
          (setq extract-cmd (format "%s --header Subject < %s" mu-gnus-extract-command temp-file))
          (setq subject (string-trim (shell-command-to-string extract-cmd)))
          
          ;; Get classification
          (setq classification (string-trim (shell-command-to-string 
                                            (format "%s %s" mu-gnus-classify-command temp-file))))
          
          ;; Add to database
          (setq add-cmd (format "%s add %s --from %s --subject %s --classification %s"
                               mu-gnus-db-command
                               (shell-quote-argument message-id)
                               (shell-quote-argument from-addr)
                               (shell-quote-argument subject)
                               (shell-quote-argument classification)))
          
          (when mu-gnus-debug
            (message "Running: %s" add-cmd))
          
          (shell-command add-cmd)
          (message "Added to database: %s" message-id))
      
      ;; Clean up temp file
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(defun mu-gnus-mark-replied ()
  "Mark current email as replied in the database."
  (interactive)
  (let* ((message-id (mu-gnus-get-message-id))
         (cmd (format "%s update %s --replied 1"
                     mu-gnus-db-command
                     (shell-quote-argument message-id))))
    
    (when mu-gnus-debug
      (message "Running: %s" cmd))
    
    (shell-command cmd)
    (message "Marked as replied: %s" message-id)))

(provide 'mu-gnus)

;;; mu-gnus.el ends here
