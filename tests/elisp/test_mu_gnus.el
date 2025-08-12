;;; test_mu_gnus.el --- Tests for mu-gnus -*- lexical-binding: t -*-

(require 'ert)
(require 'json)
(require 'cl-lib)

;; Load the file to be tested.
(load-file (expand-file-name "../../elisp/mu-gnus.el" (file-name-directory load-file-name)))

;;;; Test pure functions

(ert-deftest mu-gnus-parse-db-json-test ()
  "Test parsing of valid JSON from mail-db."
  (let ((json-str "[{\"message_id\":\"<123@a.com>\",\"from_addr\":\"a@b.com\",\"subject\":\"subj\",\"classification\":\"important\",\"needs_reply\":1,\"replied\":0,\"date\":1672531200}]")
        (json-array-type 'list)
        (json-object-type 'alist))
    (should (equal (mu-gnus-parse-db-json json-str)
                   '(((message_id . "<123@a.com>")
                      (from_addr . "a@b.com")
                      (subject . "subj")
                      (classification . "important")
                      (needs_reply . 1)
                      (replied . 0)
                      (date . 1672531200)))))))

(ert-deftest mu-gnus-parse-db-json-empty-test ()
  "Test parsing of an empty JSON array."
  (should (null (mu-gnus-parse-db-json "[]"))))

(ert-deftest mu-gnus-parse-db-json-invalid-test ()
  "Test parsing of invalid JSON."
  (let ((mu-gnus-debug t)) ; to cover the error message path
    (should (null (mu-gnus-parse-db-json "[{]")))))

(ert-deftest mu-gnus-format-reply-entry-test ()
  "Test formatting of a reply entry for display."
  (let ((entry '((message_id . "<id@host>")
                 (from_addr . "Test Sender <sender@example.com>")
                 (subject . "Test Subject")
                 (date . 1672531200)))) ; Corresponds to 2023-01-01 00:00:00 UTC
    (with-temp-buffer
      (let ((system-time-zone "UTC")) ; Set timezone for consistent test results
        (should (string= (mu-gnus-format-reply-entry entry)
                         "* TODO Reply to: Test Subject\n  From: Test Sender <sender@example.com>\n  Date: 2023-01-01 00:00\n  Message-ID: <id@host>\n"))))))

;;;; Test functions with side-effects using mocks

(ert-deftest mu-gnus-get-message-id-test ()
  "Test extraction of Message-ID."
  (let ((gnus-summary-buffer (current-buffer))) ; needed for with-current-buffer
    (with-redefs ((gnus-summary-article-header . (lambda () '((message-id . "<the-id@domain.com>")))))
      (should (string= (mu-gnus-get-message-id) "<the-id@domain.com>")))))

(defvar last-shell-command nil)
(defun mock-shell-command (command &optional output-buffer error-buffer)
  (setq last-shell-command command)
  0)

(ert-deftest mu-gnus-mark-replied-test ()
  "Test marking a message as replied."
  (let ((mu-gnus-db-command "bin/mail-db")
        (last-shell-command nil))
    (with-redefs ((mu-gnus-get-message-id . (lambda () "<msg-to-mark@replied.com>"))
                  (shell-command . #'mock-shell-command))
      (mu-gnus-mark-replied)
      (should (string= last-shell-command "bin/mail-db update '<msg-to-mark@replied.com>' --replied 1")))))

(defun mock-shell-command-to-string (command)
  (cond
   ((string-match-p "--header From" command) "Test From <from@example.com>")
   ((string-match-p "--header Subject" command) "Test Subject")
   ((string-match-p "mail-classify.py" command) "important")
   (t "")))

(ert-deftest mu-gnus-add-to-db-test ()
  "Test adding a message to the database."
  (let ((mu-gnus-db-command "bin/mail-db")
        (mu-gnus-extract-command "bin/mail-extract")
        (mu-gnus-classify-command "python3 src/python/mail-classify.py")
        (last-shell-command nil))
    (with-redefs (gnus-summary-article-number (lambda () 1))
      (with-redefs (mu-gnus-get-article-file (lambda () "/tmp/dummy.eml"))
        (with-redefs (file-exists-p (lambda (_) t))
          (with-redefs (delete-file (lambda (_)))
            (with-redefs (mu-gnus-get-message-id (lambda () "<new@msg.com>"))
              (with-redefs (shell-command-to-string #'mock-shell-command-to-string)
                (with-redefs (shell-command #'mock-shell-command)
                  (mu-gnus-add-to-db)
                  (should (string= last-shell-command "bin/mail-db add '<new@msg.com>' --from 'Test From <from@example.com>' --subject 'Test Subject' --classification 'important'")))))))))))

;;; test_mu_gnus.el ends here
