;;; test_mu_gnus.el --- Tests for mu-gnus -*- lexical-binding: t -*-

(require 'ert)
(require 'json)
(require 'cl-lib)
(require 'gnus-sum)

;; Load the file to be tested.
(load-file (expand-file-name "../../elisp/mu-gnus.el" (file-name-directory load-file-name)))

;; Define a simple macro for mocking functions
(defmacro with-redefs (bindings &rest body)
  "Temporarily redefine functions for testing.
BINDINGS is a list of (SYMBOL . NEW-DEFINITION) pairs."
  (declare (indent 1))
  (let ((old-defs (mapcar (lambda (binding)
                            (cons (gensym "old-")
                                  (car binding)))
                          bindings)))
    `(let (,@(mapcar (lambda (old-def)
                      `(,(car old-def) (and (fboundp ',(cdr old-def))
                                           (symbol-function ',(cdr old-def)))))
                    old-defs))
       (unwind-protect
           (progn
             ,@(mapcar (lambda (binding)
                        `(fset ',(car binding) ,(cdr binding)))
                      bindings)
             ,@body)
         ,@(mapcar (lambda (old-def)
                    `(if ,(car old-def)
                         (fset ',(cdr old-def) ,(car old-def))
                       (fmakunbound ',(cdr old-def))))
                  old-defs)))))

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
    ;; The formatted output will use the system's timezone
    ;; So we just check the parts that don't depend on timezone
    (let ((result (mu-gnus-format-reply-entry entry)))
      (should (string-match "\\* TODO Reply to: Test Subject" result))
      (should (string-match "From: Test Sender <sender@example.com>" result))
      (should (string-match "Message-ID: <id@host>" result))
      ;; Check that date is formatted (but don't check exact time due to timezone)
      (should (string-match "Date: 2023-01-01" result)))))

;;;; Test functions with side-effects using mocks

(ert-deftest mu-gnus-get-message-id-test ()
  "Test extraction of Message-ID."
  ;; We need to mock both gnus-summary-buffer and the functions
  (let ((gnus-summary-buffer (current-buffer))
        (old-gnus-summary-article-header (and (fboundp 'gnus-summary-article-header)
                                              (symbol-function 'gnus-summary-article-header)))
        (old-mail-header-id (and (fboundp 'mail-header-id)
                                (symbol-function 'mail-header-id))))
    (unwind-protect
        (progn
          (fset 'gnus-summary-article-header (lambda () 'dummy-header))
          (fset 'mail-header-id (lambda (header) "<the-id@domain.com>"))
          (should (string= (mu-gnus-get-message-id) "<the-id@domain.com>")))
      ;; Restore original functions
      (if old-gnus-summary-article-header
          (fset 'gnus-summary-article-header old-gnus-summary-article-header)
        (fmakunbound 'gnus-summary-article-header))
      (if old-mail-header-id
          (fset 'mail-header-id old-mail-header-id)
        (fmakunbound 'mail-header-id)))))

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
      (should (string= last-shell-command
                       (format "bin/mail-db update %s --replied 1"
                               (shell-quote-argument "<msg-to-mark@replied.com>")))))))

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
    (with-redefs ((gnus-summary-article-number . (lambda () 1))
                  (mu-gnus-get-article-file . (lambda () "/tmp/dummy.eml"))
                  (file-exists-p . (lambda (_) t))
                  (delete-file . (lambda (_)))
                  (mu-gnus-get-message-id . (lambda () "<new@msg.com>"))
                  (shell-command-to-string . #'mock-shell-command-to-string)
                  (shell-command . #'mock-shell-command))
      (mu-gnus-add-to-db)
      (should (string= last-shell-command
                       (format "bin/mail-db add %s --from %s --subject %s --classification %s"
                               (shell-quote-argument "<new@msg.com>")
                               (shell-quote-argument "Test From <from@example.com>")
                               (shell-quote-argument "Test Subject")
                               (shell-quote-argument "important")))))))

;;; test_mu_gnus.el ends here
