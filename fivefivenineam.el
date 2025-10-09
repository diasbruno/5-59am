;;; fivefivenineam.el --- an simplier integration with fiveam and sly. -*- lexical: t -*-

;; Author: Bruno Dias <dias.h.bruno@gmail.com>
;; Maintainer: Bruno Dias <dias.h.bruno@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (cl-lib "0.6"))
;; Homepage: https://github.com/diasbruno/fivefivenineam
;; Keywords: convenience, tools, lisp
;; SPDX-License-Identifier: Unlicense

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This is a simple Emacs integration with sly and fiveam
;; to provide an easy to run tests.

;;; Code:

(require 's)
(require 'sly)

(cl-defstruct 559am:test-package name suites)
(cl-defstruct 559am:test-suite name tests)
(cl-defstruct 559am:test-test name result)

(defface 559am:font-test-default-face
  '((t (:foreground "gray")))
  "Face for the first word in a line.")

(defface 559am:font-test-package-face
  '((t (:foreground "yellow")))
  "Face for the first word in a line.")

(defface 559am:font-test-suite-face
  '((t (:foreground "orange")))
  "Face for the first word in a line.")

(defface 559am:font-test-passed-face
  '((t (:foreground "green")))
  "Face for the first word in a line.")

(defface 559am:font-test-failed-face
  '((t (:foreground "red")))
  "Face for the first word in a line.")

(defvar 559am:*tests* (make-hash-table))

(defvar 559am:buffer-name "5:59am *test-buffer*")

(defvar 559am:result-buffer-name "5:59am *result-buffer*")

(defvar 559am:+debugging+ nil)

(defvar 559am:*current-test* nil)

(defun 559am:color-first-word ()
  "Coloring the buffer."
  (interactive)
  (with-current-buffer (get-buffer-create 559am:buffer-name)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((start (point)))
          (when (re-search-forward "^\\(\\w+\\)" (line-end-position) t)
            (let* ((word (thing-at-point 'symbol t))
                   (color (save-excursion
                            (line-beginning-position nil)
                            (cond
                             ((string-equal "Package" word) '559am:font-test-package-face)
                             ((string-equal "Suite" word) '559am:font-test-suite-face)
                             ((string-equal "PASS" word) '559am:font-test-passed-face)
                             ((string-equal "FAIL" word) '559am:font-test-failed-face)
                             (t '559am:font-test-default-face)))))
              (line-beginning-position nil)
              (put-text-property (- (match-beginning 1) (1- (length word))) (match-end 1) 'face color))
            (put-text-property (match-end 1) (line-end-position) 'face 'default))
          (forward-line 1))))))

(defun 559am:find-test-information-at-point ()
  "Return a pair of test suite and test name."
  (with-current-buffer (get-buffer-create 559am:buffer-name)
    (let ((line (thing-at-point 'line t)))
      (second (s-split " " line)))))

(defmacro 559am:unlock-read-only-buffer (buffer &rest body)
  "Unlock a read-only BUFFER to write and lock it back BODY."
  `(progn
     (with-current-buffer ,buffer
       (read-only-mode 0)
       ,@body
       (read-only-mode 1))))

(defun block-movement-command ()
  (when (memq this-command '(next-line
                             previous-line
                             forward-char
                             backward-char))
    (message "Movement blocked!")
    (setq this-command 'ignore)))

(defun 559am:apply-result (result-data)
  "Mark the result of the test using RESULT-DATA."
  (cl-destructuring-bind (test-name result)
      result-data
    (let ((row (tabulated-list-get-entry)))
      (tabulated-list-set-col 2 (propertize (aref row 2) 'face (if result 'success 'error)))
      (remove-hook 'pre-command-hook #'block-movement-command))))

(defun 559am:execute-test-name-on-suite ()
  "Get the string of the current line and display it in the minibuffer."
  (interactive)
  (add-hook 'pre-command-hook #'block-movement-command)
  (setf 559am:*current-test* (tabulated-list-get-id))
  (let* ((row (tabulated-list-get-entry))
         (cmd (format "(fivefivenineam:run-test '%s)"
                      (format "%s::%s"
                              (substring-no-properties (aref row 0))
                              (aref row 2)))))
    (sly-eval-async `(slynk:interactive-eval ,cmd)
      (lambda (result) (559am:apply-result (read result))))))

(defun 559am:display-result (result)
  "Display RESULT."
  (let ((b (get-buffer-create 559am:result-buffer-name)))
    (with-current-buffer b
      (erase-buffer)
      (cl-destructuring-bind (test-name result reason)
          result
        (insert test-name)
        (newline)
        (insert "---------")
        (newline)
        (newline)
        (insert reason))
      (let ((new-window (split-window-right)))
        ;; Select the new window
        (select-window new-window)
        ;; Display the buffer in the new window
        (set-window-buffer new-window b)))))

(defun 559am:get-test-report ()
  "Retrive the result of a selected test on the test buffer."
  (interactive)
  (let ((test-information
         (format "(fivefivenineam:get-report '%s)"
                 (559am:find-test-information-at-point))))
    (when 559am:+debugging+
     (message "[5:59am] trying to run %s" test-information))
    (sly-eval-async `(slynk:interactive-eval ,test-information)
      (lambda (result) (559am:display-result (read result))))))

(defun 559am:%load-tests ()
  "Execute the (load-tests) command on the REPL."
  (read (sly-eval `(slynk:interactive-eval "(fivefivenineam:load-tests)"))))

(defun 559am:process (data)
  "Process the given DATA of a Lisp repl."
  (mapcar (lambda (pkg)
            (cl-destructuring-bind (pkg-name &rest suite-data)
                pkg
              (make-559am:test-package
               :name pkg-name
               :suites (mapcar (lambda (data)
                                 (cl-destructuring-bind (suite-name &rest tests)
                                     data
                                   (make-559am:test-suite :name suite-name :tests
                                                          (mapcar (lambda (test-name)
                                                                    (make-559am:test-test :name test-name))
                                                                  (ensure-list tests)))))
                               suite-data))))
          data))

(defun 559am:load-tests ()
  "Load all test."
  (let ((data (559am:%load-tests)))
    (setq 559am:*tests* (559am:process data)) ))

(defun 559am:switch-to-tests-buffer ()
  ""
  (interactive)
  (switch-to-buffer 559am:buffer-name)
  (559am-mode)
  (559am:tests--refresh)
  (tabulated-list-print t))

(defun 559am:switch-to-result-tests-buffer ()
  (interactive)
  (switch-to-buffer 559am:result-buffer-name))

(defun 559am:find-all-tests ()
  "Create a new buffer with test suite names concatenated to test names.
 TEST-SUITES is a list of lists where the first item is the test suite name
 and the rest are test names."
  (interactive)
  (559am:load-tests))

(define-derived-mode 559am-mode tabulated-list-mode "5:59am"
  "Major mode to display topics and their test results."
  :mode-name 559am-mode-map
  (setq tabulated-list-format [("Suite" 20 t)
                               ("Result" 10 t)
                               ("Title" 40 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Suite" nil))
  (add-hook 'tabulated-list-revert-hook #'559am:tests--refresh nil t)
  (tabulated-list-init-header))

(defun 559am:tests--refresh ()
  "Refresh `tabulated-list-entries` from `my-topic-list`."
  (559am:find-all-tests)
  (labels ((create-row (pkg suite test)
                       (let ((pkg-name (559am:test-package-name pkg))
                             (suite-name (559am:test-suite-name suite))
                             (test-name (559am:test-test-name test))
                             (test-result (559am:test-test-result test)))
                         (list (format "%s-%s-%s"
                                       (symbol-name pkg-name)
                                       (symbol-name suite-name)
                                       (symbol-name test-name))
                               (vector (propertize (symbol-name pkg-name) 'face 'org-table)
                                       (symbol-name suite-name)
                                       (symbol-name test-name))))))
    (setq tabulated-list-entries
          (cl-loop for pkg in 559am:*tests*
                   nconc (reduce (lambda (acc suite)
                                   (reduce (lambda (acc test)
                                             (push (create-row pkg suite test) acc))
                                           (559am:test-suite-tests suite)
                                           :initial-value acc))
                                 (559am:test-package-suites pkg)
                                 :initial-value nil)))))

(define-key 559am-mode-map (kbd "RET") '559am:get-test-report)
(define-key 559am-mode-map (kbd "e") '559am:execute-test-name-on-suite)
(define-key 559am-mode-map (kbd "n") 'next-line)
(define-key 559am-mode-map (kbd "p") 'previous-line)
(define-key 559am-mode-map (kbd "g") '559am:find-all-tests)

(global-set-key (kbd "C-c a t") '559am:find-all-tests)
(global-set-key (kbd "C-c a b") '559am:switch-to-tests-buffer)

(provide '559am)
;;; fivefivenineam.el ends here
