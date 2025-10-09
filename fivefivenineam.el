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

(require 'cl)
(require 's)
(require 'sly)

(cl-defstruct fivefivenineam-test-package name suites)
(cl-defstruct fivefivenineam-test-suite name tests)
(cl-defstruct fivefivenineam-test-test name result)

(defface fivefivenineam-font-test-default-face
  '((t (:foreground "gray")))
  "Face for the first word in a line.")

(defface fivefivenineam-font-test-package-face
  '((t (:foreground "yellow")))
  "Face for the first word in a line.")

(defface fivefivenineam-font-test-suite-face
  '((t (:foreground "orange")))
  "Face for the first word in a line.")

(defface fivefivenineam-font-test-passed-face
  '((t (:foreground "green")))
  "Face for the first word in a line.")

(defface fivefivenineam-font-test-failed-face
  '((t (:foreground "red")))
  "Face for the first word in a line.")

(defvar *fivefivenineam-tests* (make-hash-table))

(defvar +fivefivenineam-buffer-name+ "5:59am|*test-buffer*")

(defvar +fivefivenineam-result-buffer-name+ "5:59am|*result-buffer*")

(defvar *fivefivenineam-debugging+ nil)

(defmacro fivefivenineam-unlock-read-only-buffer (buffer &rest body)
  "Unlock a read-only BUFFER to write and lock it back BODY."
  `(progn
     (with-current-buffer ,buffer
       (read-only-mode 0)
       ,@body
       (read-only-mode 1))))

(defun fivefivenineam--block-movement-command ()
  (when (memq this-command '(next-line
                             previous-line
                             forward-char
                             backward-char))
    (setq this-command 'ignore)))

(defun fivefivenineam--apply-result (result-data)
  "Mark the result of the test using RESULT-DATA."
  (cl-destructuring-bind (test-name result)
      result-data
    (let ((row (tabulated-list-get-entry)))
      (tabulated-list-set-col 2 (propertize (aref row 2) 'face (if result
                                                                   'fivefivenineam-font-test-passed-face
                                                                 'fivefivenineam-font-test-failed-face)))
      (remove-hook 'pre-command-hook #'fivefivenineam--block-movement-command))))

(defun fivefivenineam--display-result (result)
  "Display RESULT."
  (let ((b (get-buffer-create +fivefivenineam-result-buffer-name+)))
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

(defun fivefivenineam--process (data)
  "Process the given DATA of a Lisp repl."
  (mapcar (lambda (pkg)
            (cl-destructuring-bind (pkg-name &rest suite-data)
                pkg
              (make-fivefivenineam-test-package
               :name pkg-name
               :suites (mapcar (lambda (data)
                                 (cl-destructuring-bind (suite-name &rest tests)
                                     data
                                   (make-fivefivenineam-test-suite
                                    :name suite-name
                                    :tests
                                    (mapcar (lambda (test-name)
                                              (make-fivefivenineam-test-test :name test-name))
                                            (ensure-list tests)))))
                               suite-data))))
          data))

(defun fivefivenineam--load-tests ()
  "Execute the (load-tests) command on the REPL."
  (read (sly-eval `(slynk:interactive-eval "(fivefivenineam:load-tests)"))))

(defun fivefivenineam--refresh-tests ()
  "Refresh `tabulated-list-entries` from `my-topic-list`."
  (cl-labels ((create-row (pkg suite test)
                (let ((pkg-name (fivefivenineam-test-package-name pkg))
                      (suite-name (fivefivenineam-test-suite-name suite))
                      (test-name (fivefivenineam-test-test-name test))
                      (test-result (fivefivenineam-test-test-result test)))
                  (list (format "%s-%s-%s"
                                (symbol-name pkg-name)
                                (symbol-name suite-name)
                                (symbol-name test-name))
                        (vector (propertize (symbol-name pkg-name) 'face 'fivefivenineam-font-test-default-face)
                                (symbol-name suite-name)
                                (symbol-name test-name))))))
    (setq tabulated-list-entries
          (cl-loop for pkg in *fivefivenineam-tests*
                   nconc (reduce (lambda (acc suite)
                                   (reduce (lambda (acc test)
                                             (push (create-row pkg suite test) acc))
                                           (fivefivenineam-test-suite-tests suite)
                                           :initial-value acc))
                                 (fivefivenineam-test-package-suites pkg)
                                 :initial-value nil)))))

(defun fivefivenineam-execute-test-name-on-suite ()
  "Get the string of the current line and display it in the minibuffer."
  (interactive)
  (fivefivenineam--set-buffers-mode-line-process " [Running]")
  (add-hook 'pre-command-hook #'fivefivenineam--block-movement-command)
  (let* ((row (tabulated-list-get-entry))
         (cmd (format "(fivefivenineam:run-test '%s)"
                      (format "%s::%s"
                              (substring-no-properties (aref row 0))
                              (aref row 2)))))
    (sly-eval-async `(slynk:interactive-eval ,cmd)
      (lambda (result)
        (fivefivenineam--set-buffers-mode-line-process " [Finished]")
        (fivefivenineam--apply-result (read result))))))

(defun fivefivenineam-get-test-report ()
  "Retrive the result of a selected test on the test buffer."
  (interactive)
  (let ((test-information
         (format "(fivefivenineam:get-report '%s)"
                 (559am:find-test-information-at-point))))
    (when 559am:+debugging+
      (message "[5:59am] trying to run %s" test-information))
    (sly-eval-async `(slynk:interactive-eval ,test-information)
      (lambda (result) (fivefivenineam--display-result (read result))))))

(defun fivefivenineam-load-tests ()
  "Load all test."
  (let ((data (fivefivenineam--load-tests)))
    (setq *fivefivenineam-tests*
          (fivefivenineam--process data))))

(defun fivefivenineam-find-all-tests ()
  "Create a new buffer with test suite names concatenated to test names."
  (interactive)
  (fivefivenineam-switch-to-tests-buffer)
  (fivefivenineam--set-buffers-mode-line-process " [Running]")
  (fivefivenineam-load-tests)
  (fivefivenineam--set-buffers-mode-line-process " [Finished]"))

(defun fivefivenineam-switch-to-tests-buffer ()
  "Switch to tests buffer."
  (interactive)
  (switch-to-buffer +fivefivenineam-buffer-name+)
  (fivefivenineam-mode)
  (fivefivenineam--refresh-tests)
  (tabulated-list-print t))

(defun fivefivenineam-switch-to-result-tests-buffer ()
  "Switch to results test buffer."
  (interactive)
  (switch-to-buffer +fivefivenineam-result-buffer-name+))

(defun fivefivenineam--set-buffers-mode-line-process (status)
  (with-current-buffer (get-buffer-create +fivefivenineam-buffer-name+)
    (setq mode-line-process (list status))))

(define-derived-mode fivefivenineam-mode tabulated-list-mode "5:59am"
  "Major mode to display topics and their test results."
  :mode-name fivefivenineam-mode-map
  (setq tabulated-list-format [("Suite" 20 t)
                               ("Result" 10 t)
                               ("Title" 40 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Suite" nil))
  (add-hook 'tabulated-list-revert-hook #'fivefivenineam--refresh-tests nil t)
  (tabulated-list-init-header))

(define-key fivefivenineam-mode-map (kbd "RET") 'fivefivenineam-get-test-report)
(define-key fivefivenineam-mode-map (kbd "e") 'fivefivenineam-execute-test-name-on-suite)
(define-key fivefivenineam-mode-map (kbd "n") 'next-line)
(define-key fivefivenineam-mode-map (kbd "p") 'previous-line)
(define-key fivefivenineam-mode-map (kbd "g") 'fivefivenineam-find-all-tests)

(global-set-key (kbd "C-c a t") 'fivefivenineam-find-all-tests)
(global-set-key (kbd "C-c a b") 'fivefivenineam-switch-to-tests-buffer)

(provide 'fivefivenineam)
;;; fivefivenineam.el ends here
