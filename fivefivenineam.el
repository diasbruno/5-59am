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

(defun 559am:color-first-word ()
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

(defun 559am:apply-result (result-data)
  "Mark the result of the test using RESULT-DATA."
  (cl-destructuring-bind (test-name result)
      result-data
    (with-current-buffer (get-buffer-create 559am:buffer-name)
      (read-only-mode 0)
      (beginning-of-line)
      (move-beginning-of-line nil)
      (kill-word 1)
      (insert result)
      (559am:color-first-word)
      (read-only-mode 1))))

(defmacro 559am:unlock-read-only-buffer (buffer &rest body)
  "Unlock a read-only BUFFER to write and lock it back BODY."
  `(progn
     (with-current-buffer ,buffer
       (read-only-mode 0)
       ,@body
       (read-only-mode 1))))

(defun 559am:execute-test-name-on-suite ()
  "Get the string of the current line and display it in the minibuffer."
  (interactive)
  (let ((test-information (559am:find-test-information-at-point)))
    (sly-eval-async `(slynk:interactive-eval
                      ,(format "(fivefivenineam:run-test '%s)" test-information))
      (lambda (result) (559am:apply-result (read result))))))

(defun 559am:display-result (result)
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
  (interactive)
  (let ((test-information
         (format "(fivefivenineam:get-report '%s)"
                 (559am:find-test-information-at-point))))
    (when 559am:+debugging+
     (message "[5:59am] trying to run %s" test-information))
    (sly-eval-async `(slynk:interactive-eval ,test-information)
      (lambda (result) (559am:display-result (read result))))))

(defvar 559am:test-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") '559am:get-test-report)
    (define-key map (kbd "e") '559am:execute-test-name-on-suite)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "g") '55am:find-all-tests)
    map)
  "Keymap for `test-buffer-mode`.")

(define-minor-mode 559am:test-buffer-mode
  "A minor mode for test buffers."
  :lighter " TestBuf"
  :keymap 559am:test-buffer-mode-map)

(defun 559am:%load-tests ()
    (read (sly-eval `(slynk:interactive-eval "(fivefivenineam:load-tests)"))))

(defun 559am:process (data)
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
  (let ((data (559am:%load-tests)))
    (setq 559am:*tests* (559am:process data)) ))

(defun 559am:switch-to-tests-buffer ()
  (interactive)
  (switch-to-buffer 559am:buffer-name))

(defun 559am:switch-to-result-tests-buffer ()
  (interactive)
  (switch-to-buffer 559am:result-buffer-name))

(defun 559am:find-all-tests ()
  "Create a new buffer with test suite names concatenated to test names.
 TEST-SUITES is a list of lists where the first item is the test suite name
 and the rest are test names."
  (interactive)
  (let ((test-suites (559am:load-tests)))
    (559am:unlock-read-only-buffer (get-buffer-create 559am:buffer-name)
                                   (progn
                                     (erase-buffer)
                                     (map nil (lambda (pkg)
                                                (map nil (lambda (suite)
                                                           (insert (format "Suite %s\n" (559am:test-suite-name suite)))
                                                           (map nil
                                                                (lambda (test)
                                                                  test
                                                                  (insert (format "None %s::%s\n"
                                                                                  (559am:test-package-name pkg)
                                                                                  (559am:test-test-name test))))
                                                                (559am:test-suite-tests suite)))
                                                     (559am:test-package-suites pkg)))
                                          559am:*tests*)
                                     (559am:color-first-word)
                                     (559am:test-buffer-mode 1))))
  (559am:switch-to-tests-buffer))

(global-set-key (kbd "C-c a t") '559am:find-all-tests)
(global-set-key (kbd "C-c a b") '559am:switch-to-tests-buffer)

(provide '559am)
