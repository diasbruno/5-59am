(require 'sly)

(defface 559am:font-test-default-face
  '((t (:foreground "gray")))
  "Face for the first word in a line.")

(defface 559am:font-test-passed-face
  '((t (:foreground "green")))
  "Face for the first word in a line.")

(defface 559am:font-test-failed-face
  '((t (:foreground "red")))
  "Face for the first word in a line.")

(defvar 559am:*tests* (make-hash-table))

(defvar 559am:buffer-name "*test-buffer*")

(defun 559am:color-first-word ()
  (interactive)
  (with-current-buffer (get-buffer-create 559am:buffer-name)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(let ((start (point)))
	  (when (re-search-forward "^\\(\\w+\\)" (line-end-position) t)
	    (let ((color (save-excursion
			   (line-beginning-position nil)
			   (cond
			    ((string-equal "PASS" (thing-at-point 'symbol t)) '559am:font-test-passed-face)
			    ((string-equal "FAIL" (thing-at-point 'symbol t)) '559am:font-test-failed-face)
			    (t '559am:font-test-default-face)))))
	      (line-beginning-position nil)
	      (put-text-property (- (match-beginning 1) 3) (match-end 1) 'face color))
	    (put-text-property (match-end 1) (line-end-position) 'face 'default))
	  (forward-line 1))))))

(defun 559am:find-test-information-at-point ()
  "Return a pair of test suite and test name."
  (with-current-buffer (get-buffer-create 559am:buffer-name)
    (let (a b)
      (move-beginning-of-line nil)
      (re-search-forward "\\([0-9\\w-_]*\\):")
      (backward-word)
      (setq a (thing-at-point 'symbol))
      (re-search-forward ":\\([0-9\\w-_]*\\)")
      (setq b (thing-at-point 'symbol))
      (cons a b))))

(defun 559am:apply-result (result-data)
  (cl-destructuring-bind ((suite-name test-name) result)
      result-data
    (with-current-buffer (get-buffer-create 559am:buffer-name)
      (beginning-of-buffer)
      (let ((pos (re-search-forward (concat (symbol-name suite-name) ":" (symbol-name test-name)))))
	(move-beginning-of-line nil)
	(kill-word 1)
	(insert result)
	(559am:color-first-word)))))

(defun 559am:execute-test-name-on-suite ()
  "Get the string of the current line and display it in the minibuffer."
  (interactive)
  (cl-destructuring-bind (suite-name . test-name)
      (559am:find-test-information-at-point)
    ;; NOTE: always set in-suite before run the test
    (let ((result (read (sly-eval `(slynk:interactive-eval
				    ,(format "(progn (fivefivenineam:run-test '%s '%s))"
					     suite-name test-name
					     suite-name test-name))))))
      (559am:apply-result result))))

(defvar 559am:test-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") '559am:execute-test-name-on-suite)
    map)
  "Keymap for `test-buffer-mode`.")

(define-minor-mode 559am:test-buffer-mode
  "A minor mode for test buffers."
  :lighter " TestBuf"
  :keymap 559am:test-buffer-mode-map)

(defun 559am:load-tests ()
  (setq 559am:*tests* (make-hash-table))
  (mapcar (lambda (suite)
	    (puthash (car suite) (cdr suite) 559am:*tests*))
	  (read (sly-eval `(slynk:interactive-eval "(reverse
	   (mapcar
	    (lambda (x)
	      (visit-test (gethash x (5am::%tests 5am::*test*))))
	    5am::*toplevel-suites*))")))))

(defun 559am:find-all-tests ()
  "Create a new buffer with test suite names concatenated to test names.
TEST-SUITES is a list of lists where the first item is the test suite name and the rest are test names."
  (interactive)
  (let ((test-suites (559am:load-tests)))
    (with-current-buffer (get-buffer-create 559am:buffer-name)
      (erase-buffer)
      (maphash (lambda (suite-name tests)
		 (dolist (test-name tests)
		   (insert (format "NONE %s:%s\n" suite-name (symbol-name test-name)))))
	       559am:*tests*)
      (559am:test-buffer-mode 1))))

(global-set-key (kbd "C-c a t") '559am:find-all-tests)

(provide '559am)
