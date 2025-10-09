(defpackage :fivefivenineam
  (:use #:cl)
  (:export
   #:run-test
   #:visit-test
   #:load-tests
   #:get-report))

(in-package :fivefivenineam)

(defvar last-results (make-hash-table :test 'equal))

(defgeneric generate-report (result &key &allow-other-keys))

(defmethod generate-report ((result 5am::test-passed) &key stack &allow-other-keys)
  (setf (getf stack :result) (and (getf stack :result) t)
        (getf stack :reason) (format nil "~aPASS ~a~%~%" (getf stack :reason) (5am::test-expr result)))
  stack)

(defmethod generate-report ((result 5am::test-failure) &key stack &allow-other-keys)
  (setf (getf stack :result) nil
        (getf stack :reason) (format nil "~aFAIL ~a~%~%" (getf stack :reason) (5am::test-expr result)))
  stack)

(defun make-report (results)
  (reduce (lambda (acc test-result)
            (generate-report test-result :stack acc))
          (cdr results)
          :initial-value (generate-report (car results) :stack (list :result t :reason ""))))

(defun get-report (test-name)
  (let ((result (gethash (symbol-name test-name) last-results)))
    (list (symbol-name test-name)
          (getf result :result)
          (getf result :reason))))

(defun run-test (test-name)
  (let* ((results (5am:run test-name))
         (report (make-report results)))
    (setf (gethash (symbol-name test-name) last-results) report)
    (list test-name
          (if (getf report :result) "PASS" "FAIL"))))

(defun load-tests ()
  (flet ((resymbol (name)
           (alexandria:make-keyword (symbol-name name))))
    (reduce (lambda (acc suite-name)
              (let* ((bundle (gethash suite-name (5am::%tests 5am::*test*)))
                     (tests (5am::tests bundle))
                     (pkg (symbol-package suite-name))
                     (pkg-name (package-name pkg)))
                (uiop:if-let ((pkg-data (assoc pkg-name acc)))
                  (uiop:if-let ((suite-data (assoc suite-name (cdr pkg-data))))
                    (setf (cdr pkg-data)
                          (append (cdr pkg-data) (uiop:ensure-list (5am::%test-names tests))))
                    (pushnew (list (cons (resymbol suite-name)
                                         (mapcar #'resymbol (uiop:ensure-list (5am::%test-names tests)))))
                             (cdr pkg-data)))
                  (pushnew (cons (intern pkg-name)
                                 (list (cons (resymbol suite-name)
                                             (append (cdr pkg-data)
                                                     (mapcar #'resymbol (uiop:ensure-list (5am::%test-names tests)))))))
                           acc))
                acc))
            5am::*toplevel-suites*
            :initial-value nil)))
