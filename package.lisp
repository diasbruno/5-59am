(defpackage :fivefivenineam
  (:use #:cl)
  (:export
   #:run-test))

(in-package :fivefivenineam)

(defun run-test (suite-name test-name)
  `(5am:in-suite ,suite-name)
  (let* ((result (car (5am:run test-name))))
    (list (list suite-name test-name)
	  (if (5am::test-passed-p result) "PASS" "FAIL"))))

(defgeneric visit-test (test-kind &key &allow-other-keys))

(defmethod visit-test ((test-kind 5am::test-bundle) &key &allow-other-keys)
  ;; TODO(dias): find the best way to traverse this data structure
  nil)
(defmethod visit-test ((test-kind 5am::test-suite) &key &allow-other-keys)
  (with-slots (5am::names)
      (5am::tests test-kind)
    (cons (5am::name test-kind) (reverse 5am::names))))
(defmethod visit-test ((test-kind 5am::test-case) &key &allow-other-keys)
  nil)
(defmethod visit-test (test-kind &key &allow-other-keys)
  nil)
