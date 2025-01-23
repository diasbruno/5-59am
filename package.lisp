(defpackage :fivefivenineam
  (:use #:cl)
  (:export
   #:run-test
   #:visit-test
   #:load-tests))

(in-package :fivefivenineam)

(defun run-test (test-name)
  (let ((result (car (print (5am:run test-name)))))
    (print (list test-name
		 (if (5am::test-passed-p result) "PASS" "FAIL")))))

(defun load-tests ()
  (flet ((resymbol (name)
	   (intern (string-upcase (symbol-name name)))))
   (reduce (lambda (acc suite-name)
	     (let* ((bundle (gethash suite-name (5am::%tests 5am::*test*)))
		    (tests (5am::tests bundle))
		    (pkg (symbol-package suite-name))
		    (pkg-name (package-name pkg)))
	       (print acc)
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
