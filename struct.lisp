;;; Checking Structure Mutability

(defstruct st
  (id 0 :type fixnum)
  (cntnt nil :type (or null cons)))


(defparameter *new-msg* (let ((n -1)
			      (m 1000))
			  #'(lambda ()
			      (incf n)
			      (make-st :id n :cntnt (list (random m))))))


(defun change-x ()
  (setf x 10))


(defun test-change ()
  (let* ((x 0))
    (format t "Before change X is ~a~%" x)
    (funcall #'change-x)
    (format t "After funcall change X is ~a~%" x)))



(defun change-struct (s newval)
  (setf (st-num s) newval))

(defun print-st (s)
  (format t "The num in struct is ~a~%" (st-num s)))

