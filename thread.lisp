(ql:quickload :bordeaux-threads)

(defparameter *t* *standard-output*)

(defun run-times (f n tt)
  (dotimes (i n)
    (funcall  f)
    (sleep tt))
  (format *t* "Done!! ~a" tt))


(defun pexec (f)
  (bt:make-thread
   (lambda ()
     (funcall f))))


(defun ptprint ()
  (bt:make-thread
   (lambda ()
     (format *t* "Value is ~a" 10))))


(defun f1 ()
  (format *t* "Hello, World! 1~%"))

(defun f2 ()
  (format *t* "                          Hello, World! 2~%"))

(defun f3 ()
  (format *t* "                                                       Hello, World! 3~%"))

(defun run ()
  (pexec (lambda () (funcall #'run-times #'f1 10 2)))
  (pexec (lambda () (funcall #'run-times #'f2 10 1)))
  (pexec (lambda () (funcall #'run-times #'f3 10 1.5))))


(run-times #'f1 10)

(pexec #'f1)


(defun print-message-top-level-fixed ()
      (let ((top-level *standard-output*))
        (bt:make-thread
         (lambda ()
           (format top-level "Hello from thread!"))
         :name "hello")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro while (condition &rest body)
  `(loop while ,condition
         do (progn
              ,@body)))



(defun init ()
  (defparameter *f12a* 0)
  (defparameter *f12b* 0)
  (defparameter *s12* nil)
  (defparameter *s21* nil)
  (defparameter *s2* nil)
  (defparameter *n* 0)
  (defparameter *i1* 0)
  (defparameter *l12a* nil)
  (defparameter *l12b* nil)
  (defparameter *flag* t))



(defun send-from-t1-to-t2 ()
  (when (zerop *f12a*)
    (setf *l12a* (copy-list *s12*))
    (setf *s12* nil)
    (setf *f12a* 1)
    (return-from send-from-t1-to-t2))
  (when (zerop *f12b*)
    (setf *l12b* (copy-list *s12*))
    (setf *s12* nil)
    (setf *f12b* 1)))


(defun get-from-t1-in-t2 ()
  (when (= 1  *f12a*)
    (setf *s21* (copy-list *l12a*))
    (setf *l12a* nil)
    (setf *f12a* 0))
  (when (= 1 *f12b*)
    (setf *s21* (append *s21* (copy-list *l12b*)))
    (setf *l12b* nil)
    (setf *f12b* 0)))

(defun work-t1 ()
  (sleep (random 0.00001))
  (setf *s12* (append *s12* (list (incf (car (last *s12*))))))
  (incf *i1*)
  (format *t* "~a~%" *s12*))


(defun work-t2 ()
  (format *t* "                               Received ~a~%" (pop *s2*))
  (sleep (random 0.00001)))


(defun loop-1 (n)
  (setf *n* n)
  (setf *s12* '(1))
  (setf *i1* 0)
  (while (<= *i1* *n*)
	 (setf *i1* (car (last *s12*)))
	 (format *t* "~a    ~a~%" *i1* *s12*)
	 (send-from-t1-to-t2)
	 (format *t* "~a~%" *s12*)
	 (if (null *s12*)
	     (setf *s12* (list (incf *i1*))))
	 (dotimes (j 10)
	   (work-t1))))


(defun loop-2 ()
  (setf *flag* 1)
  (setf *s2* nil)
  (while *flag*
	 (get-from-t1-in-t2)
	 (setf *s2* (append *s2* (copy-list *s21*)))
	 (setf *s21* nil)
	 (while *s2*
		(if (= *n* (car *s2*))
		    (setf *flag* nil))
		(work-t2))))

(defun run-talk (n)
  (init)
  (pexec (lambda () (funcall #'loop-1 n)))
  (pexec #'loop-2))
