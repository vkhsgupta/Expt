(ql:quickload :bordeaux-threads)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ptprint-fixed ()
      (let ((top-level *standard-output*))
        (bt:make-thread
         (lambda ()
           (format top-level "Hello from thread!"))
         :name "hello")))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro while (condition &rest body)
  `(loop while ,condition
         do (progn
              ,@body)))



(defparameter *t* *standard-output*)

(defun pexec (f)
  (bt:make-thread
   (lambda ()
     (funcall f))))

(defun my-print (text)
  (sleep 10)
  (format *t* "~a~%" text))

(defun init ()
  ;;;; Take it in run-task and use let*.
  ;;;; Caveat, First ensure it/let*-access to named variables  works across threads as well.
  (defparameter *f12a* 0)
  (defparameter *f12b* 0)
  (defparameter *n* 0)
  (defparameter *i1* 0)
  (defparameter *l12a* nil)
  (defparameter *l12b* nil)
  (defparameter *flag* t)
  (defparameter *stop* 0))


  


(defun send-from-t1-to-t2 ()
  (when (zerop *f12a*)
    (format *t* "In t1-send f12a is zero so copying here f12a is ~a~%" *f12a*)
    (format *t* "In t1-send before-copy *l12a*, *s12*, and *f12a* are ~a, ~a, and ~a~%" *l12a* *s12* *f12a*)
    (setf *l12a* (copy-list *s12*))
    (setf *s12* nil)
    (setf *f12a* 1)
    (format *t* "In t1-send post-copy *l12a*, *s12*, and *f12a* are ~a, ~a, and ~a~%" *l12a* *s12* *f12a*)
    (return-from send-from-t1-to-t2))
  (when (zerop *f12b*)
    (format *t* "In t1-send f12b is zero so copying here f12b is ~a~%" *f12b*)
    (format *t* "In t1-send before-copy *l12b*, *s12*, and *f12b* are ~a, ~a, and ~a~%" *l12a* *s12* *f12a*)
    (setf *l12b* (copy-list *s12*))
    (setf *s12* nil)
    (setf *f12b* 1)
    (format *t* "In t1-send post-copy *l12b*, *s12*, and *f12b* are ~a, ~a, and ~a~%" *l12a* *s12* *f12a*)))


(defun get-from-t1-in-t2 ()
  (when (= 1  *f12a*)
    (format *t* "In t2-get f12a is 1 so copying here f12a is ~a~%" *f12a*)
    (format *t* "In t2-get before-copy *l12a*, *s21*, and *f12a* are ~a, ~a, and ~a~%" *l12a* *s21* *f12a*)
    (setf *s21* (append *s21* (copy-list *l12a*)))
    (setf *l12a* nil)
    (setf *f12a* 0)
    (format *t* "In t2-get post-copy *l12a*, *s21*, and *f12a* are ~a, ~a, and ~a~%" *l12a* *s21* *f12a*))
  (when (= 1 *f12b*)
    (format *t* "In t2-get f12b is zero so copying here f12b is ~a~%" *f12b*)
    (format *t* "In t2-get before-copy *l12b*, *s21*, and *f12b* are ~a, ~a, and ~a~%" *l12b* *s21* *f12b*)
    (setf *s21* (append *s21* (copy-list *l12b*)))
    (setf *l12b* nil)
    (setf *f12b* 0)
    (format *t* "In t2-get post-copy *l12b*, *s21*, and *f12b* are ~a, ~a, and ~a~%" *l12b* *s21* *f12b*)))

(defun work-t1 ()
  (sleep (random 0.00001))
  (setf s12 (append s12 (list (funcall *new-msg*))))
  (format *t* "In t1 s12 is ~a~%" *s12*))


(defun work-t2 ()
  (format *t* "In t2                               Received ~a~%" (pop s2))
  (sleep (random 0.00001)))


(defun thread-t1 (n)
  ;;; Use let* construct here
  (format *t* "n is ~a~%" n)
  (setf *n* n)
  (setf s12 '(1))
  ;;;; Use dotimes or improvised while. Not index based.
  (while (<= *i1* *n*)
	 (format *t* "In t1 before calling send s12 is ~a    ~a~%" s12)
	 (send-from-t1-to-t2)
	 (format *t* "In t1 after returning from send s12 is ~a~%" s12)
	 (dotimes (j (random 5))
	   (work-t1))))


(defun loop-2 ()
  ;;; Use let* construct 
  (setf *flag* 1)
  (setf s2 nil)
  (setf s21 nil)
  ;;;; Try to use something better than flag
  (while *flag*
	 (incf *stop*)
	 (format *t* "In t2 before-get s2 and s21 are ~a, ~a~%" *s2* *s21*)
	 (get-from-t1-in-t2)
	 (format *t* "In t2 after-get before append s2 and s21 are ~a, ~a~%" *s2* *s21*)
	 (setf *s2* (append *s2* (copy-list *s21*)))
	 (format *t* "In t2 after-get after append s2 and s21 are ~a, ~a~%" *s2* *s21*)
	 (setf *s21* nil)
	 (format *t* "In t2 for work loop s2 and s21 are ~a, ~a~%" *s2* *s21*)
	 (while (not (null *s2*))
		(if (>=  (car *s2*) *n*)
		    (setf *flag* nil))
		(work-t2))
	 (if (>= *stop* 10)
	     (setf *flag* nil))))

(defun run-talk (n)
  ;;; Try to use let* to define parameters, rather than global *<name>* parameters
  (init)
  (pexec (lambda () (funcall #'loop-1 n)))
  (pexec #'loop-2))

(defun show ()
  (bt:all-threads))

(defun kill-one ()
  (bt:destroy-thread (car (bt:all-threads))))


(kill-one)
