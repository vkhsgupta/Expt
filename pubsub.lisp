(ql:quickload "bordeaux-threads")


(defun make-rw ()
  (let* ((list nil)
	 (lock (bt:make-lock)))
    (flet ((read-channel ()
	     (loop while (null list)
		   do (sleep 1))
	     (bt:with-lock-held (lock)
	       (pop list)))
	   (write-channel (value)
	     (bt:with-lock-held (lock)
	       (setf list (list value)))))
      (list #'read-channel #'write-channel))))

	   
(defun make-channel ()
  (let* ((flag 0)
	 (lock (bt:make-lock))
	 (subscribers nil))
    (flet ((add-subscriber (rwpair)
	     (bt:with-lock-held (lock)
	       (push (cadr rwpair) subscribers)))
	   (remove-subscriber (rwpair)
	     (bt:with-lock-held (lock)
	       (delete (cadr rwpair) subscribers)))
	   (publish (value)
	     (bt:with-lock-held (lock)
	       (loop for s in subscribers
		     do (funcall s value)))))
      (list #'add-subscriber #'remove-subscriber #'publish))))

	     
