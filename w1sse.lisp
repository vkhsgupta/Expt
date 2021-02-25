(ql:quickload '(log4cl spinneret clack lack))

(defpackage #:server-side
  (:use #:cl))
(in-package server-side)


(defun render-page ()
  (spinneret:with-html-string
    (:html
     (:head
      (:script "
var source = new EventSource('/events');
source.addEventListener('message', function(e) {
    var events = document.getElementById('events')
    var new_element = document.createElement('li')
    new_element.innerText = e.data;
    events.appendChild(new_element)
}, false);
"))
     (:body
      (:h1 "Server-side events Clack example")
      (:h2 "Events:")
      (:ul :id "events")))))


(defun main-app (env)
  (declare (ignorable env))
  (list 200 '(:content-type "text/html")
        (list (render-page))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PubSub
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


(defun make-sse-msg (msg &key (id nil) (event nil) (reply nil))
  (let ((id (if (null id)
		""
		(format nil "id: ~a~%" id)))
	(event (if (null event)
		   ""
		   (format nil "Event: ~a~%" event)))
	(reply (if (null reply)
		   ""
		   (format nil "reply: ~a~%" reply)))
	(msg (format nil "data: ~a~%" msg)))
    (format nil "~a~a~a~a~%" reply event msg id)))


(make-sse-msg "hello" :id 10 :reply 10000 :event "fun")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *c* (make-channel))


;; Kinda works Without handling errors.
;; Need to modify
(defun events-handler (env)
  (declare (ignorable env))
  (lambda (responder)
    (let* ((writer (funcall responder '(200 (:content-type "text/event-stream" :cache-control "no-cache"))))
	   (prw (make-rw)))
      (funcall (car *c*) prw)
      (ignore-errors
       (funcall writer (format nil "~%") :close nil)
       (loop for chunk = (funcall (car prw)) ;; 
             do (funcall writer chunk :close (null chunk))
		;(format t "~a~%" chunk)
                ;(sleep 2)
	     while chunk)))))





(defun make-app ()
  (lack:builder
   (:mount "/events"
           'events-handler)
   'main-app))



(defparameter *server* nil)


(defun start-server (&key (port 8080))
  (stop-server)
  (setf *server*
        (clack:clackup (make-app)
                       :port port)))


(defun stop-server ()
  (when *server*
    (clack:stop *server*)
    (setf *server*
          nil)))

(start-server)

(stop-server)

(funcall (caddr *c*) "Hello")

(defun publish (channel value)
  (funcall (caddr channel) (format nil "~a" value)))


(publish *c* 100)


(publish *c* (format nil (make-sse-msg "Fun hello" :id 2 :event "again-fun")))


