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






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Just back up. Use as examples to try out things

(defun my-handler (env)
  ;(format t "~a~%~%~%" env)
  (let ((client (getf env :clack.io)))
    (bt:with-lock-held (*c-lock*)
      (push client *clist*))
    (format t "Client is   ~a~%~%~%" client))
  '(200 (:content-type "text/event-stream" :cache-control "no-cache") ("Hello, World, Changed")))


(defun my-handler-2 (env)
  (lambda (responder)
    (let ((content "Responder Hello"))
      (funcall responder `(200 (:content-type "text/plain") (,content))))))


(defun my-handler-3 (env)
  (lambda (responder)
    (let ((writer (funcall responder '(200 (:content-type "text/plain")))))
      (loop for chunk = "Chunk here"
            do (funcall writer chunk :close (null chunk))
            while (evenp (random 9))))))

(let* ((l nil))
  (defun blocking-read ()
    (loop while  (null l)
	  do (sleep 1))
    (pop l))
  (defun nb-write (v)
    (push v l)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Oringinal by svetlyak40wt.  Works.

(defun events-handler-1 (env)
  (declare (ignorable env))
  (let ((output (getf env :raw-body)))
    (lambda (handle-normal-response)
      (declare (ignorable handle-normal-response))
      ;(format output "200 OK~C~C" #\return #\newline)
      (format output "HTTP/1.1~C~C" #\Return #\Newline)
      (format output "Content-Type: text/event-stream~C~C" #\Return #\Newline)
      (format output "~C~C" #\Return #\Newline)
      (format output "~C~C" #\Return #\Newline)
      (force-output output)
      
      (handler-case
          (loop for i from 1 upto 1000
                do (log:info "Returning event" i)
                   (format output "data: Event ~A~%~%" i)
                   (force-output output)
                   (sleep 1))
        (sb-int:broken-pipe ()
          ;; This condition will be signaled if user will close the "tab"
          (log:info "Connection was closed")
          nil)))))

;; My first modification. works.
;; I replaced #\return etc with ~% in format
(defun events-handler-2 (env)
  (declare (ignorable env))
  (let ((output (getf env :raw-body)))
    (lambda (handle-normal-response)
      (declare (ignorable handle-normal-response))
      (format output "HTTP/1.1~%")
      (format output "Content-Type: text/event-stream~%~%~%")
      (force-output output)  
      (handler-case
          (loop for i from 1 upto 1000
                do (format output "data: Event ~A~%~%" i)
                   (force-output output)
                   (sleep 1))
        (sb-int:broken-pipe ()
          ;; This condition will be signaled if user will close the "tab"
          (log:info "Connection was closed")
          nil)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *server* nil)

(defparameter *app*
  (lambda (env)
    (funcall #'my-handler env)))


(defun start-server (&key (port 8080))
  (stop-server)
  (setf *server*
        (clack:clackup *app*
                       :port port)))


(defun stop-server ()
  (when *server*
    (clack:stop *server*)
    (setf *server*
          nil)))

(defparameter *clist* nil)
(defparameter *c-lock* (bt:make-lock))

(defun publish-message (&optional (message "Hello"))
  (loop for c in *clist*
	do (handler-case
	       (format (client-stream c) "I am writing~a~%" message)
	     (error ()
	       (bt:with-lock-held (*c-lock*)
		 (delete c *clist* :test #'eq))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Make response for event-stream and write to output
;;;


(defun my-handler-4 (env)
  (let (out (getf env :raw-body))
    (cons out *olist*))
  (lambda (responder)
    (let ((writer (funcall responder '(200 (:content-type "text/plain")))))
      (loop for chunk = "Chunk here"
            do (funcall writer chunk :close (null chunk))
            while (evenp (random 9))))))




(start-server)
(stop-server)


;;;;;;;;;;;;; JUNK? ;;;;;;;;;;;;;;;;;;;;;

;;
(defparameter *clist* nil)
(defparameter *c-lock* (bt:make-lock))
;;

(defun publish(env)
  (lambda (responder)
    (format t "Subscriber list is ~a~%" *slist*)
    (loop for c in *clist*
 	  do (handler-case
		 (progn
		   (format t "Stream is ~a~%" out)
		   (format out "data: Event ~%~%")
		   (force-output out))
	       (sb-int:broken-pipe ()
		 (bt:with-lock-held (*s-lock*)
		   (delete out *slist* #'eq)))))))




;;
;; This will subscribe to the publish list
;;
(defun events-handler(env)
  (declare (ignorable env))
  (let ((c (getf env :clack.io)))
    (bt:with-lock-held (*c-lock*)
      (push c *clist*)))
  (lambda (responder)
    (let ((writer (funcall responder '(200 (:content-type "text/event-stream" :cache-control "no-cache" :keep-alive t)))))
      (ignore-errors
       (funcall writer (format nil "~%~%") :close nil)))))








