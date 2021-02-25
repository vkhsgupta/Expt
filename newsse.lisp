;; This example was created to help the author of this thread:
;; https://www.reddit.com/r/Common_Lisp/comments/hu019r/what_is_the_recommended_way_to_readunderstand/

;; Before loading this code, do this in the REPL:
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




(defun make-app ()
  (lack:builder
   (:mount "/events"
           'events-handler)
   (:mount "/publish"
	   'publish)
   'main-app))


(defparameter *server* nil)


(defun start-server (&key (port 8080))
  (stop-server)
  (setf *clist* nil)
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

*slist*

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


;; Kinda works Without handling errors.
;; Need to modify
(defun events-handler-3 (env)
  (declare (ignorable env))
  (lambda (responder)
    (let ((writer (funcall responder '(200 (:content-type "text/event-stream" :cache-control "no-cache")))))
      (ignore-errors
       (funcall writer (format nil "~%") :close nil)
       (loop for chunk = (format nil "data: Event~%~%") ;; Need two ~%. Fewer fails.
            do (funcall writer chunk :close (null chunk))
	       (sleep 2)
	       (format t "Write is ~a~%" writer)
	       while chunk)))))
