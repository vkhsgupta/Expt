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

;; Oringinal by svetlyak40wt.  Works.
(defun events-handler-1 (env)
  (declare (ignorable env))
  (let ((output (getf env :raw-body)))
    (lambda (handle-normal-response)
      (declare (ignorable handle-normal-response))
      (format output "200 OK~C~C" #\return #\newline)
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
(defun events-handler (env)
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
(defun events-handler (env)
  (declare (ignorable env))
  (lambda (responder)
    (let ((writer (funcall responder '(200 (:content-type "text/event-stream")))))
      (ignore-errors
       (funcall writer (format nil "~%") :close nil)
       (loop for chunk = (format nil "data: Event~%~%") ;; Need two ~%. Fewer fails.
            do (funcall writer chunk :close (null chunk))
	       (sleep 2)
	       while chunk)))))



(defun make-app ()
  (lack:builder
   (:mount "/events"
           'events-handler)
   'main-app))


(defvar *server* nil)


(defun start-server (&key (port 8080))
  (when *server*
    (error "Server already started"))
  (setf *server*
        (clack:clackup (make-app)
                       :port port)))


(defun stop-server ()
  (when *server*
    (clack:stop *server*)
    (setf *server*
          nil)))

(start-server)
