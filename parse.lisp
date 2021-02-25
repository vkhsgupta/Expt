(ql:quickload :fast-http)
(ql:quickload :flexi-streams)

(defpackage my-parser
  (:use :cl :fast-http :flexi-streams))

(in-package my-parser)

(defun first-line-cb ()
  (format nil "First line has been parsed ~%"))

(defun headers-cb ()
  (format nil "Headers have been parsed ~a"))

(defun body-cb ()
  (format nil "Body has been parsed ~a"))

(defun finish-cb ()
  (format t "Have done the Job successfully ~a"))


(defparameter aparser (make-parser (make-http-request)
				   :first-line-callback #'first-line-cb
				   :header-callback #'headers-cb
				   :body-callback #'body-cb
				   :finish-callback #'finish-cb))


(defparameter p nil)

(setf p (funcall aparser (string-to-octets "GET / HTTP/1.1")))

p

(string-to-octets "hello") 
		   
