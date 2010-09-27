(in-package :cl-user)

(cl:defpackage #:voynich-user
  (:nicknames :vuser)
  (:use :cl :split-sequence
	:cl-ppcre
	:babel)
  (:export
   ;; #:*voytrans*
   ;; #:*voyscript*
   ;; #:*transtable*
   #:manuscript-line
   #:make-mline
   #:breaktag
   #:make-line-objects
   #:output-interlinear-file
   #:load-table))

