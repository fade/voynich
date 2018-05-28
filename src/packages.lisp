(in-package :cl-user)

(cl:defpackage #:voynich
  (:nicknames :vuser)
  (:use
   :cl
   :split-sequence
   :cl-ppcre
   :voynich.app-utils
   :babel
   :yaclml
   :net.didierverna.clon)
  (:export
   ;; #:*voytrans*
   ;; #:*voyscript*
   ;; #:*transtable*
   #:-main
   #:manuscript-line
   #:make-mline
   #:breaktag
   #:make-line-objects
   #:output-interlinear-file
   #:load-table))

