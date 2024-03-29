;; -*-lisp-*-
;;;; voynich.asd

(defsystem #:voynich
  :description "A program to manipulate the encoding of the voynich manuscript to reflect Banasek's theory of the text as the phonetic description of a trade language based on Manchu."

  :version "0.3"
  :author "Brian O'Reilly <fade@deepsky.com>"
  :license "GPL v3 or later."
  :serial t
  :depends-on (#:split-sequence
               #:alexandria
	       #:cl-ppcre
	       #:babel
	       #:yaclml
               ;; #:net.didierverna.clon
               )
  :pathname "./"
  :components ((:static-file "voynich.asd")
               (:file "app-utils")
	       (:module :src
		:components ((:file "packages")
			     (:file "voynich" :depends-on ("packages")))
		:serial t)))


