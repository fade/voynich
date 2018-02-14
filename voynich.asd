;; -*-lisp-*-
;;;; voynich.asd

(defsystem #:voynich
  :description "A program to manipulate the encoding of the voynich manuscript to reflect Banasek's theory of the text as the phonetic description of a phonetic trade language based on Manchu."

  :version "0.3"
  :author "Brian O'Reilly <fade@deepsky.com>"
  :license "LLGPL"
  :serial t
  :depends-on (#:split-sequence
               #:alexandria
	       #:cl-ppcre
	       #:babel
	       #:yaclml)
  :pathname "./"
  :components ((:static-file "voynich.asd")
               (:file "app-utils")
	       (:module :src
			:components ((:file "packages")
				     (:file "voynich" :depends-on ("packages")))
			:serial t)))


