(defsystem "voynich"
  :description "Some simple utilities to xlate voynich interlinear ts to codepoints in the utf-8 register."
  :version "0.2"
  :author "Brian O'Reilly <fade@deepsky.com>"
  :license "LLGPL"
  :serial t
  :depends-on (#:split-sequence
	       #:cl-ppcre
	       #:babel
	       #:yaclml)
  :components ((:static-file "voynich.asd")
	       (:module :src
			:components ((:file "packages")
				     (:file "constants")
				     (:file "xlator" :depends-on ("packages")))
			:serial t)))
