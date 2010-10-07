#!/bin/bash
set -e

cd $HOME

mkdir -p SourceCode/lisp && cd SourceCode/lisp

git clone http://www.deepsky.com/~fade/voynich.git

cd &&


# this is an sbcl init file. replace $(USER) with your uid, as lisp
# doesn't understand vars in shell syntax.

echo "(ignore-errors (require 'sb-aclrepl))

(when (find-package 'sb-aclrepl)
  (push :aclrepl cl:*features*))
#+aclrepl
(progn
  (setq sb-aclrepl:*max-history* 100)
  (setf (sb-aclrepl:alias "asdc")
	#'(lambda (sys) (asdf:operate 'asdf:compile-op sys)))
  (sb-aclrepl:alias "l" (sys) (asdf:operate 'asdf:load-op sys))
  (sb-aclrepl:alias "t" (sys) (asdf:operate 'asdf:test-op sys))
  ;; The 1 below means that two characaters ("up") are required
  (sb-aclrepl:alias ("up" 1 "Use package") (package) (use-package package))
  ;; The 0 below means only the first letter ("r") is required,
  ;; such as ":r base64"
  (sb-aclrepl:alias ("require" 0 "Require module") (sys) (require sys))
  (setq cl:*features* (delete :aclrepl cl:*features*)))

(load "/home/$(USER)/quicklisp/setup.lisp")
(use-package :ql)
(loop for path in  '(#P"/home/$(USER)/SourceCode/lisp/voynich/")
     :do (pushnew path asdf:*central-registry*))" > ~/.sbclrc


wget "http://quickstart.quicklisp.org/quicklisp.lisp"

sbcl --no-user-init --load ~/quicklisp.lisp

#(output-interlinear-file "/tmp/voynout" (make-line-objects *voyscript*))