#!/bin/bash
set -e


# change this var binding to change the location of the source in your
# home dir. if you change this you also need to change the location in
# voynich/xlate.lisp where the code looks for the translation matrix
# and other constants. I'll probably fix that so it's more abstract in
# future versions.
export TARG="SourceCode"

cd $HOME

mkdir -p $TARG/lisp && cd $TARG/lisp

git clone http://www.deepsky.com/~fade/voynich.git

cd &&


# this is an sbcl init file.

echo "(ignore-errors (require 'sb-aclrepl))

(when (find-package 'sb-aclrepl)
  (push :aclrepl cl:*features*))
#+aclrepl
(progn
  (setq sb-aclrepl:*max-history* 100)
  (setf (sb-aclrepl:alias \"asdc\")
	#'(lambda (sys) (asdf:operate 'asdf:compile-op sys)))
  (sb-aclrepl:alias \"l\" (sys) (asdf:operate 'asdf:load-op sys))
  (sb-aclrepl:alias \"t\" (sys) (asdf:operate 'asdf:test-op sys))
  ;; The 1 below means that two characaters (\"up\") are required
  (sb-aclrepl:alias (\"up\" 1 \"Use package\") (package) (use-package package))
  ;; The 0 below means only the first letter (\"r\") is required,
  ;; such as \":r base64\"
  (sb-aclrepl:alias (\"require\" 0 \"Require module\") (sys) (require sys))
  (setq cl:*features* (delete :aclrepl cl:*features*)))

(load \"/home/$USER/quicklisp/setup.lisp\")
(use-package :ql)
(loop for path in  '(#P\"/home/$USER/$TARG/lisp/voynich/\")
     :do (pushnew path asdf:*central-registry*))" > ~/.sbclrc


wget "http://quickstart.quicklisp.org/quicklisp.lisp"

sbcl --no-userinit --load ~/quicklisp.lisp

#(output-interlinear-file "/tmp/voynout" (make-line-objects *voyscript*))