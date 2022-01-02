#!/bin/bash
set -e
set -x

# change this var binding to change the location of the source in your
# home dir. if you change this you also need to change the location in
# voynich/voynich.lisp where the code looks for the translation matrix
# and other constants. I'll probably fix that so it's more abstract in
# future versions.
export TARG="$HOME/SourceCode"

cd $HOME

mkdir -p "$TARG/lisp" && cd "$TARG/lisp"

if [[ -d "$TARG/lisp/voynich" ]];
then
  cd "$TARG/lisp/voynich"
  git checkout master &&
  git fetch --all &&
  git pull origin master
else
  git clone https://github.com/fade/voynich.git
fi


cd &&

# echo -n "Installing Roswell... "
# curl -L https://raw.githubusercontent.com/roswell/roswell/release/scripts/install-for-ci.sh | sh
# echo "[Done]"

mkdir -p ~/.config/common-lisp/

cat <<EOF > ~/.config/common-lisp/source-registry.conf 
;; -*-lisp-*-
(:source-registry
 (:tree (:home "SourceCode/lisp"))
 :inherit-configuration) 
EOF

# this is an sbcl init file.

cat <<EOF > ~/.sbclrc
;; -*-lisp-*-
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (format t "~2&Loading/Using quicklisp, slynk etc....~2%")
    (load quicklisp-init)))

(use-package :ql)

EOF

rm -rf ~/quicklisp.lisp
wget "http://beta.quicklisp.org/quicklisp.lisp"
sbcl --no-userinit --load ~/quicklisp.lisp


#(output-interlinear-file "/tmp/voynout" (make-line-objects *voyscript*))
