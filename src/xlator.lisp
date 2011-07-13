(in-package :voynich-user)

"The basic assumption here is that the interlinear encoding of the
voynich manuscript is a representation of a phonetic writing system
indigenous to north eastern china, called Manchu. In the brave old
world of the 21st century we can represent this in the more expressive
IPA alphabet, or in Manchu itself, which both have codepoints in the
UTF8 unicode encoding. This is a raw naive translator, where we
literally swap the ascii encoding for the unicode encoding. Its
usefulness will be determined later by people who understand human
languages a lot better than me. -BCJO"

(defvar *tbase* (merge-pathnames "SourceCode/lisp/voynich/voyn_101/" (user-homedir-pathname)))

(defvar *voytrans* (merge-pathnames "voytrans1.23.txt" *tbase*))

(defvar *voygroup* (merge-pathnames "voygroup_UTF-8.txt" *tbase*))

(defvar *voyscript* (merge-pathnames "voyn_101.txt" *tbase*))

(defvar *transtable* (make-hash-table :test 'equal))

(defvar *tloaded* nil)

(defun strip-string (string)
  (string-trim " ^Z" string))

(defun strip-spaces (string-list)
  (mapcar #'strip-string string-list))

(defun string->number (string)
  (parse-integer string :junk-allowed t))

(defun readvoy->codepoints (filespec)
  (with-open-file (s filespec :direction :input)
    (loop for x = (read-line s nil) while x
       for y = (let* ((n (split-sequence:split-sequence #\@ x))
		      (o (strip-spaces n))
		      (fcount 0))
		 (if (>= (length o) 3)
		     (progn
		       (format t "&WHOLE:: ~A~%  0th=>~A 1st=>~A 2nd=>~A~%" o (elt o 0) (elt o 1) (elt o 2))
		       (cons (elt o 0) (elt o 2)))
		     (progn
		       (format t "BUMMER, Skipping:: ~A" o)
		       (incf fcount)
		       (values nil fcount))))
       if y collect y)))

(defclass manuscript-line ()
  ((index
    :initarg :index :accessor line-index)
   (rawline
    :initarg :rawline :accessor raw-line)
   (xline
    :initarg :xline :accessor xline)))

(defmethod initialize-instance :after ((line manuscript-line) &key)
  "set the xline slot in the manuscript-line class after we initialise the
   instance with the values from the input manuscript."
  (let* ((rawl (raw-line line))
	 (xl (xlate rawl)))
    (setf (xline line) xl)))

(defclass voygroup ()
  ((index
    :initarg :index :accessor gindex)
   (voytext
    :initarg :voytext :accessor voytext)
   (xtext
    :initarg :xtext :accessor xtext)))

(defmethod initialize-instance :after ((group voygroup) &key)
  "set the xtext slot of the voygroup class for this instance with the
   tranlated representation of the group encoding from the voytext101
   format"
  ;; (format t "nilly willy, ~A." (voytext group))
  (let* ((rawg (voytext group))
	 (xl (xlate rawg)))
    ;; (format t "index: ~A raw: ~A xlat: ~A~%" (gindex group) rawg xl)
    (setf (xtext group) xl)))

(defun breaktag (line)
  (if (stringp line)
      (let* ((breakup (cl-ppcre:split "(^\<.*\>)" line :with-registers-p t))
	     (rplist (cons (elt breakup 1) (strip-string (elt breakup 2)))))
	rplist)
      nil))

(defun make-mline (text)
  "return a filled instance of the manuscript-line class."
  (let* ((bl (breaktag text))
	 (idx (car bl))
	 (line (cdr bl)))
    (make-instance 'manuscript-line :index idx :rawline line)))

;; (defun breakgroup (line)
;;   (if (stringp line)
;;       (let* ((breakup (cl-ppcre:split "(^.*\,.*)" line :with-registers-p t))
;; 	     ()))))

(defun make-line-objects (filespec)
  "return a list of manuscript-line objects representing each line of
   the voyn_101 text located by filespec"
  (with-open-file (s filespec :direction :input :external-format :latin-1)
    (loop for x = (read-line s nil) while x
       :collect (make-mline x)))) ; (xlate (cdr (breaktag x)))

(defun make-mgroup (text)
  "return a filled instance of the voygroup class"
  (if (and (stringp text)
	   (not (zerop (length text))))
      (let* ((it (strip-string text))
	     (breakup (cl-ppcre:split  "(\,)" it :with-registers-p t))
	     (idx (car (last breakup)))
	     (group (elt breakup 0)))
	(if (>= (length breakup) 3)
	    (progn
	      (format t "~D || ~A~%" (length breakup) breakup)
	      (make-instance 'voygroup :index idx :voytext group))))
      (format t "BARK:: ~A~%" text)))

(defun make-group-objects (filespec)
  "return a list of voygroup objects representing each group entry in
   the voygroup glossary file located by filespec."
  (with-open-file (s filespec :direction :input :external-format :utf-8)
    (loop for x = (read-line s nil) while x
       if (> (length x) 0)
       ;; :do (format t "#<~A ~A ~A>~%" "stinger:: " x (type-of x))
       :collect (make-mgroup x))))

(defun output-interlinear-file (filespec line-obj-list)
  "EX: (output-interlinear-file '/path/to/output' (make-line-objects *voyscript*))"
  (with-open-file (s filespec :direction :output :if-exists :supersede)
    (loop for obj in line-obj-list
       :do (format s "~9A | ~A~%~9A | ~A~%~%" (line-index obj) (raw-line obj) (line-index obj) (xline obj)))))

(defun print-interlinear-script (line-obj-list)
  (loop for obj in line-obj-list
     :do (format t "~9A | ~A~%~9A | ~A~%~%" (line-index obj) (raw-line obj) (line-index obj) (xline obj))))

(defun tochar (code)
  "saves no work, but is clearer than code-char"
  (code-char code))

(defun tocode (char)
  "saves no work, but is clearer than char-code"
  (char-code char))

(defun xlate (line)
  (with-output-to-string (string)
    (loop for e across line
	    for key = (format nil "~S" (char-code e))
	    for out = (gethash key *transtable*)
	    :do (progn
		  (if out
		      (if (listp out) ; if we have a translation point, apply it
			  (format string "~{~A~}" (mapcar #'tochar out)) ; compound xlation
			  (format string "~A" (tochar out))) ; single char xlation
		      (format string "~A" e))) ; otherwise just output the original character.
       :finally (return string))))


(defun load-table ()
  "load the hash table *transtable* with the translation matrix we plan to use."
  (loop for i from 1
     for (ascii . utf8) in (readvoy->codepoints *voytrans*)
     ;for stripped = (strip-spaces )
     do (let ((a ascii)
	      (u utf8))
	  (format t "======[ CodePoint: ~D ]======~%" i)
	  (format t "~A~%" a)
	  (format t "~A~%" u)
	  (force-output)
	  (if (and u (>= (length u) 5)) ;; this codepoint maps to more than one unicode char.
	      (progn
		(format t "[[ ~A ]]~%" u)
		(setf u (split-sequence #\, u))))
	  (if (and (listp u) (>= (length u) 2))
	      (setf (gethash a *transtable*) (mapcar #'string->number u))
	      (setf (gethash a *transtable*) (string->number u))
	      ) ; (format t "~S ||  ~{ ~S ~}~%" a (mapcar #'parse-integer u))
	  ))
  (setf *tloaded* t)
  *tloaded*)

(defun run-object-list (objlist &optional (stream t))
  (loop for k from 1
     for i in objlist do
       (format stream "==[~D]~%~A~%~A~%" k i (describe i))))

(defun print-hash-entry (key value)
  "dump out the key/values contained in a hashtable from maphash."
  (format t "The value associated with the key ~S is ~S~%" key value))

(defun dump-matrix ()
  "show the ascii->utf8 map in *transtable*. This should make
debugging the xlation matrix a little clearer."
  (maphash #'print-hash-entry *transtable*))

(defun output-voygroup-file (filespec group-obj-list)
  "Emit a file to a stream opened on filespec containing a csv tripple
  for every line in the voygroup.txt format file."
  (cond
    (*tloaded*
     (with-open-file (s filespec :direction :output :if-exists :supersede)
       (loop for k from 0
	  for obj in group-obj-list
	  :do
	  (if obj
	      (progn
		(format t "[~D] ~A || ~A,~A,~A~%" k obj (voytext obj) (xtext obj) (gindex obj))
		(format s "~A,~A,~A~%" (voytext obj) (xtext obj) (gindex obj)))))))
    (t (error "no sex in a translation table."))))

(defun run-this-gloss-function (&key (targ "/tmp/voybar.baz"))
  "this function will output a gonkulated glossary stub in
   /tmp/voybar.baz unless it is given a different path at its callsite."
  (output-voygroup-file targ (make-group-objects *voygroup*)))

(defun run-this-gonk-function (&key (targ "/tmp/voynich-interlinear.gonk"))
  "this function will output a gonkulated xlation of the voynich
   interlinear file pointed to by *voyscript*"
  (output-interlinear-file targ (make-line-objects *voyscript*)))