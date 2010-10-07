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

(defvar *voytrans* #P "/home/fade/SourceCode/lisp/voynich/voyn_101/voytrans1.2.txt")
(defvar *voyscript* #P "/home/fade/SourceCode/lisp/voynich/voyn_101/voyn_101.txt")
(defvar *transtable* (make-hash-table :test 'equal))

(defun strip-string (string)
  (string-trim " " string))

(defun strip-spaces (string-list)
  (mapcar #'strip-string string-list))

(defun string->number (string)
  (parse-integer string :junk-allowed t))

(defun readvoy->codepoints (filespec)
  (with-open-file (s filespec :direction :input)
    (loop for x = (read-line s nil) while x
       for y = (let* ((n (split-sequence:split-sequence #\@ x))
		      (o (strip-spaces n)))
		 (if (>= (length o) 3)
		     (progn
		       (format t "&WHOLE:: ~A~%  0th=>~A 1st=>~A 2nd=>~A~%" o (elt o 0) (elt o 1) (elt o 2))
		       (cons (elt o 0) (elt o 2)))
		     nil))
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
  (let* ((indx (line-index line))
	 (rawl (raw-line line))
	 (xl (xlate rawl)))
    (declare (ignore indx))
    (setf (xline line) xl)))

(defun make-mline (text)
  (let* ((bl (breaktag text))
	 (idx (car bl))
	 (line (cdr bl)))
    (make-instance 'manuscript-line :index idx :rawline line)))

(defun breaktag (line)
  (if (stringp line)
      (let* ((breakup (cl-ppcre:split "(^\<.*\>)" line :with-registers-p t))
	     (rplist (cons (elt breakup 1) (strip-string (elt breakup 2)))))
	rplist)
      nil))

(defun make-line-objects (filespec)
  (with-open-file (s filespec :direction :input :external-format :latin-1)
    (loop for x = (read-line s nil) while x
       :collect (make-mline x)))) ; (xlate (cdr (breaktag x)))

(defun output-interlinear-file (filespec line-obj-list)
  "EX: (output-interlinear-file '/path/to/output' (make-line-objects *voyscript*))"
  (with-open-file (s filespec :direction :output :if-exists :supersede)
    (loop for obj in line-obj-list
       :do (format s "~A | ~A~%~A | ~A~%~%" (line-index obj) (raw-line obj) (line-index obj) (xline obj)))))

(defun print-interlinear-script (line-obj-list)
  (loop for obj in line-obj-list
     :do (format t "~A | ~A~%~A | ~A~%~%" (line-index obj) (raw-line obj) (line-index obj) (xline obj))))

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
		      (if (listp out)
			  (format string "~{~A~}" (mapcar #'tochar out)) ;(code-char out)
			  (format string "~A" (code-char out))) ;(code-char out)
		      (format string "~A" e))) ;(char-code e)
       :finally (return string))))


(defun load-table ()
  "load the hash table *transtable* with the translation matrix we plan to use."
  (loop for i from 1
     for (ascii . utf8) in (readvoy->codepoints *voytrans*)
     ;for stripped = (strip-spaces )
     do (let ((a ascii)
	      (u utf8))
	  (format t "======[ ~D ]======~%" i)
	  (format t "~A~%" a)
	  (format t "~A~%" u)
	  (force-output)
	  (if (and u (>= (length u) 5))
	      (setf u (split-sequence #\, u)))
	  (if (and (listp u) (>= (length u) 2))
	      (setf (gethash a *transtable*) (mapcar #'string->number u))
	      (setf (gethash a *transtable*) (string->number u))
	      ) ; (format t "~S ||  ~{ ~S ~}~%" a (mapcar #'parse-integer u))
	  )))



(defun print-hash-entry (key value)
  "dump out the key/values contained in a hashtable from maphash."
  (format t "The value associated with the key ~S is ~S~%" key value))

(defun dump-matrix ()
  "show the ascii->utf8 map in *transtable*. This should make
debugging the xlation matrix a little clearer."
  (maphash #'print-hash-entry *transtable*))