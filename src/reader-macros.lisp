;;;; src/reader-macros.lisp
(in-package #:yaml-kit)

(defmacro with-reader-speed (&body body)
  `(locally (declare (optimize (speed 3) (safety 1) (debug 0))) ,@body))

(defmacro define-yaml-production (name characters)
  "Define a YAML character production and its inline predicate.

NAME is the production suffix, such as INDICATOR.  CHARACTERS is a
string or a list of characters.  The table is built while this form is
expanded, so loading the generated code does not need to scan the
production string."
  (let* ((suffix (string-upcase (string name)))
         (package (symbol-package 'define-yaml-production))
         (table-name (intern (format nil "+YAML-~A-BITS+" suffix) package))
         (predicate-name (intern (format nil "YAML-~A-P" suffix) package))
         (characters (if (and (consp characters)
                              (eq (first characters) 'quote))
                         (second characters)
                         characters))
         (characters (etypecase characters
                       (string characters)
                       (list (coerce characters 'string))))
         (bytes (make-array 128 :element-type '(unsigned-byte 8)
                            :initial-element 0)))
    (loop for character across characters
          for code = (char-code character)
          do (unless (< code 128)
               (error "YAML production ~A contains non-ASCII character ~S"
                      name character))
             (setf (aref bytes code) 1))
    `(progn
       (defparameter ,table-name
         (make-array 128 :element-type '(unsigned-byte 8)
                     :initial-contents ',(coerce bytes 'list)))
       (declaim (type (simple-array (unsigned-byte 8) (128)) ,table-name))
       (declaim (inline ,predicate-name))
       (defmacro ,predicate-name (character)
         `(and (< (char-code ,character) 128)
               (= 1 (aref ,',table-name (char-code ,character)))))
       ',predicate-name)))

(defmacro reader-char (string index)
  `(char ,string ,index))

(defmacro reader-whitespace-p (character)
  `(or (char= ,character #\Space) (char= ,character #\Tab)))

(defmacro reader-character-at-p (state offset character)
  `(char= (or (reader-peek ,state ,offset) #\Null) ,character))

(defmacro reader-line-boundary-p (state offset)
  `(or (null (reader-peek ,state ,offset))
       (yaml-line-break-p (reader-peek ,state ,offset))
       (reader-whitespace-p (reader-peek ,state ,offset))
       (char= (reader-peek ,state ,offset) #\#)))

(defmacro reader-line-start-p (state)
  `(zerop (reader-current-indent ,state)))
