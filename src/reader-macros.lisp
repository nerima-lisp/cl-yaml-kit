;;;; src/reader-macros.lisp
(in-package #:yaml-kit)

(defmacro with-reader-speed (&body body)
  `(locally (declare (optimize (speed 3) (safety 1) (debug 0))) ,@body))

(defmacro define-yaml-production (name characters)
  "Define a compile-time ASCII character production lookup table."
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
         (bits (make-array 128 :element-type '(unsigned-byte 8)
                           :initial-element 0)))
    (loop for character across characters
          for code = (char-code character)
          do (progn
               (unless (< code 128)
                 (error "YAML production ~A contains non-ASCII character ~S"
                        name character))
               (setf (aref bits code) 1)))
    `(progn
       (defparameter ,table-name
         (make-array 128 :element-type '(unsigned-byte 8)
                     :initial-contents ',(coerce bits 'list)))
       (declaim (type (simple-array (unsigned-byte 8) (128)) ,table-name))
       (declaim (inline ,predicate-name))
       (defun ,predicate-name (character)
         (and (characterp character)
              (< (char-code character) 128)
              (= 1 (aref ,table-name (char-code character)))))
       ',predicate-name)))

(defmacro reader-char (string index)
  `(char ,string ,index))
