;;;; src/reader-macros.lisp
(in-package #:yaml-kit)

(defmacro with-reader-speed (&body body)
  `(locally (declare (optimize (speed 3) (safety 1) (debug 0))) ,@body))

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
