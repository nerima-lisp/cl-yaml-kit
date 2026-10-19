(in-package #:yaml-kit)

(defun %make-yaml-character-bits (characters)
  (let ((bits (make-array 128 :element-type 'bit :initial-element 0)))
    (dolist (character (if (stringp characters)
                           (coerce characters 'list)
                           characters))
      (setf (sbit bits (char-code character)) 1))
    bits))

(defparameter *yaml-indicator-bits*
  (%make-yaml-character-bits
   (concatenate 'string "-?:,[]{}#&*!|>'\"%@" (string (code-char 96)))))

(defparameter *yaml-flow-indicator-bits*
  (%make-yaml-character-bits "[],{}"))

(defparameter *yaml-line-break-bits*
  (%make-yaml-character-bits (list #\Newline #\Return)))

(defparameter *yaml-whitespace-bits*
  (%make-yaml-character-bits (list #\Space #\Tab)))
