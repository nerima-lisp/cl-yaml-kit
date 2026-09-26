;;;; src/emitter-state.lisp
(in-package #:yaml-kit)

(defstruct (emitter-context (:constructor %make-emitter-context))
  stream (indent 2 :type fixnum) (width 80 :type fixnum)
  (level 0 :type fixnum) (column 0 :type fixnum)
  (flow nil) (need-comma nil) (line-start t))

(defun make-emitter-context (stream indent width)
  (%make-emitter-context :stream stream :indent indent :width width))

(defun %emit-text (context text)
  (write-string text (emitter-context-stream context))
  (let ((last (and (plusp (length text)) (char text (1- (length text))))))
    (if (and last (char= last #\Newline))
        (setf (emitter-context-column context) 0
              (emitter-context-line-start context) t)
        (setf (emitter-context-line-start context) nil
              (emitter-context-column context)
              (+ (emitter-context-column context) (length text))))))

(defun %emit-newline (context)
  (%emit-text context (string #\Newline)))

(defun %emit-indent (context level)
  (unless (emitter-context-line-start context)
    (%emit-newline context))
  (%emit-text context (make-string (* level (emitter-context-indent context))
                                   :initial-element #\Space)))
