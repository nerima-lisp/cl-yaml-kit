;;;; src/emitter-state.lisp
(in-package #:yaml-kit)

(defstruct (emitter-context (:constructor %make-emitter-context))
  stream (indent 2 :type fixnum) (column 0 :type fixnum)
  (line-start t))

(defun make-emitter-context (stream indent)
  (%make-emitter-context :stream stream :indent indent))

(defun %emit-text (context text)
  (write-string text (emitter-context-stream context))
  (let* ((length (length text))
         (newline (position #\Newline text :from-end t)))
    (if newline
        (setf (emitter-context-column context) (- length newline 1)
              (emitter-context-line-start context) (= newline (1- length)))
        (setf (emitter-context-line-start context) nil
              (emitter-context-column context)
              (+ (emitter-context-column context) length)))))

(declaim (inline %emit-char))
(defun %emit-char (context character)
  (write-char character (emitter-context-stream context))
  (if (char= character #\Newline)
      (setf (emitter-context-column context) 0
            (emitter-context-line-start context) t)
      (setf (emitter-context-column context)
            (1+ (emitter-context-column context))
            (emitter-context-line-start context) nil)))

(defun %emit-newline (context)
  (%emit-text context (string #\Newline)))

(defun %emit-indent (context level)
  (unless (emitter-context-line-start context)
    (%emit-newline context))
  (%emit-text context (make-string (* level (emitter-context-indent context))
                                   :initial-element #\Space)))
