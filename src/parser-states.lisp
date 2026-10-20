(in-package #:yaml-kit)

(defstruct (parser
            (:constructor make-parser%)
            (:copier nil)
            (:predicate nil))
           scanner state states handler directives version depth max-depth max-scalar-length)

(defmacro define-parser-state (name (parser &rest arguments) &body body)
  `(defun ,name (,parser ,@arguments)
     (declare (optimize (speed 3) (safety 1)))
     ,@body))

(defmacro parser-close-collection
    (parser token event-constructor end-span &optional (advance-p t))
  (let ((parser* (gensym "PARSER"))
        (token* (gensym "TOKEN"))
        (constructor* (gensym "EVENT-CONSTRUCTOR"))
        (end-span* (gensym "END-SPAN"))
        (advance-p* (gensym "ADVANCE-P")))
    `(let* ((,parser* ,parser)
            (,token* ,token)
            (,constructor* ,event-constructor)
            (,end-span* ,end-span)
            (,advance-p* ,advance-p))
       (when ,advance-p*
         (parser-next ,parser*))
       (parser-emit ,parser*
                    (funcall ,constructor*
                             :start-span (token-start-span ,token*)
                             :end-span ,end-span*))
       (decf (parser-depth ,parser*))
       (parser-pop ,parser*))))

(defmacro parser-push-node-state (parser continuations state)
  (let ((parser* (gensym "PARSER")))
    `(let ((,parser* ,parser))
       ,@(mapcar (lambda (continuation)
                   `(parser-push ,parser* ,continuation))
                 continuations)
       ,state)))

(defmacro parser-empty-scalar-state (parser start-span state)
  (let ((parser* (gensym "PARSER"))
        (start-span* (gensym "START-SPAN")))
    `(let* ((,parser* ,parser)
            (,start-span* ,start-span))
       (parser-empty-scalar ,parser* ,start-span*)
       ,state)))

(declaim (inline parser-peek parser-next parser-push parser-pop parser-emit))
(defun parser-peek (parser) (%scanner-peek-token (parser-scanner parser)))
(defun parser-next (parser) (%scanner-next-token (parser-scanner parser)))
(defun parser-push (parser state) (push state (parser-states parser)))
(defun parser-finish (parser)
  (declare (ignore parser))
  nil)
(defun parser-pop (parser)
  (or (pop (parser-states parser)) #'parser-finish))
(defun parser-emit (parser event)
  (funcall (parser-handler parser) event)
  nil)
(defun parser-error (token context)
  (let ((span (and token (token-start-span token))))
    (error 'yaml-parse-error
           :context context
           :diagnostic (cl-parser-kit:make-diagnostic :message context :span span))))
