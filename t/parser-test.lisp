(in-package #:cl-yaml-kit/test)

(defstruct parser-test-scanner tokens)

(defun parser-test-peek (scanner)
  (car (parser-test-scanner-tokens scanner)))

(defun parser-test-next (scanner)
  (pop (parser-test-scanner-tokens scanner)))

(defmacro parser-token-cases (&body cases)
  `(progn
     ,@(mapcar
        (lambda (case)
          `(it ,(first case)
             (let* ((mark (yaml-kit:make-mark 1 1 0))
                    (scanner (make-parser-test-scanner
                              :tokens (mapcar (lambda (spec)
                                                (apply #'yaml-kit:make-token
                                                       (first spec) mark mark (rest spec)))
                                              ',(second case))))
                    (events nil)
                    (parser (yaml-kit::make-parser%
                             :scanner scanner
                             :state #'yaml-kit::yaml-parser-parse-stream-start
                             :handler (lambda (event) (push event events))
                             :depth 0 :max-depth 256 :max-scalar-length 1000)))
               (let ((old-peek (symbol-function 'yaml-kit:scanner-peek-token))
                     (old-next (symbol-function 'yaml-kit:scanner-next-token)))
                 (unwind-protect
                      (progn
                        (setf (symbol-function 'yaml-kit:scanner-peek-token) #'parser-test-peek
                              (symbol-function 'yaml-kit:scanner-next-token) #'parser-test-next)
                        (loop for state = (yaml-kit::parser-state parser) while state do
                          (setf (yaml-kit::parser-state parser) (funcall state parser))))
                   (setf (symbol-function 'yaml-kit:scanner-peek-token) old-peek
                         (symbol-function 'yaml-kit:scanner-next-token) old-next)))
               (expect (mapcar #'type-of (nreverse events)) :to-equal ',(third case)))))
        cases)))

(parser-token-cases
  ("parses a scalar token stream"
   ((:stream-start) (:scalar :value "hello" :style :plain) (:stream-end))
   (yaml-kit:stream-start-event yaml-kit:document-start-event
    yaml-kit:scalar-event yaml-kit:document-end-event yaml-kit:stream-end-event))
  ("parses an indentless sequence as a mapping value"
   ((:stream-start) (:block-mapping-start) (:key) (:scalar :value "a" :style :plain)
    (:value) (:block-entry) (:scalar :value "b" :style :plain) (:block-end)
    (:block-end) (:stream-end))
   (yaml-kit:stream-start-event yaml-kit:document-start-event
    yaml-kit:mapping-start-event yaml-kit:scalar-event
    yaml-kit:sequence-start-event yaml-kit:scalar-event
    yaml-kit:sequence-end-event yaml-kit:mapping-end-event
    yaml-kit:document-end-event yaml-kit:stream-end-event)))
