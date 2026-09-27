(in-package #:cl-yaml-kit/test)

(it "expands parser states and defines callable state functions"
  (let ((expansion
          (macroexpand-1
           '(yaml-kit::define-parser-state probe-parser-state (parser)
              (declare (ignore parser))
              :probe))))
    (expect (first expansion) :to-equal 'defun))
  (eval '(yaml-kit::define-parser-state probe-parser-state-eval (parser)
           (declare (ignore parser))
           :probe))
  (let ((state (find-symbol "PROBE-PARSER-STATE-EVAL" :cl-yaml-kit/test)))
    (expect (funcall (symbol-function state) nil) :to-equal :probe)))

(it "exercises parser state helpers with custom and scanner callbacks"
  (declare (notinline yaml-kit::parser-peek yaml-kit::parser-next
                      yaml-kit::parser-push yaml-kit::parser-pop
                      yaml-kit::parser-emit))
  (let* ((scanner (yaml-kit::make-scanner "x"))
         (events nil)
         (parser (yaml-kit::make-parser%
                  :scanner scanner :states nil :handler (lambda (event) (push event events))
                  :peek-function (lambda (s) (declare (ignore s)) :custom-peek)
                  :next-function (lambda (s) (declare (ignore s)) :custom-next))))
    (expect (yaml-kit::parser-peek parser) :to-equal :custom-peek)
    (expect (yaml-kit::parser-next parser) :to-equal :custom-next)
    (setf (yaml-kit::parser-peek-function parser) nil
          (yaml-kit::parser-next-function parser) nil)
    (expect (yaml-kit::parser-peek parser) :to-be-truthy)
    (expect (yaml-kit::parser-next parser) :to-be-truthy)
    (yaml-kit::parser-push parser #'identity)
    (expect (length (yaml-kit::parser-states parser)) :to-equal 1)
    (expect (funcall (yaml-kit::parser-pop parser) :value) :to-equal :value)
    (expect (funcall (yaml-kit::parser-pop parser) parser) :to-equal nil)
    (expect (yaml-kit::parser-emit parser :event) :to-equal nil)
    (expect events :to-equal '(:event))))

(describe "parser event details"
  (macrolet ((parser-event-cases (&body cases)
               `(progn
                  ,@(mapcar (lambda (case)
                              `(it ,(first case)
                                 (expect (parser-event-signature
                                          (nth 2 (yaml-kit:parse-events ,(second case))))
                                         :to-equal ',(third case))))
                            cases))))
    (parser-event-cases
      ("PARSER-FLOW-SEQUENCE-START-IMPLICIT" "[a]"
       (:sequence-start t :flow nil nil))
      ("PARSER-FLOW-MAPPING-START-IMPLICIT" "{a: b}"
       (:mapping-start t :flow nil nil))
      ("PARSER-FLOW-SEQUENCE-START-EXPLICIT-TAG" "!!seq [a]"
       (:sequence-start nil :flow nil "tag:yaml.org,2002:seq"))
      ("PARSER-FLOW-MAPPING-START-EXPLICIT-TAG" "!!map {a: b}"
       (:mapping-start nil :flow nil "tag:yaml.org,2002:map"))))
  (it "PARSER-EMPTY-ANCHOR-SCALAR-PRESERVES-PROPERTIES"
    (expect (parser-event-signature
             (nth 2 (parse-parser-token-events
                     '((:stream-start) (:anchor :value "a") (:stream-end)))))
            :to-equal '(:scalar "" "a" nil t nil)))
  (it "PARSER-FLOW-NODE-EXPECTED-CONTENT"
    (expect (handler-case
                (progn
                  (parse-parser-token-events
                   '((:stream-start) (:flow-sequence-start)
                     (:block-sequence-start) (:block-end)
                     (:flow-sequence-end) (:stream-end)))
                  nil)
            (yaml-kit:yaml-parse-error () t))
            :to-be-truthy)))
  (dolist (case '(("flow sequence key after entry" "[a, ? b]")
                  ("flow sequence empty mapping value" "[a:]")
                  ("flow sequence missing value indicator" "[? a]")
                  ("flow sequence empty mapping key" "[?]")))
    (destructuring-bind (name input) case
      (it name
        (expect (handler-case (yaml-kit:parse-events input)
                  (yaml-kit:yaml-parse-error () nil))
                :to-be-truthy))))
