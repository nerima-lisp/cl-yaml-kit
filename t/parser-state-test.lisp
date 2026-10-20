(in-package #:cl-yaml-kit/test)

(it "expands parser states and defines callable state functions"
    (let ((expansion
           (macroexpand-1
            '(yaml-kit::define-parser-state probe-parser-state
                                            (parser)
                                            (declare (ignore parser))
                                            :probe))))
      (expect (first expansion) :to-equal 'defun))
    (eval
     '(yaml-kit::define-parser-state probe-parser-state-eval
                                     (parser)
                                     (declare (ignore parser))
                                     :probe))
    (let ((state (find-symbol "PROBE-PARSER-STATE-EVAL" :cl-yaml-kit/test)))
      (expect (funcall (symbol-function state) nil) :to-equal :probe)))
(it "expands parser continuations as successive pushes"
  (let ((body
          (cddr
           (macroexpand-1
            '(yaml-kit::parser-push-node-state parser
              (#'first-state #'second-state)
              #'next-state)))))
    (expect (mapcar #'car (subseq body 0 2))
            :to-equal
            '(yaml-kit::parser-push yaml-kit::parser-push))
    (expect (mapcar #'third (subseq body 0 2))
            :to-equal
            '((function first-state) (function second-state)))
    (expect (car (last body)) :to-equal '(function next-state))))
(it "uses scanner input for parser peek and next"
    (let* ((scanner (yaml-kit::make-scanner "x"))
           (parser (yaml-kit::make-parser% :scanner scanner :states nil)))
      (expect (yaml-kit::token-kind (yaml-kit::parser-peek parser))
              :to-equal
              :stream-start)
      (expect (yaml-kit::token-kind (yaml-kit::parser-next parser))
              :to-equal
              :stream-start)
      (expect (yaml-kit::token-kind (yaml-kit::parser-peek parser))
              :to-equal
              :scalar)))

(it "pushes and pops parser continuations"
    (let ((parser (yaml-kit::make-parser% :states nil)))
      (yaml-kit::parser-push parser #'identity)
      (expect (length (yaml-kit::parser-states parser)) :to-equal 1)
      (expect (funcall (yaml-kit::parser-pop parser) :value) :to-equal :value)
      (expect (funcall (yaml-kit::parser-pop parser) parser) :to-equal nil)))

(it "emits events through the configured handler"
    (let* ((events nil)
           (parser
            (yaml-kit::make-parser% :handler
                                    (lambda (event)
                                      (push event events)))))
      (expect (yaml-kit::parser-emit parser :event) :to-equal nil)
      (expect events :to-equal '(:event))
      (expect (mapcar #'type-of (yaml-kit:parse-events "x"))
              :to-equal
              '(yaml-kit:stream-start-event yaml-kit:document-start-event
                                            yaml-kit:scalar-event
                                            yaml-kit:document-end-event
                                            yaml-kit:stream-end-event))))

(cl-weave:it-each
 (("PARSER-FLOW-SEQUENCE-START-IMPLICIT" "[a]"
                                         (:sequence-start t :flow nil nil))
  ("PARSER-FLOW-MAPPING-START-IMPLICIT" "{a: b}"
                                        (:mapping-start t :flow nil nil))
  ("PARSER-FLOW-SEQUENCE-START-EXPLICIT-TAG" "!!seq [a]"
                                             (:sequence-start nil
                                                              :flow
                                                              nil
                                                              "tag:yaml.org,2002:seq"))
  ("PARSER-FLOW-MAPPING-START-EXPLICIT-TAG" "!!map {a: b}"
                                            (:mapping-start nil
                                                            :flow
                                                            nil
                                                            "tag:yaml.org,2002:map")))
 "parser event detail case ~S"
 (name input expected)
 (declare (ignore name))
 (expect (parser-event-signature (nth 2 (yaml-kit:parse-events input)))
         :to-equal
         expected))

(it "PARSER-EMPTY-ANCHOR-SCALAR-PRESERVES-PROPERTIES"
    (expect (parser-event-signature (nth 2 (yaml-kit:parse-events "&a")))
            :to-equal
            '(:scalar "" "a" nil t nil)))

(it "PARSER-FLOW-NODE-EXPECTED-CONTENT"
    (expect
     (lambda ()
       (yaml-kit:parse-events "[-]"))
     :to-signal-parser-error
     (list 'yaml-kit:yaml-parse-error "did not find expected node content")))

(cl-weave:it-each
 (("flow sequence key after entry" "[a, ? b]")
  ("flow sequence empty mapping value" "[a:]")
  ("flow sequence missing value indicator" "[? a]")
  ("flow sequence empty mapping key" "[?]"))
 "accepts flow sequence case ~S"
 (name input)
 (declare (ignore name))
 (expect (yaml-kit:parse-events input) :to-be-truthy))

