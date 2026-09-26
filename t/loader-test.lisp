;;;; t/loader-test.lisp
(in-package #:cl-yaml-kit/test)

(defun loader-event (type &rest options)
  (apply (ecase type
           (:stream-start #'yaml-kit:make-stream-start-event)
           (:stream-end #'yaml-kit:make-stream-end-event)
           (:document-start #'yaml-kit:make-document-start-event)
           (:document-end #'yaml-kit:make-document-end-event)
           (:scalar #'yaml-kit:make-scalar-event)
           (:sequence-start #'yaml-kit:make-sequence-start-event)
           (:sequence-end #'yaml-kit:make-sequence-end-event)
           (:mapping-start #'yaml-kit:make-mapping-start-event)
           (:mapping-end #'yaml-kit:make-mapping-end-event)
           (:alias #'yaml-kit:make-alias-event))
         options))

(describe "loader"
  (it "constructs a scalar from events"
    (let ((value (yaml-kit:parse
                  (list (loader-event :stream-start)
                        (loader-event :document-start)
                        (loader-event :scalar :value "42")
                        (loader-event :document-end)
                        (loader-event :stream-end)))))
      (expect value :to-equal 42)))
  (it "preserves aliases as shared nodes during composition"
    (let* ((events (list (loader-event :stream-start)
                         (loader-event :document-start)
                         (loader-event :mapping-start)
                         (loader-event :scalar :value "left")
                         (loader-event :sequence-start :anchor "a")
                         (loader-event :scalar :value "x")
                         (loader-event :sequence-end)
                         (loader-event :scalar :value "right")
                         (loader-event :alias :anchor "a")
                         (loader-event :mapping-end)
                         (loader-event :document-end)
                         (loader-event :stream-end)))
           (node (yaml-kit:compose events)))
      (expect (yaml-kit:mapping-node-p node) :to-be-truthy)
      (expect (eq (yaml-kit:sequence-node-items
                   (cdr (first (yaml-kit:mapping-node-pairs node))))
                  (yaml-kit:sequence-node-items
                   (cdr (second (yaml-kit:mapping-node-pairs node)))))
              :to-be-truthy))))
