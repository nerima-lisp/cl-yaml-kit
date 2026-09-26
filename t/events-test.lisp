;;;; t/events-test.lisp
(in-package #:cl-yaml-kit/test)
(describe "event contract"
  (it "constructs a scalar event with marks"
    (let ((event (make-scalar-event
                  :start-mark (make-mark 1 2 3)
                  :end-mark (make-mark 1 4 5)
                  :value "hello")))
      (expect (scalar-event-p event) :to-be-truthy)
      (expect (scalar-event-value event) :to-equal "hello"))))
