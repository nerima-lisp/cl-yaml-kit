;;;; t/events-test.lisp
(in-package #:cl-yaml-kit/test)
(defparameter *invalid-event-slot-value* -1)

(progn
  (defun test-point-span (line column offset)
    (cl-parser-kit:make-span
     :source nil :start offset :end offset
     :start-line (1+ line) :start-column (1+ column)
     :end-line (1+ line) :end-column (1+ column)))
  (defun contract-value-equal-p (actual expected)
    (if (and (typep actual 'cl-parser-kit:span)
             (typep expected 'cl-parser-kit:span))
        (and (equal (cl-parser-kit:span-source actual)
                    (cl-parser-kit:span-source expected))
             (= (cl-parser-kit:span-start actual)
                (cl-parser-kit:span-start expected))
             (= (cl-parser-kit:span-end actual)
                (cl-parser-kit:span-end expected))
             (= (cl-parser-kit:span-start-line actual)
                (cl-parser-kit:span-start-line expected))
             (= (cl-parser-kit:span-start-column actual)
                (cl-parser-kit:span-start-column expected))
             (= (cl-parser-kit:span-end-line actual)
                (cl-parser-kit:span-end-line expected))
             (= (cl-parser-kit:span-end-column actual)
                (cl-parser-kit:span-end-column expected)))
        (equal actual expected))))

(it "constructs a span and exposes its point coordinates"
  (let ((span (test-point-span 11 12 13)))
    (expect (cl-parser-kit:span-start-line span) :to-equal 12)
    (expect (cl-parser-kit:span-start-column span) :to-equal 13)
    (expect (cl-parser-kit:span-start span) :to-equal 13)))

(progn
  (it "uses non-scalar event defaults"
    (let ((stream-start (yaml-kit:make-stream-start-event))
          (stream-end (yaml-kit:make-stream-end-event))
          (document-start (yaml-kit:make-document-start-event))
          (document-end (yaml-kit:make-document-end-event))
          (sequence-start (yaml-kit:make-sequence-start-event))
          (sequence-end (yaml-kit:make-sequence-end-event))
          (mapping-start (yaml-kit:make-mapping-start-event))
          (mapping-end (yaml-kit:make-mapping-end-event))
          (alias (yaml-kit:make-alias-event)))
      (expect
       (list (list (yaml-kit:event-start-span stream-start)
                   (yaml-kit:event-end-span stream-start))
             (list (yaml-kit:event-start-span stream-end)
                   (yaml-kit:event-end-span stream-end))
             (list (yaml-kit:event-start-span document-start)
                   (yaml-kit:event-end-span document-start)
                   (yaml-kit:document-start-event-explicit-p document-start)
                   (yaml-kit:document-start-event-version document-start)
                   (yaml-kit:document-start-event-tag-directives document-start))
             (list (yaml-kit:event-start-span document-end)
                   (yaml-kit:event-end-span document-end)
                   (yaml-kit:document-end-event-explicit-p document-end))
             (list (yaml-kit:event-start-span sequence-start)
                   (yaml-kit:event-end-span sequence-start)
                   (yaml-kit:sequence-start-event-anchor sequence-start)
                   (yaml-kit:sequence-start-event-tag sequence-start)
                   (yaml-kit:sequence-start-event-implicit-p sequence-start)
                   (yaml-kit:sequence-start-event-style sequence-start))
             (list (yaml-kit:event-start-span sequence-end)
                   (yaml-kit:event-end-span sequence-end))
             (list (yaml-kit:event-start-span mapping-start)
                   (yaml-kit:event-end-span mapping-start)
                   (yaml-kit:mapping-start-event-anchor mapping-start)
                   (yaml-kit:mapping-start-event-tag mapping-start)
                   (yaml-kit:mapping-start-event-implicit-p mapping-start)
                   (yaml-kit:mapping-start-event-style mapping-start))
             (list (yaml-kit:event-start-span mapping-end)
                   (yaml-kit:event-end-span mapping-end))
             (list (yaml-kit:event-start-span alias)
                   (yaml-kit:event-end-span alias)
                   (yaml-kit:alias-event-anchor alias)))
       :to-equal '((nil nil) (nil nil) (nil nil nil nil nil) (nil nil nil)
                   (nil nil nil nil nil :block) (nil nil)
                   (nil nil nil nil nil :block) (nil nil) (nil nil nil))))
  (it "uses scalar event defaults"
    (let ((scalar (yaml-kit:make-scalar-event)))
      (expect (list (yaml-kit:event-start-span scalar)
                    (yaml-kit:event-end-span scalar)
                    (yaml-kit:scalar-event-anchor scalar)
                    (yaml-kit:scalar-event-tag scalar)
                    (yaml-kit:scalar-event-value scalar)
                    (yaml-kit:scalar-event-plain-implicit-p scalar)
                    (yaml-kit:scalar-event-quoted-implicit-p scalar)
                    (yaml-kit:scalar-event-style scalar))
              :to-equal '(nil nil nil nil "" nil nil :plain))))))
(it "rejects invalid event slot values"
  (expect (every (lambda (thunk)
                   (handler-case (funcall thunk)
                     (type-error () t)))
                 (list (lambda () (yaml-kit:make-document-start-event :start-span *invalid-event-slot-value*))
                       (lambda () (yaml-kit:make-document-start-event :end-span *invalid-event-slot-value*))
                       (lambda () (yaml-kit:make-scalar-event :start-span *invalid-event-slot-value*))
                       (lambda () (yaml-kit:make-stream-start-event :start-span *invalid-event-slot-value*))
                       (lambda () (yaml-kit:make-stream-start-event :end-span *invalid-event-slot-value*))
                       (lambda () (yaml-kit:make-sequence-start-event
                                   :anchor *invalid-event-slot-value*))
                       (lambda () (yaml-kit:make-sequence-start-event
                                   :tag *invalid-event-slot-value*))
                       (lambda () (yaml-kit:make-mapping-start-event
                                   :anchor *invalid-event-slot-value*))
                       (lambda () (yaml-kit:make-mapping-start-event
                                   :tag *invalid-event-slot-value*))
                       (lambda () (yaml-kit:make-scalar-event
                                   :anchor *invalid-event-slot-value*))
                       (lambda () (yaml-kit:make-scalar-event
                                   :tag *invalid-event-slot-value*))))
          :to-be-truthy))

(defmacro define-event-contract-tests (name constructor predicate accessors arguments)
  (let ((keys (mapcar (lambda (accessor)
                        (loop for key in arguments by #'cddr
                              when (and (keywordp key)
                                        (search (string-upcase (symbol-name key))
                                                (string-upcase (symbol-name accessor))
                                                :from-end t))
                                do (return key)))
                      accessors)))
    `(progn
       (it ,(format nil "constructs ~A" name)
         (let ((value (,constructor ,@arguments)))
           (expect (,predicate value) :to-be-truthy)
           (expect (typep value ',name) :to-be-truthy)))
       ,@(mapcar (lambda (accessor key)
                   `(it ,(format nil "sets ~A via ~A" name accessor)
                        (let ((value (,constructor ,@arguments)))
                          (expect (contract-value-equal-p
                                   (,accessor value)
                                   (getf (list ,@arguments) ,key))
                                  :to-be-truthy))))
                 accessors keys))))

(define-event-contract-tests yaml-kit:stream-start-event yaml-kit:make-stream-start-event
  yaml-kit:stream-start-event-p (yaml-kit:event-start-span yaml-kit:event-end-span)
  (:start-span (test-point-span 1 2 3) :end-span (test-point-span 1 3 4)))
(define-event-contract-tests yaml-kit:stream-end-event yaml-kit:make-stream-end-event
  yaml-kit:stream-end-event-p (yaml-kit:event-start-span yaml-kit:event-end-span)
  (:start-span (test-point-span 2 0 5) :end-span (test-point-span 2 1 6)))
(define-event-contract-tests yaml-kit:document-start-event yaml-kit:make-document-start-event
  yaml-kit:document-start-event-p
  (yaml-kit:event-start-span yaml-kit:event-end-span yaml-kit:document-start-event-explicit-p
   yaml-kit:document-start-event-version yaml-kit:document-start-event-tag-directives)
  (:start-span (test-point-span 3 0 7) :end-span (test-point-span 3 1 8)
   :explicit-p t :version '(1 . 2) :tag-directives '(directive)))
(define-event-contract-tests yaml-kit:document-end-event yaml-kit:make-document-end-event
  yaml-kit:document-end-event-p
  (yaml-kit:event-start-span yaml-kit:event-end-span yaml-kit:document-end-event-explicit-p)
  (:start-span (test-point-span 4 0 9) :end-span (test-point-span 4 1 10) :explicit-p t))
(define-event-contract-tests yaml-kit:sequence-start-event yaml-kit:make-sequence-start-event
  yaml-kit:sequence-start-event-p
  (yaml-kit:event-start-span yaml-kit:event-end-span yaml-kit:sequence-start-event-anchor
   yaml-kit:sequence-start-event-tag yaml-kit:sequence-start-event-implicit-p
   yaml-kit:sequence-start-event-style)
  (:start-span (test-point-span 5 0 11) :end-span (test-point-span 5 1 12)
   :anchor "a" :tag "t" :implicit-p t :style :flow))
(define-event-contract-tests yaml-kit:sequence-end-event yaml-kit:make-sequence-end-event
  yaml-kit:sequence-end-event-p (yaml-kit:event-start-span yaml-kit:event-end-span)
  (:start-span (test-point-span 6 0 13) :end-span (test-point-span 6 1 14)))
(define-event-contract-tests yaml-kit:mapping-start-event yaml-kit:make-mapping-start-event
  yaml-kit:mapping-start-event-p
  (yaml-kit:event-start-span yaml-kit:event-end-span yaml-kit:mapping-start-event-anchor
   yaml-kit:mapping-start-event-tag yaml-kit:mapping-start-event-implicit-p
   yaml-kit:mapping-start-event-style)
  (:start-span (test-point-span 7 0 15) :end-span (test-point-span 7 1 16)
   :anchor "a" :tag "t" :implicit-p t :style :flow))
(define-event-contract-tests yaml-kit:mapping-end-event yaml-kit:make-mapping-end-event
  yaml-kit:mapping-end-event-p (yaml-kit:event-start-span yaml-kit:event-end-span)
  (:start-span (test-point-span 8 0 17) :end-span (test-point-span 8 1 18)))
(define-event-contract-tests yaml-kit:scalar-event yaml-kit:make-scalar-event
  yaml-kit:scalar-event-p
  (yaml-kit:event-start-span yaml-kit:event-end-span yaml-kit:scalar-event-anchor
   yaml-kit:scalar-event-tag yaml-kit:scalar-event-value
   yaml-kit:scalar-event-plain-implicit-p yaml-kit:scalar-event-quoted-implicit-p
   yaml-kit:scalar-event-style)
  (:start-span (test-point-span 9 0 19) :end-span (test-point-span 9 1 20)
   :anchor "a" :tag "t" :value "value" :plain-implicit-p t
   :quoted-implicit-p t :style :double-quoted))
(progn
  (define-event-contract-tests yaml-kit:alias-event yaml-kit:make-alias-event
    yaml-kit:alias-event-p
    (yaml-kit:event-start-span yaml-kit:event-end-span yaml-kit:alias-event-anchor)
    (:start-span (test-point-span 10 0 21) :end-span (test-point-span 10 1 22)
     :anchor "a"))
  (it "returns anchor, tag, and style from events"
    (let ((event (yaml-kit:make-scalar-event
                  :anchor "a" :tag "tag:example" :style :double-quoted)))
      (expect (yaml-kit::%event-attribute event :anchor) :to-equal "a")
      (expect (yaml-kit::%event-attribute event :tag) :to-equal "tag:example")
      (expect (yaml-kit::%event-attribute event :style) :to-equal :double-quoted)))
  (it "returns nil for an event without the requested attribute"
    (expect (yaml-kit::%event-attribute
             (yaml-kit:make-stream-start-event) :anchor)
            :to-equal nil))
  (it "rejects an unsupported event attribute"
    (expect (handler-case
                (progn
                  (yaml-kit::%event-attribute (yaml-kit:make-scalar-event) :unknown)
                  nil)
              (type-error () t))
            :to-be-truthy)))
