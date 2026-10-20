;;;; t/events-test.lisp
(in-package #:cl-yaml-kit/test)
(defparameter *invalid-event-slot-value* -1)

(defun contract-value-equal-p (actual expected)
  (if (and (typep actual 'yaml-kit:mark) (typep expected 'yaml-kit:mark))
      (and (= (yaml-kit:mark-line actual) (yaml-kit:mark-line expected))
           (= (yaml-kit:mark-column actual) (yaml-kit:mark-column expected))
           (= (yaml-kit:mark-offset actual) (yaml-kit:mark-offset expected)))
      (equal actual expected)))

(it "constructs a mark and exposes its coordinates"
  (let ((mark (yaml-kit:make-mark 11 12 13)))
    (expect (yaml-kit:mark-line mark) :to-equal 11)
    (expect (yaml-kit:mark-column mark) :to-equal 12)
    (expect (yaml-kit:mark-offset mark) :to-equal 13)))

(it "uses event constructor defaults"
  (let ((stream-start (yaml-kit:make-stream-start-event))
        (stream-end (yaml-kit:make-stream-end-event))
        (document-start (yaml-kit:make-document-start-event))
        (document-end (yaml-kit:make-document-end-event))
        (sequence-start (yaml-kit:make-sequence-start-event))
        (sequence-end (yaml-kit:make-sequence-end-event))
        (mapping-start (yaml-kit:make-mapping-start-event))
        (mapping-end (yaml-kit:make-mapping-end-event))
        (scalar (yaml-kit:make-scalar-event))
        (alias (yaml-kit:make-alias-event)))
    (expect (list (list (yaml-kit:event-start-mark stream-start)
                        (yaml-kit:event-end-mark stream-start))
                  (list (yaml-kit:event-start-mark stream-end)
                        (yaml-kit:event-end-mark stream-end))
                  (list (yaml-kit:event-start-mark document-start)
                        (yaml-kit:event-end-mark document-start)
                        (yaml-kit:document-start-event-explicit-p document-start)
                        (yaml-kit:document-start-event-version document-start)
                        (yaml-kit:document-start-event-tag-directives document-start))
                  (list (yaml-kit:event-start-mark document-end)
                        (yaml-kit:event-end-mark document-end)
                        (yaml-kit:document-end-event-explicit-p document-end)))
            :to-equal '((nil nil) (nil nil) (nil nil nil nil nil) (nil nil nil)))
    (expect (list (list (yaml-kit:event-start-mark sequence-start)
                        (yaml-kit:event-end-mark sequence-start)
                        (yaml-kit:sequence-start-event-anchor sequence-start)
                        (yaml-kit:sequence-start-event-tag sequence-start)
                        (yaml-kit:sequence-start-event-implicit-p sequence-start)
                        (yaml-kit:sequence-start-event-style sequence-start))
                  (list (yaml-kit:event-start-mark sequence-end)
                        (yaml-kit:event-end-mark sequence-end))
                  (list (yaml-kit:event-start-mark mapping-start)
                        (yaml-kit:event-end-mark mapping-start)
                        (yaml-kit:mapping-start-event-anchor mapping-start)
                        (yaml-kit:mapping-start-event-tag mapping-start)
                        (yaml-kit:mapping-start-event-implicit-p mapping-start)
                        (yaml-kit:mapping-start-event-style mapping-start))
                  (list (yaml-kit:event-start-mark mapping-end)
                        (yaml-kit:event-end-mark mapping-end)))
            :to-equal '((nil nil nil nil nil :block) (nil nil)
                        (nil nil nil nil nil :block) (nil nil)))
    (expect (list (list (yaml-kit:event-start-mark scalar)
                        (yaml-kit:event-end-mark scalar)
                        (yaml-kit:scalar-event-anchor scalar)
                        (yaml-kit:scalar-event-tag scalar)
                        (yaml-kit:scalar-event-value scalar)
                        (yaml-kit:scalar-event-plain-implicit-p scalar)
                        (yaml-kit:scalar-event-quoted-implicit-p scalar)
                        (yaml-kit:scalar-event-style scalar))
                  (list (yaml-kit:event-start-mark alias)
                        (yaml-kit:event-end-mark alias)
                        (yaml-kit:alias-event-anchor alias)))
            :to-equal '((nil nil nil nil "" nil nil :plain) (nil nil nil)))))
(it "rejects invalid event slot values"
  (expect (every (lambda (thunk)
                   (handler-case (funcall thunk)
                     (type-error () t)))
                 (list (lambda () (yaml-kit:make-mark *invalid-event-slot-value* 0 0))
                       (lambda () (yaml-kit:make-mark 0 *invalid-event-slot-value* 0))
                       (lambda () (yaml-kit:make-mark 0 0 *invalid-event-slot-value*))
                       (lambda () (yaml-kit:make-stream-start-event
                                   :start-mark *invalid-event-slot-value*))
                       (lambda () (yaml-kit:make-stream-start-event
                                   :end-mark *invalid-event-slot-value*))
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
    `(it ,(format nil "constructs ~A" name)
       (let ((value (,constructor ,@arguments)))
         (expect (,predicate value) :to-be-truthy)
         (expect (typep value ',name) :to-be-truthy)
         ,@(mapcar (lambda (accessor key)
                     `(expect (contract-value-equal-p (,accessor value)
                                     (getf (list ,@arguments) ,key))
                              :to-be-truthy))
                   accessors keys)))))

(define-event-contract-tests yaml-kit:stream-start-event yaml-kit:make-stream-start-event
  yaml-kit:stream-start-event-p (yaml-kit:event-start-mark yaml-kit:event-end-mark)
  (:start-mark (yaml-kit:make-mark 1 2 3) :end-mark (yaml-kit:make-mark 1 3 4)))
(define-event-contract-tests yaml-kit:stream-end-event yaml-kit:make-stream-end-event
  yaml-kit:stream-end-event-p (yaml-kit:event-start-mark yaml-kit:event-end-mark)
  (:start-mark (yaml-kit:make-mark 2 0 5) :end-mark (yaml-kit:make-mark 2 1 6)))
(define-event-contract-tests yaml-kit:document-start-event yaml-kit:make-document-start-event
  yaml-kit:document-start-event-p
  (yaml-kit:event-start-mark yaml-kit:event-end-mark yaml-kit:document-start-event-explicit-p
   yaml-kit:document-start-event-version yaml-kit:document-start-event-tag-directives)
  (:start-mark (yaml-kit:make-mark 3 0 7) :end-mark (yaml-kit:make-mark 3 1 8)
   :explicit-p t :version '(1 . 2) :tag-directives '(directive)))
(define-event-contract-tests yaml-kit:document-end-event yaml-kit:make-document-end-event
  yaml-kit:document-end-event-p
  (yaml-kit:event-start-mark yaml-kit:event-end-mark yaml-kit:document-end-event-explicit-p)
  (:start-mark (yaml-kit:make-mark 4 0 9) :end-mark (yaml-kit:make-mark 4 1 10) :explicit-p t))
(define-event-contract-tests yaml-kit:sequence-start-event yaml-kit:make-sequence-start-event
  yaml-kit:sequence-start-event-p
  (yaml-kit:event-start-mark yaml-kit:event-end-mark yaml-kit:sequence-start-event-anchor
   yaml-kit:sequence-start-event-tag yaml-kit:sequence-start-event-implicit-p
   yaml-kit:sequence-start-event-style)
  (:start-mark (yaml-kit:make-mark 5 0 11) :end-mark (yaml-kit:make-mark 5 1 12)
   :anchor "a" :tag "t" :implicit-p t :style :flow))
(define-event-contract-tests yaml-kit:sequence-end-event yaml-kit:make-sequence-end-event
  yaml-kit:sequence-end-event-p (yaml-kit:event-start-mark yaml-kit:event-end-mark)
  (:start-mark (yaml-kit:make-mark 6 0 13) :end-mark (yaml-kit:make-mark 6 1 14)))
(define-event-contract-tests yaml-kit:mapping-start-event yaml-kit:make-mapping-start-event
  yaml-kit:mapping-start-event-p
  (yaml-kit:event-start-mark yaml-kit:event-end-mark yaml-kit:mapping-start-event-anchor
   yaml-kit:mapping-start-event-tag yaml-kit:mapping-start-event-implicit-p
   yaml-kit:mapping-start-event-style)
  (:start-mark (yaml-kit:make-mark 7 0 15) :end-mark (yaml-kit:make-mark 7 1 16)
   :anchor "a" :tag "t" :implicit-p t :style :flow))
(define-event-contract-tests yaml-kit:mapping-end-event yaml-kit:make-mapping-end-event
  yaml-kit:mapping-end-event-p (yaml-kit:event-start-mark yaml-kit:event-end-mark)
  (:start-mark (yaml-kit:make-mark 8 0 17) :end-mark (yaml-kit:make-mark 8 1 18)))
(define-event-contract-tests yaml-kit:scalar-event yaml-kit:make-scalar-event
  yaml-kit:scalar-event-p
  (yaml-kit:event-start-mark yaml-kit:event-end-mark yaml-kit:scalar-event-anchor
   yaml-kit:scalar-event-tag yaml-kit:scalar-event-value
   yaml-kit:scalar-event-plain-implicit-p yaml-kit:scalar-event-quoted-implicit-p
   yaml-kit:scalar-event-style)
  (:start-mark (yaml-kit:make-mark 9 0 19) :end-mark (yaml-kit:make-mark 9 1 20)
   :anchor "a" :tag "t" :value "value" :plain-implicit-p t
   :quoted-implicit-p t :style :double-quoted))
(define-event-contract-tests yaml-kit:alias-event yaml-kit:make-alias-event
  yaml-kit:alias-event-p
  (yaml-kit:event-start-mark yaml-kit:event-end-mark yaml-kit:alias-event-anchor)
  (:start-mark (yaml-kit:make-mark 10 0 21) :end-mark (yaml-kit:make-mark 10 1 22)
   :anchor "a"))
