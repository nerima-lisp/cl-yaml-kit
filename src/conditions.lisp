;;;; src/conditions.lisp
(in-package #:yaml-kit)

(defmacro define-yaml-condition-family (superclasses &body definitions)
  `(progn
     ,@(mapcar (lambda (definition)
                 (destructuring-bind (name slots &rest options) definition
                   `(define-condition ,name ,superclasses ,slots ,@options)))
               definitions)))

(defun %yaml-error-diagnostic (diagnostic message)
  (let ((result
          (or diagnostic
              (cl-parser-kit:make-diagnostic
               :kind :error
               :message (princ-to-string (or message "YAML error"))))))
    (check-type result cl-parser-kit:diagnostic)
    (check-type (cl-parser-kit:diagnostic-message result) string)
    result))

(progn
  (define-condition yaml-kit-error (error)
    ((diagnostic :initarg :diagnostic
                 :initform (cl-parser-kit:make-diagnostic
                            :kind :error :message "YAML error")
                 :reader yaml-kit-error-diagnostic
                 :type cl-parser-kit:diagnostic))
    (:report (lambda (condition stream)
               (write-string
                (cl-parser-kit:diagnostic->string
                 (yaml-kit-error-diagnostic condition))
                stream))))
  (defmethod initialize-instance :after ((condition yaml-kit-error)
                                         &key &allow-other-keys)
    (check-type
     (cl-parser-kit:diagnostic-message
      (yaml-kit-error-diagnostic condition))
     string)))
(define-condition yaml-parse-error (yaml-kit-error)
  ((context :initarg :context :initform "YAML" :reader yaml-parse-error-context)))
(define-yaml-condition-family (yaml-kit-error)
  (yaml-compose-error ((context :initarg :context :initform "YAML composition"
                                :reader yaml-compose-error-context)
                       (cause :initarg :cause :initform nil
                              :reader yaml-compose-error-cause)))
  (yaml-emit-error ((context :initarg :context :initform "YAML emission"
                             :reader yaml-emit-error-context)
                    (cause :initarg :cause :initform nil
                           :reader yaml-emit-error-cause))))
(defun signal-yaml-compose-error (&key diagnostic context cause message)
  (error 'yaml-compose-error
         :diagnostic (%yaml-error-diagnostic diagnostic message)
         :context (or context "YAML composition")
         :cause cause))

(defun signal-yaml-emit-error (&key diagnostic context cause message)
  (error 'yaml-emit-error
         :diagnostic (%yaml-error-diagnostic diagnostic message)
         :context (or context "YAML emission")
         :cause cause))
(define-condition yaml-resource-limit-error (yaml-kit-error)
  ((limit-name :initarg :limit-name :initform "resource"
               :reader yaml-resource-limit-error-limit-name)
   (limit :initarg :limit :initform nil :reader yaml-resource-limit-error-limit)
   (actual :initarg :actual :initform nil :reader yaml-resource-limit-error-actual)
   (context :initarg :context :initform "YAML"
            :reader yaml-resource-limit-error-context)))

(defun %signal-yaml-resource-limit (name limit actual &key span context)
  (error 'yaml-resource-limit-error
         :limit-name name
         :limit limit
         :actual actual
         :context context
         :diagnostic
         (cl-parser-kit:make-diagnostic
          :message (format nil "YAML resource limit ~S exceeded: limit ~S, actual ~S"
                           name limit actual)
          :span span
          :data (list :limit-name name :limit limit :actual actual
                      :context context))))

(define-condition yaml-format-edit-path-error (yaml-kit-error)
  ((path :initarg :path :initform nil :reader yaml-format-edit-path-error-path)
   (document :initarg :document :initform 0
             :reader yaml-format-edit-path-error-document)
   (operation :initarg :operation :initform nil
              :reader yaml-format-edit-path-error-operation)))

(define-condition yaml-format-edit-structure-error (yaml-kit-error)
  ((path :initarg :path :initform nil
         :reader yaml-format-edit-structure-error-path)))

(define-condition yaml-format-edit-anchor-error (yaml-kit-error)
  ((path :initarg :path :initform nil :reader yaml-format-edit-anchor-error-path)
   (anchor :initarg :anchor :initform nil
           :reader yaml-format-edit-anchor-error-anchor)))
