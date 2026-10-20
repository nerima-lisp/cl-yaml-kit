(in-package #:yaml-kit)

(defconstant +default-max-input-length+ 104857600)

(defconstant +default-max-depth+ 1000)

(defconstant +default-max-scalar-length+ 16777216)

(defconstant +default-max-nodes+ 1000000)

(defconstant +default-max-alias-expansions+ 100000)

(defmacro define-yaml-resource-function
    (name positional-arguments resource-spec &body body)
  (labels ((proper-list-p (value)
             (and (listp value)
                  (handler-case
                      (integerp (list-length value))
                    (type-error () nil))))
           (valid-variable-p (value)
             (and (symbolp value)
                  (not (keywordp value))
                  (not (member value
                               '(&optional &rest &body &key
                                 &allow-other-keys &aux &whole &environment)
                               :test #'eq))))
           (keyword-argument-name (argument)
             (cond
               ((valid-variable-p argument)
                (intern (symbol-name argument) :keyword))
               ((and (proper-list-p argument)
                     (member (length argument) '(2 3)))
                (let* ((name-form (first argument))
                       (name
                         (cond
                           ((valid-variable-p name-form)
                            (intern (symbol-name name-form) :keyword))
                           ((and (proper-list-p name-form)
                                 (= (length name-form) 2)
                                 (keywordp (first name-form))
                                 (valid-variable-p (second name-form)))
                            (first name-form))
                           (t
                            (error "Invalid YAML keyword name: ~S"
                                   name-form))))
                       (variable (if (consp name-form)
                                     (second name-form)
                                     name-form)))
                  (unless (valid-variable-p variable)
                    (error "Invalid YAML keyword variable: ~S" variable))
                  (when (and (= (length argument) 3)
                             (not (valid-variable-p (third argument))))
                    (error "Invalid YAML supplied-p variable: ~S"
                           (third argument)))
                  name))
               (t
                (error "Invalid YAML keyword argument: ~S" argument)))))
    (unless (symbolp name)
      (error "Invalid YAML resource function name: ~S" name))
    (unless (proper-list-p positional-arguments)
      (error "Invalid YAML positional arguments: ~S" positional-arguments))
    (unless (and (proper-list-p resource-spec)
                 (= (length resource-spec) 4)
                 (let ((keys (loop for (key value) on resource-spec by #'cddr
                                   collect key)))
                   (and (member :group keys :test #'eq)
                        (member :keywords keys :test #'eq)
                        (not (eq (first keys) (second keys))))))
      (error "Invalid YAML resource spec: ~S" resource-spec))
    (let* ((group (getf resource-spec :group))
           (keyword-arguments (getf resource-spec :keywords)))
      (unless (proper-list-p keyword-arguments)
        (error "Invalid YAML keyword arguments: ~S" keyword-arguments))
      (let* ((resource-keywords
               (case group
                 (:input
                  '((max-input-length +default-max-input-length+)))
                 (:scanner
                  '((max-input-length +default-max-input-length+)
                    (max-depth +default-max-depth+)
                    (max-scalar-length +default-max-scalar-length+)))
                 (:composer
                  '((max-depth +default-max-depth+)
                    (max-scalar-length +default-max-scalar-length+)
                    (max-nodes +default-max-nodes+)
                    (max-alias-expansions +default-max-alias-expansions+)))
                 (:reader
                  '((max-input-length +default-max-input-length+)
                    (max-depth +default-max-depth+)
                    (max-scalar-length +default-max-scalar-length+)
                    (max-nodes +default-max-nodes+)
                    (max-alias-expansions +default-max-alias-expansions+)))
                 (otherwise
                  (error "Unsupported YAML resource group: ~S" group))))
             (all-keyword-arguments
               (append keyword-arguments resource-keywords))
             (seen-keywords nil))
        (dolist (argument all-keyword-arguments)
          (let ((keyword (keyword-argument-name argument)))
            (when (member keyword seen-keywords :test #'eq)
              (error "Duplicate YAML keyword argument: ~S" keyword))
            (push keyword seen-keywords)))
        (append (list 'defun name
                      (append positional-arguments
                              (cons '&key all-keyword-arguments)))
                body)))))
