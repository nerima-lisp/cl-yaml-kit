(in-package #:yaml-kit)

(defconstant +default-max-input-length+ 104857600)

(defconstant +default-max-depth+ 1000)

(defconstant +default-max-scalar-length+ 16777216)

(defconstant +default-max-nodes+ 1000000)

(defconstant +default-max-alias-expansions+ 100000)

(defmacro define-yaml-resource-function
    (name positional-arguments resource-spec &body body)
  (let* ((group (getf resource-spec :group))
         (keyword-arguments (getf resource-spec :keywords))
         (resource-keywords
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
              (error "Unsupported YAML resource group: ~S" group)))))
    (when (intersection (mapcar #'first keyword-arguments)
                       (mapcar #'first resource-keywords)
                       :test #'eq)
      (error "YAML resource keyword was specified more than once"))
    (append (list 'defun name
                  (append positional-arguments
                          (cons '&key
                                (append keyword-arguments resource-keywords))))
            body)))
