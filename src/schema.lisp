;;;; src/schema.lisp
(in-package #:yaml-kit)

(defparameter +yaml-tag-prefix+ "tag:yaml.org,2002:")
(defparameter +schema-prefilter+
  (let ((table (make-hash-table :test #'eql)))
    (loop for character across "~nNtTfF.+-0123456789"
          do (setf (gethash character table) t))
    table))

(defconstant +core-resolution-table+
  '((:null "^(?:~|null|Null|NULL)$")
    (:bool "^(?:true|True|TRUE|false|False|FALSE)$")
    (:int "^(?:[-+]?(?:[0-9][0-9_]*|0o[0-7_]+|0x[0-9a-fA-F_]+))$")
    (:float "^(?:[-+]?(?:[0-9][0-9_]*\\.[0-9_]*(?:[eE][-+]?[0-9]+)?|[0-9][0-9_]*(?:[eE][-+]?[0-9]+)|\\.[0-9_]+(?:[eE][-+]?[0-9]+)?|\\.(?:inf|Inf|INF)|\\.(?:nan|NaN|NAN)))$")))

(defconstant +json-resolution-table+
  '((:null "^null$") (:bool "^(?:true|false)$")
    (:int "^-?(?:0|[1-9][0-9]*)$")
    (:float "^-?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)$")))

(defmacro define-schema-resolver (name table)
  (let ((entries (if (symbolp table) (symbol-value table) table)))
    `(defun ,name (value)
       (declare (optimize (speed 3) (safety 1)))
       (cond
         ,@(mapcar (lambda (entry)
                     `((cl-regex-kit:full-match-p
                        (load-time-value
                         (cl-regex-kit:compile-regex ,(second entry)) t)
                        value)
                       ,(first entry)))
                   entries)
         (t :str)))))

(define-schema-resolver %core-kind +core-resolution-table+)
(define-schema-resolver %json-kind +json-resolution-table+)

(defun %core-resolve (value)
  (if (or (zerop (length value))
          (not (gethash (char value 0) +schema-prefilter+)))
      :str
      (%core-kind value)))

(defun resolve-tag (node &key (schema :core))
  "Return NODE's explicit tag or its implicit schema tag."
  (let ((tag (node-tag node)))
    (if tag
        (cond ((string= tag "!!str") "tag:yaml.org,2002:str")
              ((string= tag "!!int") "tag:yaml.org,2002:int")
              ((string= tag "!!float") "tag:yaml.org,2002:float")
              ((string= tag "!!bool") "tag:yaml.org,2002:bool")
              ((string= tag "!!null") "tag:yaml.org,2002:null")
              (t tag))
        (if (and (scalar-node-p node) (member schema '(:core :json)))
            (concatenate 'string +yaml-tag-prefix+
                         (symbol-name (if (eq schema :json)
                                         (if (zerop (length (scalar-node-value node)))
                                             :str
                                             (%json-kind (scalar-node-value node)))
                                         (%core-resolve (scalar-node-value node)))))
            (concatenate 'string +yaml-tag-prefix+
                         (if (scalar-node-p node) "str" "map"))))))
