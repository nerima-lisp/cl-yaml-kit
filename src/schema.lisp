;;;; src/schema.lisp
(in-package #:yaml-kit)

(defparameter +yaml-tag-prefix+ "tag:yaml.org,2002:")

(defun %schema-return-tag (tag value)
  (declare (ignore value))
  tag)

;; Each row is (tag regexp conversion-function specification-section).
;; The regular expressions deliberately omit anchors: FULL-MATCH-P already
;; expresses whole-string matching.
(defvar *schema-spec-tables*
  '((:failsafe
     ("tag:yaml.org,2002:str" nil %schema-return-tag "10.1.2"))
    (:json
     ("tag:yaml.org,2002:null" "null" %schema-return-tag "10.2.2")
     ("tag:yaml.org,2002:bool" "true|false" %schema-return-tag "10.2.2")
     ("tag:yaml.org,2002:int" "-?(0|[1-9][0-9]*)" %schema-return-tag "10.2.2")
     ("tag:yaml.org,2002:float"
      "-?(0|[1-9][0-9]*)(\\.[0-9]*)?([eE][-+]?[0-9]+)?"
      %schema-return-tag "10.2.2")
     ("tag:yaml.org,2002:str" nil %schema-return-tag "10.2.2"))
    (:core
     ("tag:yaml.org,2002:null" "~|null|Null|NULL" %schema-return-tag "10.3.2")
     ("tag:yaml.org,2002:null" "" %schema-return-tag "10.3.2")
     ("tag:yaml.org,2002:bool"
      "true|True|TRUE|false|False|FALSE" %schema-return-tag "10.3.2")
     ("tag:yaml.org,2002:int" "[-+]?[0-9]+" %schema-return-tag "10.3.2")
     ("tag:yaml.org,2002:int" "0o[0-7]+" %schema-return-tag "10.3.2")
     ("tag:yaml.org,2002:int" "0x[0-9a-fA-F]+" %schema-return-tag "10.3.2")
     ("tag:yaml.org,2002:float"
      "[-+]?((\\.[0-9]+)|([0-9]+(\\.[0-9]*)?))([eE][-+]?[0-9]+)?"
      %schema-return-tag "10.3.2")
     ("tag:yaml.org,2002:float" "[-+]?\\.(inf|Inf|INF)"
      %schema-return-tag "10.3.2")
     ("tag:yaml.org,2002:float" "\\.(nan|NaN|NAN)"
      %schema-return-tag "10.3.2")
     ("tag:yaml.org,2002:str" nil %schema-return-tag "10.3.2"))))

(defun %schema-first-characters (rows)
  (let ((bits (make-array 128 :element-type 'bit :initial-element 0)))
    (dolist (row rows (the simple-bit-vector bits))
      (let ((regexp (second row)))
        (when (and regexp (plusp (length regexp)))
          ;; This is deliberately conservative.  The previous implementation
          ;; treated every regexp beginning with '-' as matching only '-',
          ;; which rejected all JSON numbers before FULL-MATCH-P ran.
          (let ((characters
                  (concatenate 'string
                               (when (or (search "0-9" regexp)
                                         (search "[1-9]" regexp))
                                 "0123456789")
                               (when (or (search "[-+]" regexp)
                                         (search "-?" regexp))
                                 "-+")
                               (when (search "null" regexp) "nN~")
                               (when (search "true" regexp) "tT")
                               (when (search "false" regexp) "fF")
                               (when (search "inf" regexp) ".iI")
                               (when (search "nan" regexp) ".nNaA")
                               (when (search "\\." regexp) "."))))
            (loop for character across characters
                  do (setf (sbit bits (char-code character)) 1))))))))

(defun %compile-schema-table (rows)
  (let ((compiled
          (mapcar (lambda (row)
                    (list (first row)
                          (and (second row)
                               (cl-regex-kit:compile-regex (second row)))
                          (third row)
                          (fourth row)))
                  rows)))
    (cons (%schema-first-characters rows) compiled)))

;; DEFVAR is intentional. A source reload must not replace compiled regex
;; objects, keeping compilation once per loaded image.
(defvar *compiled-schema-tables*
  (mapcar (lambda (schema)
            (cons (first schema) (%compile-schema-table (rest schema))))
          *schema-spec-tables*))

(defun %schema-table (schema)
  (or (cdr (assoc schema *compiled-schema-tables*))
      (error "Unknown YAML schema: ~S" schema)))

(defun %schema-resolve (value table)
  (declare (optimize (speed 3) (safety 1))
           (type string value))
  (let ((bits (car table)))
    (when (and (plusp (length value))
               (< (char-code (char value 0)) (length bits))
               (zerop (sbit bits (char-code (char value 0)))))
      (return-from %schema-resolve "tag:yaml.org,2002:str"))
    (dolist (row (cdr table) "tag:yaml.org,2002:str")
      (let ((regexp (second row)))
        (when (and regexp
                   (cl-regex-kit:full-match-p regexp value))
          (return (funcall (third row) (first row) value)))))))

(defmacro define-schema-resolver (name schema)
  `(defun ,name (value)
     (declare (optimize (speed 3) (safety 1))
              (type string value))
     (%schema-resolve value (%schema-table ,schema))))

(define-schema-resolver %failsafe-kind :failsafe)
(define-schema-resolver %json-kind :json)
(define-schema-resolver %core-kind :core)

(defun resolve-plain-scalar-tag (string schema)
  "Resolve plain STRING to its YAML tag under SCHEMA.

SCHEMA is :FAILSAFE, :JSON, or :CORE. This package-internal contract is
shared by loaders and dumpers when deciding whether STRING may be emitted as
a plain scalar. Quoted and block scalars must not call this resolver."
  (ecase schema
    (:failsafe (%failsafe-kind string))
    (:json (%json-kind string))
    (:core (%core-kind string))))

(defun %implicit-node-tag (node schema)
  (cond ((scalar-node-p node)
         (resolve-plain-scalar-tag (scalar-node-value node) schema))
        ((sequence-node-p node) "tag:yaml.org,2002:seq")
        (t "tag:yaml.org,2002:map")))

(defun resolve-tag (node &key (schema :core))
  "Return NODE's explicit tag or its implicit schema tag."
  (let ((tag (node-tag node)))
    (if (and tag (not (member tag '("!" "?") :test #'string=)))
        (cond ((string= tag "!!str") "tag:yaml.org,2002:str")
              ((string= tag "!!int") "tag:yaml.org,2002:int")
              ((string= tag "!!float") "tag:yaml.org,2002:float")
              ((string= tag "!!bool") "tag:yaml.org,2002:bool")
              ((string= tag "!!null") "tag:yaml.org,2002:null")
              ((string= tag "!!seq") "tag:yaml.org,2002:seq")
              ((string= tag "!!map") "tag:yaml.org,2002:map")
              (t tag))
        (if (and tag (scalar-node-p node) (string= tag "!"))
            "tag:yaml.org,2002:str"
            (%implicit-node-tag node schema)))))
