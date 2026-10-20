(in-package #:yaml-kit)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %reload-safe-schema-constant (name initializer)
    (if (boundp name)
        (symbol-value name)
        (funcall initializer))))

;; Reuse non-EQL values on reload because DEFCONSTANT redefinitions require EQL.
(defconstant +yaml-schema-names+
  (%reload-safe-schema-constant
   (quote +yaml-schema-names+)
   (lambda () '(:failsafe :json :core))))

(defconstant +yaml-json-null-spellings+
  (%reload-safe-schema-constant
   (quote +yaml-json-null-spellings+)
   (lambda () '("null"))))

(defconstant +yaml-json-true-spellings+
  (%reload-safe-schema-constant
   (quote +yaml-json-true-spellings+)
   (lambda () '("true"))))

(defconstant +yaml-json-false-spellings+
  (%reload-safe-schema-constant
   (quote +yaml-json-false-spellings+)
   (lambda () '("false"))))

(defconstant +yaml-core-null-spellings+
  (%reload-safe-schema-constant
   (quote +yaml-core-null-spellings+)
   (lambda () (append '("" "~") +yaml-json-null-spellings+ '("Null" "NULL")))))

(defconstant +yaml-core-true-spellings+
  (%reload-safe-schema-constant
   (quote +yaml-core-true-spellings+)
   (lambda () (append +yaml-json-true-spellings+ '("True" "TRUE")))))

(defconstant +yaml-core-false-spellings+
  (%reload-safe-schema-constant
   (quote +yaml-core-false-spellings+)
   (lambda () (append +yaml-json-false-spellings+ '("False" "FALSE")))))

(defconstant +yaml-schema-string-tag+
  (%reload-safe-schema-constant
   (quote +yaml-schema-string-tag+)
   (lambda () (concatenate 'string +yaml-tag-prefix+ "str"))))
(defconstant +yaml-schema-int-tag+
  (%reload-safe-schema-constant
   (quote +yaml-schema-int-tag+)
   (lambda () (concatenate 'string +yaml-tag-prefix+ "int"))))
(defconstant +yaml-schema-float-tag+
  (%reload-safe-schema-constant
   (quote +yaml-schema-float-tag+)
   (lambda () (concatenate 'string +yaml-tag-prefix+ "float"))))
(defconstant +yaml-schema-bool-tag+
  (%reload-safe-schema-constant
   (quote +yaml-schema-bool-tag+)
   (lambda () (concatenate 'string +yaml-tag-prefix+ "bool"))))
(defconstant +yaml-schema-null-tag+
  (%reload-safe-schema-constant
   (quote +yaml-schema-null-tag+)
   (lambda () (concatenate 'string +yaml-tag-prefix+ "null"))))

(defconstant +yaml-schema-sequence-tag+
  (%reload-safe-schema-constant
   (quote +yaml-schema-sequence-tag+)
   (lambda () (concatenate 'string +yaml-tag-prefix+ "seq"))))

(defconstant +yaml-schema-mapping-tag+
  (%reload-safe-schema-constant
   (quote +yaml-schema-mapping-tag+)
   (lambda () (concatenate 'string +yaml-tag-prefix+ "map"))))

;; A tag row pairs a short tag with its URI, kind, and collection status.
(defmacro define-tag-table (name &rest rows)
  (let ((entries
          (mapcar (lambda (row)
                    (destructuring-bind (short uri kind category) row
                      (unless (member category (list :scalar :collection))
                        (error "Invalid tag-table category: ~S" category))
                      `(list ,short ,uri ,kind ,(eq category :collection))))
                  rows)))
    `(defparameter ,name
       (list ,@entries))))

(define-tag-table *yaml-tag-table*
  ("!!str" +yaml-schema-string-tag+ :str :scalar)
  ("!!int" +yaml-schema-int-tag+ :int :scalar)
  ("!!float" +yaml-schema-float-tag+ :float :scalar)
  ("!!bool" +yaml-schema-bool-tag+ :bool :scalar)
  ("!!null" +yaml-schema-null-tag+ :null :scalar)
  ;; Collection tags cannot be applied to scalar nodes.
  ("!!seq" +yaml-schema-sequence-tag+ :invalid-collection :collection)
  ("!!map" +yaml-schema-mapping-tag+ :invalid-collection :collection))

;; Quote each spelling before joining regex alternatives.
(defun %schema-spellings-regex (spellings)
  (format nil "~{~A~^|~}"
          (mapcar #'cl-regex-kit:escape spellings)))

;; Regexes omit anchors because the resolver full-matches complete scalar values.
;; Rows are (tag-suffix regexp first-chars); suffix completes the URI.
;; First-chars prefilters regex starts.
;; Keep integer rows before overlapping float rows; resolution takes the first full match.
;; Source-table edits take effect only in a fresh image; compiled regexes are cached.
(defvar *schema-spec-tables*
  ;; Failsafe recognizes every plain scalar as a string, so it has no pattern rows.
  `((:failsafe)
    (:json
     ("null" ,(%schema-spellings-regex +yaml-json-null-spellings+) "n")
     ("bool" ,(%schema-spellings-regex
                (append +yaml-json-true-spellings+
                        +yaml-json-false-spellings+))
      "tf")
     ("int" "-?(0|[1-9][0-9]*)" "-0123456789")
     ("float" "-?(0|[1-9][0-9]*)(\\.[0-9]+)?([eE][-+]?[0-9]+)?"
      "-0123456789"))
    (:core
     ("null" ,(%schema-spellings-regex
                (remove "" +yaml-core-null-spellings+ :test #'string=))
      "~nN")
     ;; The empty spelling matches an empty plain scalar as null.
     ("null" "" "")
     ("bool" ,(%schema-spellings-regex
                (append +yaml-core-true-spellings+
                        +yaml-core-false-spellings+))
      "tTfF")
     ("int" "[-+]?[0-9]+" "-+0123456789")
     ("int" "[-+]?0o[0-7]+" "-+0")
     ("int" "[-+]?0x[0-9a-fA-F]+" "-+0")
     ("float" "[-+]?((\\.[0-9]+)|([0-9]+(\\.[0-9]*)?))([eE][-+]?[0-9]+)?"
      "-+.0123456789")
     ("float" "[-+]?\\.(inf|Inf|INF)" "-+.")
     ("float" "\\.(nan|NaN|NAN)" "."))))
