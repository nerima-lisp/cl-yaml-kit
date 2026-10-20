(in-package #:cl-yaml-kit/test)

(defun format-edit-octets (text encoding &optional bom)
  (let ((body (cl-codec-kit:string-to-octets text :encoding encoding)))
    (if bom
        (concatenate '(vector (unsigned-byte 8)) bom body)
        body)))

(defun format-edit-lines (newline trailing-p &rest lines)
  (with-output-to-string (out)
    (loop for line in lines
          for first-p = t then nil
          do (unless first-p (write-string newline out))
             (write-string line out)
          finally (when trailing-p (write-string newline out)))))

(defun format-edit-value-at (source document path)
  (let ((value (nth document (yaml-kit:parse-all source))))
    (dolist (component path value)
      (setf value
            (cond
              ((hash-table-p value) (gethash component value))
              ((vectorp value) (aref value component))
              ((listp value) (nth component value))
              (t (error "cannot descend through ~S" value)))))))

(defun format-edit-present-at (source document path)
  (let ((value (nth document (yaml-kit:parse-all source))))
    (dolist (component path t)
      (cond
        ((hash-table-p value)
         (multiple-value-bind (next presentp) (gethash component value)
           (unless presentp (return-from format-edit-present-at nil))
           (setf value next)))
        ((vectorp value)
         (unless (and (integerp component) (< -1 component (length value)))
           (return-from format-edit-present-at nil))
         (setf value (aref value component)))
        ((listp value)
         (unless (and (integerp component) (< -1 component (length value)))
           (return-from format-edit-present-at nil))
         (setf value (nth component value)))
        (t (return-from format-edit-present-at nil))))))

(defun format-edit-condition-type (thunk)
  (handler-case
      (progn (funcall thunk) nil)
    (yaml-kit:yaml-kit-error (condition)
      (type-of condition))))

(defun format-edit-expect (source path value expected-source
                           &key (document 0) (operation :set) unchanged
                             (unchanged-document document) (check-deleted t))
  (let* ((edited (yaml-kit:edit-source source path value
                                       :document document :operation operation))
         (parsed (yaml-kit:parse-all edited)))
    (expect edited :to-equal expected-source)
    (if (and (eq operation :delete) check-deleted)
        (expect (format-edit-present-at edited document path) :to-be-falsy)
        (unless (eq operation :delete)
          (expect (format-edit-value-at edited document path) :to-equal value)))
    (loop for (sibling-path sibling-value) on unchanged by #'cddr
          do (expect (format-edit-value-at edited unchanged-document sibling-path)
                     :to-equal sibling-value))
    parsed))

(describe "format-preserving editing"
  (it "replaces a scalar without changing comments or quoting outside the value"
    (let ((source (format-edit-lines (string #\Newline) t
                                     "plain: old  # keep" "single: 'old' # quote")))
      (format-edit-expect
       source '("plain") "new"
       (format-edit-lines (string #\Newline) t
                          "plain: new  # keep" "single: 'old' # quote")
       :unchanged (list '("single") "old"))))
  (it "preserves CRLF while replacing a scalar"
    (let ((crlf (format nil "~C~C" #\Return #\Newline)))
      (format-edit-expect
       (format-edit-lines crlf t "a: old" "# keep" "b: 2")
       '("a") "new"
       (format-edit-lines crlf t "a: new" "# keep" "b: 2")
       :unchanged (list '("b") 2))))
  (it "replaces a value in a flow collection and quotes flow indicators"
    (format-edit-expect
     (format-edit-lines (string #\Newline) t "root: {a: old, b: 2} # tail")
     '("root" "a") "x, y"
     (format-edit-lines (string #\Newline) t "root: {a: 'x, y', b: 2} # tail")
     :unchanged (list '("root" "b") 2))
    (format-edit-expect
     (format-edit-lines (string #\Newline) t "items: [a, b]")
     '("items" 0) "x]"
     (format-edit-lines (string #\Newline) t "items: ['x]', b]")
     :unchanged (list '("items" 1) "b")))
  (it "appends a mapping entry in block style and keeps the final newline"
    (format-edit-expect
     (format-edit-lines (string #\Newline) t "a: 1" "b: 2")
     '("c") 3
     (format-edit-lines (string #\Newline) t "a: 1" "b: 2" "c: 3")
       :unchanged (list '("a") 1 '("b") 2)))
  (it "quotes multiline and YAML-boundary strings for inserted values and keys"
    (dolist (value (list (format nil "line1~%line2") "---" "..." "# comment"
                          "- x" "true" "123" "" "null"))
      (let ((edited (yaml-kit:edit-source (format nil "root: 1~%")
                                         (list "key") value)))
        (expect (format-edit-value-at edited 0 '("key")) :to-equal value)
        (expect (format-edit-value-at edited 0 '("root")) :to-equal 1)))
    (dolist (key (list (format nil "line1~%line2") "---" "..." "# comment"
                        "- x" "true" "123" "" "null"))
      (let ((edited (yaml-kit:edit-source (format nil "root: 1~%")
                                         (list key) 2)))
        (expect (format-edit-value-at edited 0 (list key)) :to-equal 2)
        (expect (format-edit-value-at edited 0 '("root")) :to-equal 1))))
  (it "validates additions in non-final documents and nested mappings"
    (let* ((source (format-edit-lines (string #\Newline) t
                                      "---" "a: 1" "..." "---" "b: 2" "..."))
           (edited (yaml-kit:edit-source source '("c") 3 :document 0)))
      (expect (format-edit-value-at edited 0 '("c")) :to-equal 3)
      (expect (format-edit-value-at edited 0 '("a")) :to-equal 1)
      (expect (format-edit-value-at edited 1 '("b")) :to-equal 2))
    (let* ((source (format-edit-lines (string #\Newline) t
                                      "a:" "  b: 1" "c: 2"))
           (edited (yaml-kit:edit-source source '("a" "d") 3)))
      (expect (format-edit-value-at edited 0 '("a" "d")) :to-equal 3)
      (expect (format-edit-value-at edited 0 '("a" "b")) :to-equal 1)
      (expect (format-edit-value-at edited 0 '("c")) :to-equal 2)))
  (it "keeps trailing comment blocks, empty-value deletion, and the first newline style"
    (let* ((source (format-edit-lines (string #\Newline) t "a: 1" "# tail" "# keep"))
           (edited (yaml-kit:edit-source source '("b") 2)))
      (expect (format-edit-value-at edited 0 '("a")) :to-equal 1)
      (expect (format-edit-value-at edited 0 '("b")) :to-equal 2)
      (expect (search "# tail" edited) :to-be-truthy)
      (expect (search "# keep" edited) :to-be-truthy))
    (let ((source (format-edit-lines (string #\Newline) t "a: 1" "b:" "c: 3")))
      (let ((edited (yaml-kit:edit-source source '("b") nil :operation :delete)))
        (expect (format-edit-present-at edited 0 '("b")) :to-be-falsy)
        (expect (format-edit-value-at edited 0 '("a")) :to-equal 1)
        (expect (format-edit-value-at edited 0 '("c")) :to-equal 3)))
    (let* ((crlf (format nil "~C~C" #\Return #\Newline))
           (source (concatenate 'string "a: 1" crlf "b: 2" (string #\Newline)))
           (edited (yaml-kit:edit-source source '("c") 3)))
      (expect edited :to-equal (concatenate 'string "a: 1" crlf
                                            "b: 2" (string #\Newline)
                                            "c: 3" crlf))))
  (it "appends a sequence item in block style and keeps the final newline"
    (format-edit-expect
     (format-edit-lines (string #\Newline) t "items:" "  - one" "  - two # keep")
     '("items" 2) "three"
     (format-edit-lines (string #\Newline) t
                        "items:" "  - one" "  - two # keep" "  - three")
     :unchanged (list '("items" 0) "one" '("items" 1) "two")))
  (it "deletes a mapping entry and its inline comment"
    (format-edit-expect
     (format-edit-lines (string #\Newline) t "a: 1" "b: 2  # remove" "c: 3")
     '("b") nil
     (format-edit-lines (string #\Newline) t "a: 1" "c: 3")
     :operation :delete
     :unchanged (list '("a") 1 '("c") 3)))
  (it "deletes a sequence item without changing neighboring lines"
    (format-edit-expect
     (format-edit-lines (string #\Newline) t
                        "items:" "  - one" "  - two # remove" "  - three")
     '("items" 1) nil
     (format-edit-lines (string #\Newline) t "items:" "  - one" "  - three")
     :operation :delete
     :check-deleted nil
     :unchanged (list '("items" 0) "one" '("items" 1) "three")))
  (it "selects a document by zero-based number"
    (format-edit-expect
     (format-edit-lines (string #\Newline) t
                        "---" "a: first" "..." "--- # second" "a: old" "...")
     '("a") "new"
     (format-edit-lines (string #\Newline) t
                        "---" "a: first" "..." "--- # second" "a: new" "...")
     :document 1
     :unchanged-document 0
     :unchanged (list '("a") "first")))
  (it "preserves a UTF-8 BOM in octet input"
    (let* ((bom #(239 187 191))
           (source (format-edit-octets
                    (format-edit-lines (format nil "~C~C" #\Return #\Newline)
                                       t "a: old") :utf-8 bom))
           (edited (yaml-kit:edit-source source '("a") "new")))
      (expect (equalp edited
                      (format-edit-octets
                       (format-edit-lines (format nil "~C~C" #\Return #\Newline)
                                          t "a: new") :utf-8 bom))
              :to-be-truthy)
      (expect (format-edit-value-at edited 0 '("a")) :to-equal "new")))
  (it "preserves detected UTF encodings for octet input"
    (dolist
        (case (list (list :utf-8 nil)
                    (list :utf-16be #(254 255))
                    (list :utf-32le #(255 254 0 0))
                    (list :utf-32be #(0 0 254 255))
                    (list :utf-16be nil)
                    (list :utf-16le nil)
                    (list :utf-16le #(255 254))))
      (destructuring-bind (encoding bom) case
        (let* ((source
                (format-edit-octets
                 (format-edit-lines (string (code-char 10)) t "a: old")
                 encoding
                 bom))
               (edited (yaml-kit:edit-source source (list "a") "new"))
               (expected
                (format-edit-octets
                 (format-edit-lines (string (code-char 10)) t "a: new")
                 encoding
                 bom)))
          (expect (equalp edited expected) :to-be-truthy)
          (expect (format-edit-value-at edited 0 (list "a")) :to-equal "new")))))
  (it "rejects collection, empty, block, multiline, and tagged scalar sets"
    (dolist (case (list
                   (list (format-edit-lines (string #\Newline) t "a:" "  x: 1" "b: 2")
                         '("a") "new")
                   (list (format-edit-lines (string #\Newline) t "a: |" "  line1" "b: 2")
                         '("a") "new")
                   (list (format-edit-lines (string #\Newline) t "a: one" "  two" "b: 2")
                         '("a") "new")
                   (list (format-edit-lines (string #\Newline) t "a:" "b: 2")
                         '("a") "new")
                   (list (format-edit-lines (string #\Newline) t "a: !!str 5" "b: 2")
                         '("a") "new")))
      (destructuring-bind (source path value) case
        (expect (handler-case
                    (progn (yaml-kit:edit-source source path value) nil)
                  (yaml-kit:yaml-format-edit-structure-error () t))
                :to-be-truthy))))
  (it "rejects mappings and sequences as replacement values"
    (dolist (value (list (yaml-kit:make-yaml-mapping (list (cons "x" 1)))
                         (list 1 2)))
      (expect (handler-case
                  (progn (yaml-kit:edit-source
                          (format-edit-lines (string #\Newline) t "a: old")
                          '("a") value) nil)
                (yaml-kit:yaml-format-edit-structure-error () t))
              :to-be-truthy)))
  (it "rejects structural edits that share a line with a sequence marker"
    (let ((source (format-edit-lines (string #\Newline) t
                                     "- a: 1" "  b: 2" "- c: 3")))
      (dolist (case (list (list '(0 "z") 9 :set)
                          (list '(0 "a") nil :delete)))
        (destructuring-bind (path value operation) case
          (expect (handler-case
                      (progn (yaml-kit:edit-source source path value :operation operation) nil)
                    (yaml-kit:yaml-format-edit-structure-error () t))
                  :to-be-truthy))))
    (let ((source (format-edit-lines (string #\Newline) t
                                     "- - a" "  - b" "- c")))
      (dolist (case (list (list '(0 2) "z" :set)
                          (list '(0 0) nil :delete)))
        (destructuring-bind (path value operation) case
          (expect (handler-case
                      (progn (yaml-kit:edit-source source path value :operation operation) nil)
                    (yaml-kit:yaml-format-edit-structure-error () t))
                  :to-be-truthy)))))
  (it "rejects edits through unsafe tagged and anchored nodes"
    (dolist
        (case (list
               (list (format-edit-lines (string #\Newline) t "a: !!str 5")
                     (list "a")
                     "YAML-FORMAT-EDIT-STRUCTURE-ERROR")
               (list (format-edit-lines (string #\Newline) t "a: 1" "a: 2")
                     (list "a")
                     "YAML-FORMAT-EDIT-STRUCTURE-ERROR")
               (list
                (format-edit-lines (string #\Newline)
                                   t
                                   "base: &b {x: 1}"
                                   "d:"
                                   "  <<: *b"
                                   "  y: 2")
                (list "d" "y")
                "YAML-FORMAT-EDIT-STRUCTURE-ERROR")
               (list (format-edit-lines (string #\Newline) t "a: 1")
                     (list 0)
                     "YAML-FORMAT-EDIT-PATH-ERROR")
               (list
                (format-edit-lines (string #\Newline) t "root: !!map {a: 1}")
                (list "root" "a")
                "YAML-FORMAT-EDIT-STRUCTURE-ERROR")
               (list (format-edit-lines (string #\Newline) t "root: &x {a: 1}")
                     (list "root" "a")
                     "YAML-FORMAT-EDIT-ANCHOR-ERROR")
               (list (format-edit-lines (string #\Newline) t "&key a: 1")
                     (list "a")
                     "YAML-FORMAT-EDIT-ANCHOR-ERROR")))
      (destructuring-bind (source path expected-type) case
        (expect
         (symbol-name
          (format-edit-condition-type
           (lambda ()
             (yaml-kit:edit-source source path "new"))))
         :to-equal
         expected-type))))
  (it "rejects only-child deletion, flow deletion, and non-tail sequence addition"
    (dolist (case (list
                   (list (format-edit-lines (string #\Newline) t "a: 1")
                         '("a") nil :delete)
                   (list (format-edit-lines (string #\Newline) t "root: [a, b]")
                         '("root" 0) nil :delete)
                   (list (format-edit-lines (string #\Newline) t "items:" "  - a" "  - b")
                         '("items" 3) "x" :set)))
      (destructuring-bind (source path value operation) case
        (expect (format-edit-condition-type
                 (lambda ()
                   (yaml-kit:edit-source source path value :operation operation)))
                :to-equal 'yaml-kit:yaml-format-edit-structure-error))))
  (it "rejects anchors and aliases but allows an unrelated anchored sibling"
    (let ((source (format-edit-lines (string #\Newline) t
                                     "base: &base 1" "other: 2")))
      (expect (handler-case
                  (progn (yaml-kit:edit-source source '("base") 3) nil)
                (yaml-kit:yaml-format-edit-anchor-error () t))
              :to-be-truthy)
      (format-edit-expect source '("other") 3
                           (format-edit-lines (string #\Newline) t
                                              "base: &base 1" "other: 3"))))
  (it "rejects alias replacement and deletion, including merge aliases"
    (dolist (operation (list :set :delete))
      (expect (handler-case
                  (progn (yaml-kit:edit-source
                          (format-edit-lines (string #\Newline) t "a: &x 1" "b: *x")
                                               '("b") 2 :operation operation)
                         nil)
                (yaml-kit:yaml-format-edit-anchor-error () t))
              :to-be-truthy)))
  (it "rejects a deletion that changes an unrelated alias value"
    (let ((source (format-edit-lines (string #\Newline) t
                                     "a: &x 1" "b:" "  c: &x 2" "d: *x")))
      (expect (format-edit-condition-type
               (lambda () (yaml-kit:edit-source source '("b") nil
                                                :operation :delete)))
              :to-equal 'yaml-kit:yaml-format-edit-structure-error)))
  (it "preserves final newline for CRLF and no-final-newline additions"
    (let ((crlf (format nil "~C~C" #\Return #\Newline)))
      (format-edit-expect (format-edit-lines crlf t "a: 1" "b: 2")
                          '("c") 3
                          (format-edit-lines crlf t "a: 1" "b: 2" "c: 3"))
      (format-edit-expect (format-edit-lines crlf nil "a: 1" "b: 2")
                          '("c") 3
                          (format-edit-lines crlf nil "a: 1" "b: 2" "c: 3"))))
  (it "converts invalid octets to a YAML kit condition and accepts base strings"
    (let ((octets (coerce #(255 0 1) '(vector (unsigned-byte 8)))))
      (expect (format-edit-condition-type
               (lambda () (yaml-kit:edit-source octets '("a") 1
                                                :document 4 :operation :delete)))
              :to-equal 'yaml-kit:yaml-parse-error))
    (expect (format-edit-condition-type
             (lambda () (yaml-kit:edit-source 42 '("a") 1
                                              :document 4 :operation :delete)))
            :to-equal 'yaml-kit:yaml-format-edit-path-error)
    (expect (handler-case
                (yaml-kit:edit-source 42 '("a") 1
                                      :document 4 :operation :delete)
              (yaml-kit:yaml-format-edit-path-error (condition)
                (and (= (yaml-kit:yaml-format-edit-path-error-document condition) 4)
                     (eq (yaml-kit:yaml-format-edit-path-error-operation condition)
                         :delete))))
            :to-be-truthy)
    (format-edit-expect (format nil "a: old~%b: 2~%") '("a") "new"
                         (format nil "a: new~%b: 2~%")
                         :unchanged (list '("b") 2)))
  (cl-weave:it-each
    ((apostrophe "O'Brien, Jr" "'O''Brien, Jr'")
     (colon-space "key: value" "'key: value'")
     (colon-question "key:?value" "'key:?value'")
     (empty "" "''")
     (safe-colon "name:value" "name:value")
     (closing-bracket "x]y" "'x]y'"))
  "quotes YAML-dangerous flow value case ~S"
  (name value fragment)
  (declare (ignore name))
  (format-edit-expect
   (format nil "root: [old]~%") '("root" 0) value
   (format nil "root: [~A]~%" fragment)))
  (it "escapes controls and Unicode line separators in replacement strings"
    (let* ((codes '(0 7 8 9 10 11 12 13 27 34 92 133 8232 8233 1 127 65534))
           (escape-codes '("0" "a" "b" "t" "n" "v" "f" "r" "e" "\"" "\\"
                           "N" "L" "P" "x01" "x7F" "uFFFE"))
           (value (coerce (mapcar #'code-char codes) 'string))
           (fragment
             (with-output-to-string (out)
               (write-char #\" out)
               (loop for escape in escape-codes
                     do (write-char #\\ out)
                        (write-string escape out))
               (write-char #\" out)))
           (edited (yaml-kit:edit-source (format nil "value: old~%")
                                         '("value") value)))
      (expect (length codes) :to-equal (length escape-codes))
      (expect (yaml-kit::%format-edit-double-quoted-fragment value) :to-equal fragment)
      (expect (format-edit-value-at edited 0 '("value")) :to-equal value)))
  (it "copies semantic YAML mappings recursively"
    (let* ((mapping (yaml-kit:make-yaml-mapping
                     (list (cons "a" 1) (cons "items" (list 10 20)))))
           (copy (yaml-kit::%format-edit-semantic-copy mapping)))
      (expect (yaml-kit:yaml-mapping-entries copy)
              :to-equal (yaml-kit:yaml-mapping-entries mapping))
      (expect (eq copy mapping) :to-be-falsy)
      (expect (eq (cdr (assoc "items" (yaml-kit:yaml-mapping-entries copy)
                              :test #'equal))
                  (cdr (assoc "items" (yaml-kit:yaml-mapping-entries mapping)
                              :test #'equal)))
              :to-be-falsy)))
  (it "replaces, adds, deletes, and validates semantic mapping entries"
    (let* ((mapping (yaml-kit:make-yaml-mapping
                     (list (cons "a" 1) (cons "items" (list 10 20)))))
           (original (yaml-kit:yaml-mapping-entries mapping))
           (added-entries (append original (list (cons "c" 3))))
           (replaced (yaml-kit::%format-edit-semantic-replace-child
                      mapping "a" 9 nil))
           (added (yaml-kit::%format-edit-semantic-add-child
                   mapping "c" 3 nil))
           (deleted (yaml-kit::%format-edit-semantic-delete-child
                     mapping "a" nil))
           (expected (yaml-kit::%format-edit-semantic-expected
                      (list mapping) '("c") 3 :add 0)))
      (expect (mapcar #'yaml-kit:yaml-mapping-entries
                      (list replaced added deleted (first expected)))
              :to-equal
              (list (list (cons "a" 9) (cons "items" (list 10 20)))
                    added-entries
                    (list (cons "items" (list 10 20)))
                    added-entries))
      (expect (yaml-kit:yaml-mapping-entries mapping) :to-equal original)))
  (it "replaces, appends, and deletes semantic list items"
    (let* ((sequence '(10 20))
           (replaced (yaml-kit::%format-edit-semantic-replace-child
                      sequence 1 99 nil))
           (added (yaml-kit::%format-edit-semantic-add-child sequence 2 30 nil))
           (deleted (yaml-kit::%format-edit-semantic-delete-child sequence 0 nil)))
      (expect (list replaced added deleted) :to-equal '((10 99) (10 20 30) (20)))
      (expect sequence :to-equal '(10 20))))
  (it "reports presence for semantic mapping and list child lookups"
    (let ((mapping (yaml-kit:make-yaml-mapping (list (cons "a" 1))))
          (sequence '(10 20)))
      (expect (multiple-value-list
               (yaml-kit::%format-edit-semantic-child mapping "a"))
              :to-equal '(1 t))
      (expect (multiple-value-list
               (yaml-kit::%format-edit-semantic-child sequence 1))
              :to-equal '(20 t))
      (expect (multiple-value-list
               (yaml-kit::%format-edit-semantic-child 10 "a"))
              :to-equal '(nil nil))))
  (it "compares semantic YAML mappings by entries"
    (let* ((mapping (yaml-kit:make-yaml-mapping
                     (list (cons "a" 1) (cons "b" 2))))
           (copy (yaml-kit::%format-edit-semantic-copy mapping))
           (changed (yaml-kit::%format-edit-semantic-add-child
                     mapping "c" 3 nil)))
      (expect (yaml-kit::%format-edit-semantic-equal-p mapping copy)
              :to-be-truthy)
      (expect (yaml-kit::%format-edit-semantic-equal-p mapping changed)
              :to-be-falsy)))
  (it "compares semantic lists and scalar leaves"
    (let ((sequence '(10 20)))
      (expect (yaml-kit::%format-edit-semantic-equal-p sequence
                                                        (copy-list sequence))
              :to-be-truthy)
      (expect (yaml-kit::%format-edit-semantic-equal-p sequence #(10 20))
              :to-be-falsy)
      (expect (list (yaml-kit::%format-edit-semantic-equal-p 3 3.0)
                    (yaml-kit::%format-edit-semantic-equal-p 3 4))
              :to-equal '(t nil))))
  (cl-weave:it-each
    ((big-endian :utf-32be)
     (little-endian :utf-32le))
  "detects BOM-less ~S input and preserves its bytes"
  (name encoding)
  (declare (ignore name))
  (let* ((source (format-edit-octets (format nil "a: old~%") encoding))
         (edited (yaml-kit:edit-source source '("a") "new"))
         (expected (format-edit-octets (format nil "a: new~%") encoding)))
    (expect (equalp edited expected) :to-be-truthy)
    (expect (format-edit-value-at edited 0 '("a")) :to-equal "new"))))
