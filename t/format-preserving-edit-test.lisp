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
  (it "preserves a UTF-16LE BOM in octet input"
    (let* ((bom #(255 254))
           (source (format-edit-octets
                    (format-edit-lines (format nil "~C~C" #\Return #\Newline)
                                       t "a: old") :utf-16le bom))
           (edited (yaml-kit:edit-source source '("a") "new")))
      (expect (equalp edited
                      (format-edit-octets
                       (format-edit-lines (format nil "~C~C" #\Return #\Newline)
                                          t "a: new") :utf-16le bom))
              :to-be-truthy)
      (expect (format-edit-value-at edited 0 '("a")) :to-equal "new")))
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
  (it "rejects tags, duplicate keys, merge keys, and integer map paths"
    (dolist (case (list
                   (list (format-edit-lines (string #\Newline) t "a: !!str 5")
                         '("a") "new")
                   (list (format-edit-lines (string #\Newline) t "a: 1" "a: 2")
                         '("a") 3)
                   (list (format-edit-lines (string #\Newline) t
                                            "base: &b {x: 1}" "d:" "  <<: *b" "  y: 2")
                         '("d" "y") 3)
                   (list (format-edit-lines (string #\Newline) t "a: 1")
                         '(0) 2)))
      (destructuring-bind (source path value) case
        (expect (handler-case
                    (progn (yaml-kit:edit-source source path value) nil)
                  (yaml-kit:yaml-kit-error () t))
                :to-be-truthy))))
  (it "rejects only-child deletion, flow deletion, and non-tail sequence addition"
    (dolist (case (list
                   (list (format-edit-lines (string #\Newline) t "a: 1")
                         '("a") nil :delete)
                   (list (format-edit-lines (string #\Newline) t "root: [a, b]")
                         '("root" 0) nil :delete)
                   (list (format-edit-lines (string #\Newline) t "items:" "  - a" "  - b")
                         '("items" 3) "x" :set)))
      (destructuring-bind (source path value operation) case
        (expect (handler-case
                    (progn (yaml-kit:edit-source source path value :operation operation) nil)
                  (yaml-kit:yaml-kit-error () t))
                :to-be-truthy))))
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
  (it "preserves final newline for CRLF and no-final-newline additions"
    (let ((crlf (format nil "~C~C" #\Return #\Newline)))
      (format-edit-expect (format-edit-lines crlf t "a: 1" "b: 2")
                          '("c") 3
                          (format-edit-lines crlf t "a: 1" "b: 2" "c: 3"))
      (format-edit-expect (format-edit-lines crlf nil "a: 1" "b: 2")
                          '("c") 3
                          (format-edit-lines crlf nil "a: 1" "b: 2" "c: 3"))))
  (it "converts invalid octets to a YAML kit condition and accepts base strings"
    (expect (handler-case
                (progn (yaml-kit:edit-source #(255 0 1) '("a") 1) nil)
              (yaml-kit:yaml-kit-error () t))
            :to-be-truthy)
    (format-edit-expect (format nil "a: old~%b: 2~%") '("a") "new"
                         (format nil "a: new~%b: 2~%")
                         :unchanged (list '("b") 2))))
