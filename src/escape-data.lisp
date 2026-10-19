(in-package #:yaml-kit)

(defparameter *yaml-escape-table*
  (let ((table (make-hash-table :test 'eql)))
    (setf (gethash #\Null table) "\\0"
          (gethash #\Bell table) "\\a"
          (gethash #\Backspace table) "\\b"
          (gethash #\Tab table) "\\t"
          (gethash #\Newline table) "\\n"
          (gethash #\Vt table) "\\v"
          (gethash #\Page table) "\\f"
          (gethash #\Return table) "\\r"
          (gethash #\Escape table) "\\e"
          (gethash #\" table) "\\\""
          (gethash #\\ table) "\\\\"
          (gethash (code-char #x85) table) "\\N"
          (gethash (code-char #xa0) table) "\\_"
          (gethash (code-char #x2028) table) "\\L"
          (gethash (code-char #x2029) table) "\\P")
    table))
