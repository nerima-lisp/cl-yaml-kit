;;;; t/helpers.lisp
(in-package #:cl-yaml-kit/test)

(defparameter *default-test-timeout-ms* 120000)
(defun %yaml-active-pair-p (active-pairs left right)
  (member right (gethash left active-pairs) :test (function eq)))

(defun %yaml-with-active-pair (active-pairs left right thunk)
  (if (%yaml-active-pair-p active-pairs left right)
      t
      (progn
        (push right (gethash left active-pairs))
        (unwind-protect
             (funcall thunk)
          (let ((remaining
                  (remove right (gethash left active-pairs)
                          :test (function eq)
                          :count 1)))
            (if remaining
                (setf (gethash left active-pairs) remaining)
                (remhash left active-pairs)))))))

(defun %yaml-hash-entries (table)
  (loop for key being the hash-keys of table using (hash-value value)
        collect (cons key value)))

(defun %yaml-match-hash-entries
    (left-entries right-entries used active-pairs)
  (if (null left-entries)
      t
      (loop for index below (length right-entries)
            thereis
              (and (not (aref used index))
                   (let ((candidate (aref right-entries index)))
                     (and
                      (%yaml-value-equal-in-context
                       (caar left-entries) (car candidate) active-pairs)
                      (%yaml-value-equal-in-context
                       (cdar left-entries) (cdr candidate) active-pairs)
                      (setf (aref used index) t)
                      (let ((matched
                              (%yaml-match-hash-entries
                               (cdr left-entries) right-entries used active-pairs)))
                        (unless matched
                          (setf (aref used index) nil))
                        matched)))))))

(defun %yaml-hash-table-equal-p (left right active-pairs)
  (and (= (hash-table-count left) (hash-table-count right))
       (%yaml-with-active-pair
        active-pairs left right
        (lambda ()
          (%yaml-match-hash-entries
           (%yaml-hash-entries left)
           (coerce (%yaml-hash-entries right) (quote vector))
           (make-array (hash-table-count right) :initial-element nil)
           active-pairs)))))

(defun %yaml-mapping-equal-p (left right active-pairs)
  (and (yaml-kit:yaml-mapping-p left)
       (yaml-kit:yaml-mapping-p right)
       (%yaml-with-active-pair
        active-pairs left right
        (lambda ()
          (%yaml-value-equal-in-context
           (yaml-kit:yaml-mapping-entries left)
           (yaml-kit:yaml-mapping-entries right)
           active-pairs)))))

(defun %yaml-cons-equal-p (left right active-pairs)
  (and (consp left)
       (consp right)
       (%yaml-with-active-pair
        active-pairs left right
        (lambda ()
          (and (%yaml-value-equal-in-context (car left) (car right) active-pairs)
               (%yaml-value-equal-in-context (cdr left) (cdr right) active-pairs))))))

(defun %yaml-vector-equal-p (left right active-pairs)
  (and (vectorp left)
       (vectorp right)
       (%yaml-with-active-pair
        active-pairs left right
        (lambda ()
          (and (= (length left) (length right))
               (loop for index below (length left)
                     always
                       (%yaml-value-equal-in-context
                        (aref left index) (aref right index) active-pairs)))))))

(defun %yaml-float-equal-p (left right)
  (and (floatp left)
       (floatp right)
       (eq (type-of left) (type-of right))
       (let ((left-nan-p (yaml-kit::%float-nan-p left))
             (right-nan-p (yaml-kit::%float-nan-p right)))
         (if (or left-nan-p right-nan-p)
             (and left-nan-p right-nan-p)
             (and (= left right)
                  (or (not (zerop left))
                      (eql (sb-kernel:float-sign-bit-set-p left)
                           (sb-kernel:float-sign-bit-set-p right))))))))

(defun %yaml-value-equal-in-context (left right active-pairs)
  (cond
    ((or (yaml-kit:yaml-null-p left)
         (yaml-kit:yaml-null-p right)
         (yaml-kit:yaml-false-p left)
         (yaml-kit:yaml-false-p right))
     (eq left right))
    ((eq left right) t)
    ((or (stringp left) (stringp right))
     (and (stringp left) (stringp right) (string= left right)))
    ((or (yaml-kit:yaml-mapping-p left)
         (yaml-kit:yaml-mapping-p right))
     (%yaml-mapping-equal-p left right active-pairs))
    ((or (hash-table-p left) (hash-table-p right))
     (and (hash-table-p left)
          (hash-table-p right)
          (%yaml-hash-table-equal-p left right active-pairs)))
    ((or (consp left) (consp right))
     (%yaml-cons-equal-p left right active-pairs))
    ((or (vectorp left) (vectorp right))
     (%yaml-vector-equal-p left right active-pairs))
    ((or (floatp left) (floatp right))
     (%yaml-float-equal-p left right))
    (t (eql left right))))

(defun %yaml-value-equal-p (actual expected)
  (%yaml-value-equal-in-context
   actual expected (make-hash-table :test (function eq))))
(defun %yaml-single-matcher-value (expected)
  (if (and (consp expected)
           (null (cdr expected)))
      (values (car expected) t)
      (values nil nil)))
(cl-weave:defmatcher :to-parse-as (actual expected)
  "Compares ACTUAL parsed as YAML with one expected value."
  (let ((parsed (yaml-kit:parse actual)))
    (multiple-value-bind (expected-value expected-valid-p)
        (%yaml-single-matcher-value expected)
      (values (and expected-valid-p
                   (%yaml-value-equal-p parsed expected-value))
              parsed
              (if expected-valid-p expected-value expected)))))
(cl-weave:defmatcher :to-emit-as (actual expected)
  "Compares the YAML emitted from ACTUAL with one expected string."
  (let ((emitted (yaml-kit:emit actual)))
    (multiple-value-bind (expected-value expected-valid-p)
        (%yaml-single-matcher-value expected)
      (values (and expected-valid-p
                   (stringp expected-value)
                   (string= emitted expected-value))
              emitted
              (if expected-valid-p expected-value expected)))))
(cl-weave:defmatcher :to-round-trip (actual expected)
  "Checks that an emit/parse round trip preserves an emit-able ACTUAL YAML value."
  (declare (ignore expected))
  (let ((round-tripped (yaml-kit:parse (yaml-kit:emit actual))))
    (values (%yaml-value-equal-p round-tripped actual)
            round-tripped
            actual)))
(cl-weave:defmatcher :to-signal-yaml-error (actual expected)
  "Checks that parsing ACTUAL signals the single expected YAML condition type."
  (let ((condition
          (handler-case
              (progn (yaml-kit:parse actual) nil)
            (yaml-kit:yaml-kit-error (condition) condition))))
    (multiple-value-bind (expected-type expected-valid-p)
        (%yaml-single-matcher-value expected)
      (values (and condition
                   expected-valid-p
                   (typep condition expected-type))
              condition
              (if expected-valid-p expected-type expected)))))
(defmacro it-each (cases suite-name bindings &body body)
  "Expands CASES as tests; both lists must be proper and equal-length; SUITE-NAME must contain ~S."
  (labels ((proper-list-length (value)
             (handler-case
                 (values (list-length value) t)
               (type-error () (values nil nil)))))
    (unless (and (stringp suite-name) (search "~S" suite-name))
      (error "IT-EACH suite name must be a string containing ~~S."))
    (multiple-value-bind (binding-count bindings-proper-p)
        (proper-list-length bindings)
      (unless (and bindings-proper-p (integerp binding-count))
        (error "IT-EACH bindings must be a proper list."))
      (multiple-value-bind (case-count cases-proper-p)
          (proper-list-length cases)
        (unless (and cases-proper-p (integerp case-count))
          (error "IT-EACH cases must be a proper list.")))
      (dolist (case-form cases)
        (multiple-value-bind (case-count case-proper-p)
            (proper-list-length case-form)
          (unless (and case-proper-p
                       (integerp case-count)
                       (= case-count binding-count))
            (error "IT-EACH case ~S must be a proper list with ~D values."
                   case-form binding-count)))))
    (append (list (quote cl-weave:it-each) cases suite-name bindings) body)))

(defun unit-run-options ()
  "cl-weave's CL_WEAVE_* settings, with a per-test timeout and an empty run failing."
  (let ((options (cl-weave/cli:run-all-options-from-environment)))
    (setf (getf options :pass-with-no-tests) nil)
    (unless (getf options :timeout-ms)
      (setf (getf options :timeout-ms) *default-test-timeout-ms*))
    options))

(defun run-tests ()
  (setf *conformance-last-summaries* nil)
  (let ((unit-tests-passed
          (apply (function cl-weave:run-all) (unit-run-options)))
        (conformance-passed nil))
    (unless *conformance-last-summaries*
      (multiple-value-bind (summaries passedp)
          (conformance-run-report)
        (declare (ignore summaries))
        (setf conformance-passed passedp)))
    (when *conformance-last-summaries*
      (setf conformance-passed
            (every (lambda (summary)
                    (and (zerop (getf summary :failed))
                         (zerop (getf summary :drift))))
                   *conformance-last-summaries*)))
    (and unit-tests-passed conformance-passed)))
