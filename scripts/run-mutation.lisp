(require :asdf)
(asdf:load-asd
 (merge-pathnames "../cl-yaml-kit.asd"
                  (uiop:pathname-directory-pathname *load-truename*)))
(asdf:load-asd
 (merge-pathnames "cl-weave/cl-weave.asd"
                  (pathname (uiop:getenv "CL_YAML_DEPS"))))
(asdf:load-system "cl-yaml-kit")
(asdf:load-system "cl-weave")

(defpackage #:yaml-kit/mutation (:use #:cl))
(in-package #:yaml-kit/mutation)

(defun mutation-report (name form probe expected)
  (let* ((results
           (cl-weave:run-mutations
            form
            (lambda (mutated mutation)
              (declare (ignore mutation))
              (eval mutated)
              (equal expected (funcall probe)))
            :timeout-ms 5000))
         (summary (cl-weave:mutation-summary results)))
    (format t "~A ~S~%" name summary)
    (dolist (result results)
      (when (member (cl-weave:mutation-result-status result)
                    '(:survived :errored))
        (format t "  ~A ~S~%"
                (cl-weave:mutation-result-status result)
                (cl-weave:mutation-result-mutation result))))
    summary))

(defun main ()
  (let ((reports
          (list
           (mutation-report
            "scanner-character-class"
            '(defun yaml-kit::mutation-scanner-digit (character)
               (if (char<= #\0 character #\9) t nil))
            (lambda () (list (funcall 'yaml-kit::mutation-scanner-digit #\1)
                             (funcall 'yaml-kit::mutation-scanner-digit #\１)))
            '(t nil))
           (mutation-report
            "schema-scalar-resolution"
            '(defun yaml-kit::mutation-schema-scalar (text)
               (if (string= text "true")
                   "tag:yaml.org,2002:bool"
                   "tag:yaml.org,2002:str"))
            (lambda () (funcall 'yaml-kit::mutation-schema-scalar "true"))
            "tag:yaml.org,2002:bool"))))
    (format t "mutation-total ~D killed ~D survived ~D errored ~D~%"
            (reduce #'+ reports :key (lambda (summary) (getf summary :total)))
            (reduce #'+ reports :key (lambda (summary) (getf summary :killed)))
            (reduce #'+ reports :key (lambda (summary) (getf summary :survived)))
            (reduce #'+ reports :key (lambda (summary) (getf summary :errored))))))

(main)
