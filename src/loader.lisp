;;;; src/loader.lisp
(in-package #:yaml-kit)

(defun compose (events &key (max-nodes 1000000) (max-depth 1000)
                            (max-alias-expansions 100000) &allow-other-keys)
  "Return the first representation graph in EVENTS."
  (compose-events events :max-nodes max-nodes :max-depth max-depth
                  :max-alias-expansions max-alias-expansions))

(defun compose-all (events &key (max-nodes 1000000) (max-depth 1000)
                                (max-alias-expansions 100000) &allow-other-keys)
  "Return all representation graphs in EVENTS."
  (compose-all-events events :max-nodes max-nodes :max-depth max-depth
                      :max-alias-expansions max-alias-expansions))

(defun %load-events (events &rest options)
  (apply #'construct (apply #'compose events options) options))

(defun parse (input &rest options)
  "Parse the first YAML document from INPUT."
  (apply #'%load-events (if (listp input) input (parse-events input)) options))

(defun parse-all (input &rest options)
  "Parse every YAML document from INPUT."
  (mapcar (lambda (node) (apply #'construct node options))
          (apply #'compose-all (if (listp input) input (parse-events input)) options)))

(defun read-yaml (stream &rest options)
  "Read one YAML document from STREAM."
  (let ((text (with-output-to-string (output)
                (loop for line = (read-line stream nil nil)
                      while line do (write-line line output)))))
    (apply #'parse text options)))
