(in-package #:yaml-kit)

(defconstant +yaml-tag-prefix+
  (if (boundp '+yaml-tag-prefix+)
      (symbol-value '+yaml-tag-prefix+)
      "tag:yaml.org,2002:"))
