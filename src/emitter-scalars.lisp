;;;; src/emitter-scalars.lisp
(in-package #:yaml-kit)

(defun %yaml-scalar-line-break-p (character)
  (or (yaml-line-break-p character)
      (case (char-code character)
        ((#x85 #x2028 #x2029) t)
        (otherwise nil))))

(defun %scalar-style-candidates
    (line-breaks flow-indicators block-indicators leading-space leading-break
     trailing-space trailing-break break-space space-break special-characters)
  (let ((flow-plain t) (block-plain t) (single-quoted t) (block t))
    (when (or leading-space leading-break trailing-space trailing-break)
      (setf flow-plain nil block-plain nil))
    (when break-space (setf flow-plain nil block-plain nil single-quoted nil))
    (when (or space-break special-characters)
      (setf flow-plain nil block-plain nil single-quoted nil))
    (when line-breaks (setf flow-plain nil block-plain nil))
    (when flow-indicators (setf flow-plain nil))
    (when block-indicators (setf block-plain nil))
    (list :multiline line-breaks :flow-plain flow-plain
          :block-plain block-plain :single-quoted single-quoted
          :block block)))

(defun %scalar-analysis (value)
  "Return the libyaml-style scalar restrictions for VALUE."
  (if (zerop (length value))
      (list :multiline nil :flow-plain nil :block-plain t
            :single-quoted t :block nil)
      (multiple-value-bind (block-indicators flow-indicators line-breaks
                            special-characters leading-space leading-break
                            trailing-space trailing-break break-space space-break)
          (%scalar-analysis-flags value)
        (%scalar-style-candidates line-breaks flow-indicators block-indicators
                                  leading-space leading-break trailing-space
                                  trailing-break break-space space-break
                                  special-characters))))

(defun %scalar-analysis-indicator-flags (value)
  (let ((length (length value))
        (block-indicators nil)
        (flow-indicators nil))
    (when (and (>= length 3)
               (or (string= value "---" :end1 3)
                   (string= value "..." :end1 3)))
      (setf block-indicators t flow-indicators t))
    (loop for index from 0 below length
          for character = (char value index)
          for preceded-by-whitespace =
            (or (zerop index)
                (yaml-whitespace-p (char value (1- index)))
                (%yaml-scalar-line-break-p (char value (1- index))))
          for followed-by-whitespace =
            (or (= index (1- length))
                (yaml-whitespace-p (char value (1+ index)))
                (%yaml-scalar-line-break-p (char value (1+ index))))
          do (if (zerop index)
                 (when (yaml-indicator-p character)
                   (setf flow-indicators t block-indicators t))
                 (progn
                   (when (or (yaml-flow-indicator-p character)
                             (char= character #\?))
                     (setf flow-indicators t))
                   (when (char= character #\:)
                     (setf flow-indicators t)
                     (when followed-by-whitespace
                       (setf block-indicators t)))
                   (when (and (char= character #\#)
                              preceded-by-whitespace)
                     (setf flow-indicators t block-indicators t)))))
    (values block-indicators flow-indicators)))

(defun %scalar-analysis-content-flags (value)
  (let ((length (length value))
        (line-breaks nil) (special-characters nil)
        (leading-space nil) (leading-break nil)
        (trailing-space nil) (trailing-break nil)
        (break-space nil) (space-break nil)
        (previous-space nil) (previous-break nil))
    (loop for index from 0 below length
          for character = (char value index)
          for blank = (yaml-whitespace-p character)
          for line-break = (%yaml-scalar-line-break-p character)
          do (unless (yaml-printable-p character)
               (setf special-characters t))
             (when line-break (setf line-breaks t))
             (cond
               (blank
                (when (zerop index) (setf leading-space t))
                (when (= index (1- length)) (setf trailing-space t))
                (when previous-break (setf break-space t))
                (setf previous-space t previous-break nil))
               (line-break
                (when (zerop index) (setf leading-break t))
                (when (= index (1- length)) (setf trailing-break t))
                (when previous-space (setf space-break t))
                (setf previous-space nil previous-break t))
               (t (setf previous-space nil previous-break nil))))
    (values line-breaks special-characters leading-space leading-break
            trailing-space trailing-break break-space space-break)))

(defun %scalar-analysis-flags (value)
  "Collect scalar restrictions from VALUE's characters."
  (declare (optimize (speed 3) (safety 1)) (type simple-string value))
  (multiple-value-bind (block-indicators flow-indicators)
      (%scalar-analysis-indicator-flags value)
    (multiple-value-bind (line-breaks special-characters leading-space
                          leading-break trailing-space trailing-break
                          break-space space-break)
        (%scalar-analysis-content-flags value)
      (values block-indicators flow-indicators line-breaks special-characters
              leading-space leading-break trailing-space trailing-break
              break-space space-break))))

(progn
  (defun %yaml-tag-suffix-p (tag suffix)
    (and tag
         (= (length tag) (+ (length +yaml-tag-prefix+) (length suffix)))
         (string= tag +yaml-tag-prefix+ :end1 (length +yaml-tag-prefix+))
         (string= tag suffix :start1 (length +yaml-tag-prefix+))))

  (defun %plain-safe-p (value &key flow tag analysis)
    (let ((analysis (or analysis (%scalar-analysis value))))
      (and (or (if flow (getf analysis :flow-plain) (getf analysis :block-plain))
               (and (or (%yaml-tag-suffix-p tag "int")
                        (%yaml-tag-suffix-p tag "float"))
                    (> (length value) 1)
                    (char= (char value 0) #\-)
                    (not (yaml-whitespace-p (char value 1)))))
           (or (and (%yaml-tag-suffix-p tag "str")
                    (%yaml-tag-suffix-p (resolve-plain-scalar-tag value :core) "str"))
               (and tag
                    (not (%yaml-tag-suffix-p tag "str"))
                    (member tag *implicit-scalar-tags* :test #'string=))
               (or (null tag)
                   (%yaml-tag-suffix-p
                    (resolve-plain-scalar-tag value :core) "str")))))))

(defun %scalar-style (value requested flow &optional tag)
  (let* ((analysis (%scalar-analysis value))
         (style (if (eq requested :plain) :plain requested)))
    (when (and (eq style :plain)
             (not (and (null tag) (getf analysis :multiline)))
               (or (zerop (length value))
                   (not (%plain-safe-p value :flow flow :tag tag :analysis analysis))))
      (setf style :single-quoted))
    (when (and tag
               (getf analysis :multiline)
               (member style '(:plain :single-quoted)))
      (setf style :double-quoted))
    (when (and (eq style :single-quoted)
               (not (getf analysis :single-quoted)))
      (setf style :double-quoted))
    (when (and (member style '(:literal :folded))
               (or flow (not (getf analysis :block))))
      (setf style :double-quoted))
    style))

(defun %hex-digit (value) (schar "0123456789ABCDEF" value))
