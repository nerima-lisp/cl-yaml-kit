;;;; src/emitter-scalars.lisp
(in-package #:yaml-kit)

(defparameter +yaml-indicator-characters+ "-?:,[]{}#&*!|>'\"%@`")
(defparameter +implicit-scalar-tags+
  '("tag:yaml.org,2002:int" "tag:yaml.org,2002:float"
    "tag:yaml.org,2002:bool" "tag:yaml.org,2002:null"
    "tag:yaml.org,2002:str"))

(defun %yaml-line-break-p (character)
  (case (char-code character)
    ((#x0a #x0d #x85 #x2028 #x2029) t)
    (otherwise nil)))

(defun %yaml-printable-character-p (character)
  (let ((code (char-code character)))
    (or (= code #x09) (= code #x0a) (= code #x0d)
        (<= #x20 code #x7e) (= code #x85)
        (<= #xa0 code #xd7ff) (<= #xe000 code #xfffd)
        (<= #x10000 code #x10ffff))))

(defun %yaml-blank-p (character)
  (or (char= character #\Space) (char= character #\Tab)))

(defparameter +scalar-character-predicates+
  '((:blank . %yaml-blank-p)
    (:line-break . %yaml-line-break-p)
    (:printable . %yaml-printable-character-p)))

(defun %scalar-character-properties (character)
  (loop for (property . predicate) in +scalar-character-predicates+
        when (funcall predicate character)
          collect property))

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

(defun %scalar-analysis-flags (value)
  "Collect scalar restrictions from VALUE's characters."
  (declare (optimize (speed 3) (safety 1)) (type simple-string value))
  (let ((length (length value))
        (block-indicators nil) (flow-indicators nil)
        (line-breaks nil) (special-characters nil)
        (leading-space nil) (leading-break nil)
        (trailing-space nil) (trailing-break nil)
        (break-space nil) (space-break nil)
        (previous-space nil) (previous-break nil))
    (when (or (and (>= length 3) (string= value "---" :end1 3))
              (and (>= length 3) (string= value "..." :end1 3)))
      (setf block-indicators t flow-indicators t))
    (loop for index from 0 below length
          for character = (char value index)
          for properties = (%scalar-character-properties character)
          for preceded-by-whitespace = (or (zerop index)
                                           (member :blank (%scalar-character-properties
                                                           (char value (1- index))))
                                           (member :line-break (%scalar-character-properties
                                                                (char value (1- index)))))
          for followed-by-whitespace = (or (= index (1- length))
                                           (member :blank (%scalar-character-properties
                                                           (char value (1+ index))))
                                           (member :line-break (%scalar-character-properties
                                                                (char value (1+ index)))))
          do
             (if (zerop index)
                 (cond
                   ((find character +yaml-indicator-characters+)
                    (setf flow-indicators t block-indicators t))
                   )
                 (progn
                   (when (find character ",?[]{}") (setf flow-indicators t))
                   (when (char= character #\:)
                     (setf flow-indicators t)
                     (when followed-by-whitespace (setf block-indicators t)))
                   (when (and (char= character #\#) preceded-by-whitespace)
                     (setf flow-indicators t block-indicators t))))
             (unless (member :printable properties)
               (setf special-characters t))
             (when (member :line-break properties) (setf line-breaks t))
             (cond
               ((member :blank properties)
                (when (zerop index) (setf leading-space t))
                (when (= index (1- length)) (setf trailing-space t))
                (when previous-break (setf break-space t))
                (setf previous-space t previous-break nil))
               ((member :line-break properties)
                (when (zerop index) (setf leading-break t))
                (when (= index (1- length)) (setf trailing-break t))
                (when previous-space (setf space-break t))
                (setf previous-space nil previous-break t))
               (t (setf previous-space nil previous-break nil))))
    (values block-indicators flow-indicators line-breaks special-characters
            leading-space leading-break trailing-space trailing-break
            break-space space-break)))

(defun %plain-safe-p (value &key flow tag analysis)
  (let ((analysis (or analysis (%scalar-analysis value))))
    (and (or (if flow (getf analysis :flow-plain) (getf analysis :block-plain))
             (and tag
                  (member tag '("tag:yaml.org,2002:int"
                                "tag:yaml.org,2002:float") :test #'string=)
                  (> (length value) 1)
                  (char= (char value 0) #\-)
                  (not (%yaml-blank-p (char value 1)))))
         (or (and tag
                  (string= tag "tag:yaml.org,2002:str")
                  (string= (resolve-plain-scalar-tag value :core)
                           "tag:yaml.org,2002:str"))
             (and tag
                  (not (string= tag "tag:yaml.org,2002:str"))
                  (member tag +implicit-scalar-tags+ :test #'string=))
             (or (null tag)
                 (string= (resolve-plain-scalar-tag value :core)
                          "tag:yaml.org,2002:str"))))))

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
