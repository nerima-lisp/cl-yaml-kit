;;;; src/emitter-scalars.lisp
(in-package #:yaml-kit)

(defparameter +yaml-indicator-characters+ "-?:,[]{}#&*!|>'\"%@`")
(defparameter +implicit-scalar-tags+
  '("tag:yaml.org,2002:int" "tag:yaml.org,2002:float"
    "tag:yaml.org,2002:bool" "tag:yaml.org,2002:null"
    "tag:yaml.org,2002:str"))

(declaim (inline %yaml-line-break-p %yaml-printable-character-p
                %yaml-blank-p))

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

(defun %scalar-analysis (value)
  "Return the libyaml-style scalar restrictions for VALUE."
  (declare (optimize (speed 3) (safety 1)) (type simple-string value))
  (let ((length (length value))
        (block-indicators nil) (flow-indicators nil)
        (line-breaks nil) (special-characters nil)
        (leading-space nil) (leading-break nil)
        (trailing-space nil) (trailing-break nil)
        (break-space nil) (space-break nil)
        (previous-space nil) (previous-break nil))
    (when (zerop length)
      (return-from %scalar-analysis
        (list :multiline nil :flow-plain nil :block-plain t
              :single-quoted t :block nil)))
    (when (or (and (>= length 3) (string= value "---" :end1 3))
              (and (>= length 3) (string= value "..." :end1 3)))
      (setf block-indicators t flow-indicators t))
    (loop for index from 0 below length
          for character = (char value index)
          for preceded-by-whitespace = (or (zerop index)
                                           (%yaml-blank-p (char value (1- index)))
                                           (%yaml-line-break-p (char value (1- index))))
          for followed-by-whitespace = (or (= index (1- length))
                                           (%yaml-blank-p (char value (1+ index)))
                                           (%yaml-line-break-p (char value (1+ index))))
          do
             (if (zerop index)
                 (cond
                   ((find character +yaml-indicator-characters+)
                    (setf flow-indicators t block-indicators t))
                   ((find character "-?:")
                    (setf flow-indicators t block-indicators t))
                   ((and (char= character #\-) followed-by-whitespace)
                    (setf flow-indicators t block-indicators t)))
                 (progn
                   (when (find character ",?[]{}") (setf flow-indicators t))
                   (when (char= character #\:)
                     (setf flow-indicators t)
                     (when followed-by-whitespace (setf block-indicators t)))
                   (when (and (char= character #\#) preceded-by-whitespace)
                     (setf flow-indicators t block-indicators t))))
             (unless (%yaml-printable-character-p character)
               (setf special-characters t))
             (when (%yaml-line-break-p character) (setf line-breaks t))
             (cond
               ((%yaml-blank-p character)
                (when (zerop index) (setf leading-space t))
                (when (= index (1- length)) (setf trailing-space t))
                (when previous-break (setf break-space t))
                (setf previous-space t previous-break nil))
               ((%yaml-line-break-p character)
                (when (zerop index) (setf leading-break t))
                (when (= index (1- length)) (setf trailing-break t))
                (when previous-space (setf space-break t))
                (setf previous-space nil previous-break t))
               (t (setf previous-space nil previous-break nil))))
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
            :block block))))

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

(defun %write-hex-escape (context prefix code digits)
  (%emit-text context prefix)
  (loop for shift downfrom (* 4 (1- digits)) to 0 by 4
        do (%emit-char context (%hex-digit (logand #xf (ash code (- shift)))))))

(defun %write-double-quoted (value context)
  (%emit-char context #\")
  (loop for character across value
        for code fixnum = (char-code character)
        do (case character
             (#\Null (%emit-text context "\\0")) (#\Bell (%emit-text context "\\a"))
             (#\Backspace (%emit-text context "\\b")) (#\Tab (%emit-text context "\\t"))
             (#\Newline (%emit-text context "\\n")) (#\Vt (%emit-text context "\\v"))
             (#\Page (%emit-text context "\\f")) (#\Return (%emit-text context "\\r"))
             (#\Escape (%emit-text context "\\e")) (#\" (%emit-text context "\\\""))
             (#\\ (%emit-text context "\\\\"))
             (t (cond ((= code #x85) (%emit-text context "\\N"))
                      ((= code #xa0) (%emit-text context "\\_"))
                      ((= code #x2028) (%emit-text context "\\L"))
                      ((= code #x2029) (%emit-text context "\\P"))
                      ((or (< code #x20) (= code #x7f) (= code #xfeff))
                       (%write-hex-escape context (if (= code #xfeff) "\\u" "\\x")
                                           code (if (= code #xfeff) 4 2)))
                      (t (%emit-char context character))))))
  (%emit-char context #\"))

(defun %write-plain (value context)
  (declare (optimize (speed 3) (safety 1)) (type simple-string value))
  (loop for start = 0 then (1+ end)
        for end = (position #\Newline value :start start)
        do (if end
               (progn
                 (when (> end start)
                   (%emit-text context (subseq value start end)))
                 (%emit-char context #\Newline)
                 (%emit-char context #\Newline))
               (progn
                 (when (< start (length value))
                   (%emit-text context (subseq value start)))
                 (return)))))

(defun %write-single-quoted (value context &optional (indent 0))
  (%emit-char context #\')
  (let ((breaks nil))
    (loop for character across value do
      (if (char= character #\Newline)
          (progn
            (unless breaks (%emit-char context #\Newline))
            (%emit-char context #\Newline)
            (setf breaks t))
          (progn
            (when breaks
              (%emit-text context (make-string indent :initial-element #\Space))
              (setf breaks nil))
            (%emit-char context character)
            (when (char= character #\') (%emit-char context #\')))))
    (when breaks
      (%emit-text context (make-string indent :initial-element #\Space))))
  (%emit-char context #\'))

(defun %write-block-scalar (value context folded indent &optional (preserve-blank-indentation t))
  (let ((chomp (cond ((and (plusp (length value))
                           (%yaml-line-break-p (char value (1- (length value)))))
                      (if (and (> (length value) 1)
                               (%yaml-line-break-p (char value (- (length value) 2)))) "+" ""))
                     (t "-"))))
    (%emit-char context (if folded #\> #\|))
    (let ((first-content
            (loop for start = 0 then (1+ end)
                  for end = (position #\Newline value :start start)
                  for line = (subseq value start (or end (length value)))
                  unless (zerop (length line))
                    do (return line)
                  when (null end) do (return nil))))
      (when (and first-content
                 (or (%yaml-blank-p (char first-content 0))
                     (char= (char first-content 0) #\#)))
        (%emit-char context #\2)))
    (%emit-text context chomp)
    (%emit-char context #\Newline)
    (loop for start = 0 then (1+ end)
          for end = (position #\Newline value :start start)
          do (when (>= start (length value)) (return))
             (unless (or (= start (length value))
                         (and (not preserve-blank-indentation)
                              end (= start end))
                         (and end (= start end)
                              (let* ((next-start (1+ end))
                                     (next-end (and (< next-start (length value))
                                                   (position #\Newline value :start next-start))))
                                (or (null next-end)
                                    (= next-start next-end)
                                    (not (loop for index from next-start below next-end
                                               always (%yaml-blank-p (char value index))))))))
               (%emit-text context (make-string indent :initial-element #\Space)))
             (when end
               (%emit-text context (subseq value start end))
               (%emit-char context #\Newline))
             (unless end
               (when (< start (length value))
                 (%emit-text context (subseq value start)))
               (when (or (zerop (length value))
                         (not (%yaml-line-break-p (char value (1- (length value))))))
                 (%emit-char context #\Newline))
               (return)))))

(defun %write-scalar (value style context &optional (indent 0)
                                             (preserve-blank-indentation t))
  (case style
    (:plain (%write-plain value context))
    (:single-quoted (%write-single-quoted value context indent))
    (:double-quoted (%write-double-quoted value context))
    (:literal (%write-block-scalar value context nil indent preserve-blank-indentation))
    (:folded (%write-block-scalar value context t indent preserve-blank-indentation))))
