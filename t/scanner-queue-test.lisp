(in-package #:cl-yaml-kit/test)

(defparameter *invalid-scanner-state-slot-value* -1)

(progn
  (defun scanner-with-recycled-token (text)
    (let* ((scanner (yaml-kit::make-scanner (scanner-source text)))
           (seeded-span (test-point-span 9 8 7))
           (recycled (yaml-kit:make-token :tag seeded-span seeded-span
                                          :handle "!e!" :suffix "old")))
      (setf (yaml-kit::scanner-token-pool scanner) (list recycled))
      (values scanner recycled)))

  (defun scanner-recycled-stream-start ()
    (multiple-value-bind (scanner recycled)
        (scanner-with-recycled-token "payload")
      (values scanner recycled (yaml-kit::%scanner-next-token scanner))))

  (defun scanner-recycled-scalar ()
    (multiple-value-bind (scanner recycled)
        (scanner-with-recycled-token "payload")
      (yaml-kit::%scanner-next-token scanner)
      (yaml-kit::recycle-scanner-token scanner)
      (values scanner recycled (yaml-kit::%scanner-next-token scanner))))

  (defun scanner-token-positions (token)
    (list (list (cl-parser-kit:span-start-line (yaml-kit:token-start-span token))
                (cl-parser-kit:span-start-column (yaml-kit:token-start-span token))
                (cl-parser-kit:span-start (yaml-kit:token-start-span token)))
          (list (cl-parser-kit:span-start-line (yaml-kit:token-end-span token))
                (cl-parser-kit:span-start-column (yaml-kit:token-end-span token))
                (cl-parser-kit:span-start (yaml-kit:token-end-span token)))))

  (it "uses the recycled object for the stream-start token"
    (multiple-value-bind (scanner recycled token)
        (scanner-recycled-stream-start)
      (declare (ignore scanner))
      (expect (eq token recycled) :to-be-truthy)))

  (it "sets the recycled token kind to stream-start"
    (multiple-value-bind (scanner recycled token)
        (scanner-recycled-stream-start)
      (declare (ignore scanner recycled))
      (expect (yaml-kit:token-kind token) :to-be :stream-start)))

  (it "clears recycled token fields for stream-start"
    (multiple-value-bind (scanner recycled token)
        (scanner-recycled-stream-start)
      (declare (ignore scanner recycled))
      (expect (list (yaml-kit:token-value token)
                    (yaml-kit:token-handle token)
                    (yaml-kit:token-suffix token)
                    (yaml-kit:token-style token)
                    (yaml-kit:token-major token)
                    (yaml-kit:token-minor token))
              :to-equal (list nil nil nil nil nil nil))))

  (it "resets recycled stream-start token spans"
    (multiple-value-bind (scanner recycled token)
        (scanner-recycled-stream-start)
      (declare (ignore scanner recycled))
      (expect (scanner-token-positions token)
              :to-equal (list (list 1 1 0) (list 1 1 0)))))

  (it "returns a recycled token to the scanner pool"
    (multiple-value-bind (scanner recycled token)
        (scanner-recycled-stream-start)
      (declare (ignore recycled))
      (yaml-kit::recycle-scanner-token scanner)
      (expect (eq (first (yaml-kit::scanner-token-pool scanner)) token)
              :to-be-truthy)))

  (it "reuses a recycled stream-start token for the next scalar"
    (multiple-value-bind (scanner recycled token)
        (scanner-recycled-scalar)
      (declare (ignore scanner))
      (expect (eq token recycled) :to-be-truthy)))

  (it "sets the reused scalar token kind and value"
    (multiple-value-bind (scanner recycled token)
        (scanner-recycled-scalar)
      (declare (ignore scanner recycled))
      (expect (list (yaml-kit:token-kind token) (yaml-kit:token-value token))
              :to-equal (list :scalar "payload"))))

  (it "clears tag metadata on a reused scalar token"
    (multiple-value-bind (scanner recycled token)
        (scanner-recycled-scalar)
      (declare (ignore scanner recycled))
      (expect (list (yaml-kit:token-handle token)
                    (yaml-kit:token-suffix token)
                    (yaml-kit:token-major token)
                    (yaml-kit:token-minor token))
              :to-equal (list nil nil nil nil))))

  (it "sets plain style on a reused scalar token"
    (multiple-value-bind (scanner recycled token)
        (scanner-recycled-scalar)
      (declare (ignore scanner recycled))
      (expect (yaml-kit:token-style token) :to-be :plain)))

  (it "sets spans on a reused scalar token"
    (multiple-value-bind (scanner recycled token)
        (scanner-recycled-scalar)
      (declare (ignore scanner recycled))
      (expect (scanner-token-positions token)
              :to-equal (list (list 1 1 0) (list 1 8 7)))))

  (it "records the reused scalar as the previous scanner token kind"
    (multiple-value-bind (scanner recycled token)
        (scanner-recycled-scalar)
      (declare (ignore recycled token))
      (expect (yaml-kit::scanner-previous-token-kind scanner) :to-be :scalar))))

(it "rejects invalid scanner state slot values"
  (let ((scanner (yaml-kit::make-scanner (scanner-source ""))))
    (expect
      (lambda ()
        (setf (yaml-kit::scanner-recyclable-token scanner)
              *invalid-scanner-state-slot-value*))
      :to-throw
      'type-error)
    (expect
      (lambda ()
        (setf (yaml-kit::scanner-previous-token-kind scanner)
              *invalid-scanner-state-slot-value*))
      :to-throw
      'type-error)))

(it "compacts a long token queue"
  (multiple-value-bind (expected-values kinds scalars compacted-p)
      (scanner-queue-compaction-observation)
    (declare (ignore expected-values scalars))
    (expect (> (length kinds) 64) :to-be-truthy)
    (expect compacted-p :to-be-truthy)))

(it "preserves token kinds across queue compaction"
  (multiple-value-bind (expected-values kinds scalars compacted-p)
      (scanner-queue-compaction-observation)
    (declare (ignore scalars compacted-p))
    (expect
     kinds
     :to-equal
     (append '(:stream-start :flow-sequence-start)
             (loop for index below (length expected-values)
                   append (if (< index (1- (length expected-values)))
                              '(:scalar :flow-entry)
                              '(:scalar)))
             '(:flow-sequence-end :stream-end)))))

(it "preserves scalar values across queue compaction"
  (multiple-value-bind (expected-values kinds scalars compacted-p)
      (scanner-queue-compaction-observation)
    (declare (ignore kinds compacted-p))
    (expect scalars :to-equal expected-values)))

(defun scanner-queue-compaction-observation ()
  (let* ((expected-values
           (loop for index below 40 collect (format nil "item-~D" index)))
         (scanner
           (yaml-kit::make-scanner
            (scanner-source (format nil "[~{~A~^,~}]" expected-values))))
         (kinds nil)
         (scalars nil)
         (previous-head 0)
         (compacted-p nil))
    (loop for token = (yaml-kit::%scanner-next-token scanner)
          while token
          do (let ((head (yaml-kit::scanner-tokens-head scanner)))
               (when (< head previous-head)
                 (setf compacted-p t))
               (setf previous-head head))
             (push (yaml-kit:token-kind token) kinds)
             (when (eq (yaml-kit:token-kind token) :scalar)
               (push (yaml-kit:token-value token) scalars)))
    (values expected-values (nreverse kinds) (nreverse scalars) compacted-p)))
