(in-package #:cl-yaml-kit/test)

(defparameter *invalid-scanner-state-slot-value* -1)

(it "reuses a recycled token for the next scanner token"
  (let* ((scanner (yaml-kit::make-scanner (scanner-source "payload")))
         (seeded-span (test-point-span 9 8 7))
         (recycled (yaml-kit:make-token :tag seeded-span seeded-span
                                        :handle "!e!" :suffix "old")))
    (setf (yaml-kit::scanner-token-pool scanner) (list recycled))
    (let ((stream-start (yaml-kit::%scanner-next-token scanner)))
      (expect (list (eq stream-start recycled)
                    (yaml-kit:token-kind stream-start)
                    (yaml-kit:token-value stream-start)
                    (yaml-kit:token-handle stream-start)
                    (yaml-kit:token-suffix stream-start)
                    (yaml-kit:token-style stream-start)
                    (yaml-kit:token-major stream-start)
                    (yaml-kit:token-minor stream-start)
                    (list (cl-parser-kit:span-start-line (yaml-kit:token-start-span stream-start))
                          (cl-parser-kit:span-start-column (yaml-kit:token-start-span stream-start))
                          (cl-parser-kit:span-start (yaml-kit:token-start-span stream-start)))
                    (list (cl-parser-kit:span-start-line (yaml-kit:token-end-span stream-start))
                          (cl-parser-kit:span-start-column (yaml-kit:token-end-span stream-start))
                          (cl-parser-kit:span-start (yaml-kit:token-end-span stream-start))))
              :to-equal '(t :stream-start nil nil nil nil nil nil
                                  (1 1 0) (1 1 0)))
      (yaml-kit::recycle-scanner-token scanner)
      (expect (eq (first (yaml-kit::scanner-token-pool scanner)) stream-start)
              :to-be-truthy)
      (let ((scalar (yaml-kit::%scanner-next-token scanner)))
        (expect (list (eq scalar recycled)
                      (yaml-kit:token-kind scalar)
                      (yaml-kit:token-value scalar)
                      (yaml-kit:token-handle scalar)
                      (yaml-kit:token-suffix scalar)
                      (yaml-kit:token-style scalar)
                      (yaml-kit:token-major scalar)
                      (yaml-kit:token-minor scalar)
                      (list (cl-parser-kit:span-start-line (yaml-kit:token-start-span scalar))
                            (cl-parser-kit:span-start-column (yaml-kit:token-start-span scalar))
                            (cl-parser-kit:span-start (yaml-kit:token-start-span scalar)))
                      (list (cl-parser-kit:span-start-line (yaml-kit:token-end-span scalar))
                            (cl-parser-kit:span-start-column (yaml-kit:token-end-span scalar))
                            (cl-parser-kit:span-start (yaml-kit:token-end-span scalar)))
                      (yaml-kit::scanner-previous-token-kind scanner))
                :to-equal '(t :scalar "payload" nil nil :plain nil nil
                                  (1 1 0) (1 8 7) :scalar))))))

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
