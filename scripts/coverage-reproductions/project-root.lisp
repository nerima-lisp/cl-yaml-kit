(defun coverage-reproduction-project-root (script-pathname)
  "SCRIPT-PATHNAME must be directly in scripts/coverage-reproductions/."
  (uiop:pathname-parent-directory-pathname
   (uiop:pathname-parent-directory-pathname
    (uiop:pathname-directory-pathname script-pathname))))
