;; Top-level form heads that SB-COVER reports unexecuted as a whole although the
;; form ran at compile or load time. scripts/coverage-gate.lisp excuses such a
;; form only when the whole form is reported unexecuted, prints each excused
;; form, and fails on a rule that excused nothing.
(:head "in-package" :reason "The package switch takes effect at compile time.")
(:head "defpackage" :reason "Package definitions are compile/load-time metadata.")
(:head "declaim" :reason "Global declarations take effect at compile time.")
(:head "defconstant" :reason "The constant value is computed while compiling.")
(:head "defparameter" :reason "The initial value is computed while loading the file.")
(:head "defvar" :reason "The initial value is computed while loading the file.")
(:head "defstruct" :reason "Structure definitions are compile/load-time metadata.")
(:head "deftype" :reason "Type definitions are compile-time declarations.")
(:head "define-tag-table" :reason "The table expands to a DEFPARAMETER of static data.")
