(in-package :yadfa)
(defvar *battle* nil)
(defvar *mod-registry* nil)
(defvar *pattern-cache* (make-hash-table :test 'equal))
