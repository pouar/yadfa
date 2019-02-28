(in-package :yadfa)
(defvar *battle* nil)
(defvar *mod-registry* nil)
(defvar *tile-cache* (make-hash-table :test 'equal))
