(in-package :yadfa)
(defvar *battle* nil)
(defvar *events-in-game* (make-hash-table))
(defvar *zones-to-initialize* '())
(defvar *game* nil)
(defvar *inithooks/zone* (make-hash-table :test 'equal))
(defvar *mod-registry* nil)
