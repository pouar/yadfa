;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa"; coding: utf-8-unix; -*-
(in-package :yadfa)
(defstruct action
  "An action for a prop or item"
  (documentation nil :type (or null simple-string))
  (lambda '(lambda (prop))
    :type (or list symbol function)))
(defmethod documentation ((x action) (doc-type (eql t)))
  (action-documentation x))
(defmethod describe-object ((object action) stream)
  (call-next-method)
  ;;; copied from SBCL
  (let ((doc (handler-bind ((warning #'muffle-warning))
               (documentation object t))))
    (when doc
      (format stream "~%~@:_Documentation:~@:_")
      (pprint-logical-block (stream nil :per-line-prefix "  ")
        (princ doc stream)))))
