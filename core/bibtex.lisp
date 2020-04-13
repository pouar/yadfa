;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-bibtex"; coding: utf-8-unix; -*-
(uiop:define-package :yadfa-bibtex (:use :iterate :cl))
(in-package :yadfa-bibtex)
(defun generate-bibtex ()
  (uiop:with-input-file (in "yadfa:home;docs;guide;pouar.bib")
    (uiop:with-output-file (out "yadfa:home;docs;guide;references.texi" :if-exists :supersede :if-does-not-exist :create)
      (let ((bibtex-runtime:*bib-database* (make-hash-table :test 'equalp))
            (*print-miser-width* 20))
        (declare (special bibtex-runtime:*bib-database*))
        (macrolet ((generate-entries ()
                     `(list
                       ,@(iter (for i in '(("author")
                                           ("title" . (format nil "@cite{~a}" this))
                                           ("date" . (multiple-value-bind (second minute hour date month year day daylight-p zone)
                                                         (decode-universal-time (date-time-parser:parse-date-time this))
                                                       (declare (ignorable second minute hour date month year day daylight-p zone))
                                                       (format nil "@abbr{~a.} ~a@comma{} ~a"
                                                               (aref local-time:+short-month-names+ month) day year)))
                                           ("url" . (format nil "URL:@url{~a}" this))))
                           (collect `(let ((this (bibtex-runtime:bib-entry-ref ,(car i) v)))
                                       (when this
                                         (if ',(cdr i) ,(cdr i) this))))))))
          (bibtex-runtime:read-bib-database in)
          (iter (for (k v) in-hashtable bibtex-runtime:*bib-database*)
            (format out "@ifset ~aisref~%@item ~a @anchor{~a}~%~%~{~a.~^ ~}~%@end ifset~%" k k k
                    (iter (for i in (generate-entries))
                      (when i (collect i))))))))))
