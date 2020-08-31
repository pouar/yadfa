;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-tests"; coding: utf-8-unix; -*-
(in-package :yadfa-tests)
(def-suite all-tests)
(def-suite initialization-tests :in all-tests)
(in-suite initialization-tests)
(defmacro test-initialize-package (package)
  `(iter (for class in-package ,package external-only t)
         (when (find-class class nil)
           (let ((result (handler-case (class-of (make-instance class))
                           (error (c) c))))
             (is (eq result (find-class class nil))
                 "Class ~s failed to initialize:~%~8t~a" class result)))))
(test initialize-items
      (test-initialize-package :yadfa-items))
(test initialize-enemies
      (test-initialize-package :yadfa-enemies))
(test initialize-moves
      (test-initialize-package :yadfa-moves))
(test initialize-props
      (test-initialize-package :yadfa-props))
(test initialize-status-conditions
      (test-initialize-package :yadfa-status-conditions))
(test initialize-allies
      (test-initialize-package :yadfa-allies))
