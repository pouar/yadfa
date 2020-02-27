;;;; -*- mode: Common-Lisp; sly-buffer-package: "common-lisp-user"; coding: utf-8-unix; -*-
(defpackage #:yadfa-tests
  (:use #:cl #:fiveam #:iterate)
  (:export #:run-tests
           #:all-tests
           #:initialization-tests
           #:initialize-items
           #:initialize-enemies
           #:initialize-moves
           #:initialize-props
           #:initialize-status-conditions
           #:initialize-allies))
