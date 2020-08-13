;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-props"; coding: utf-8-unix; -*-
(in-package :yadfa-props)
(defclass training-potty (placable-toilet) ()
  (:default-initargs :name "Training Potty"
                     :description "A training potty"))
