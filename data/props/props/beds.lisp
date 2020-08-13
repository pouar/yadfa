;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-props"; coding: utf-8-unix; -*-
(in-package :yadfa-props)
(defclass pet-bed (placable-bed) ()
  (:default-initargs :name "Pet Bed"
                     :description "A portable pet bed big enough for you to sleep in"))
(defmethod use-script ((item pet-bed) (user base-character) (target team-member))
  (go-to-sleep))
(defmethod cant-use-p ((item pet-bed) (user base-character) (target base-character) action &key &allow-other-keys)
  (when *battle*
    (values t '(:format-control "that item can't be used in battle"))))
(defclass crib (placable-bed) ()
  (:default-initargs :name "Crib"
                     :description "A crib big enough for you to sleep in"))
