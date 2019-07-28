(in-package :yadfa-status-conditions)
(defclass skunked (status-condition)
  ()
  (:default-initargs
   :name "Skunked"
   :description "User's defenses are reduced due to the smell"
   :duration 2
   :stat-delta '(:defense -5)))
