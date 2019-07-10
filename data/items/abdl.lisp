(in-package :yadfa-items)
(defclass pacifier (headpiece) ()
  (:default-initargs
   :name "Pacifier"
   :description "A pacifier you can suck on."
   :value 250))
(defclass gold-pacifier (headpiece) ()
  (:default-initargs
   :name "Gold-Pacifier"
   :description "A pacifier you can suck on. Has a golden mouth shield and a ruby for a handle"
   :value 10000))
(defclass recovering-pacifier (headpiece) ()
  (:default-initargs
   :name "Recovering Pacifier"
   :description "A pacifier you can suck on. Recovers your health and energy when you suck on it"
   :value 5000
   :wear-script '(lambda (item user)
                  (declare (ignorable item))
                  (incf (health-of user) (/ (calculate-stat user :health) 16))
                  (incf (health-of user) (/ (calculate-stat user :energy) 16))
                  (when (> (health-of user) (calculate-stat user :health))
                    (setf (health-of user) (calculate-stat user :health)))
                  (when (> (energy-of user) (calculate-stat user :energy))
                    (setf (energy-of user) (calculate-stat user :energy))))))
(defclass healing-pacifier (headpiece) ()
  (:default-initargs
   :name "Healing Pacifier"
   :description "A pacifier you can suck on. Recovers your health when you suck on it"
   :value 2500
   :wear-script '(lambda (item user)
                  (declare (ignorable item))
                  (incf (health-of user) (/ (calculate-stat user :health) 16))
                  (when (> (health-of user) (calculate-stat user :health))
                    (setf (health-of user) (calculate-stat user :health))))))
(defclass energizing-pacifier (headpiece) ()
  (:default-initargs
   :name "Energizing Pacifier"
   :description "A pacifier you can suck on. Recovers your energy when you suck on it"
   :value 2500
   :wear-script '(lambda (item user)
                  (declare (ignorable item))
                  (incf (health-of user) (/ (calculate-stat user :energy) 16))
                  (when (> (energy-of user) (calculate-stat user :energy))
                    (setf (energy-of user) (calculate-stat user :energy))))))
(defclass blanket (item) ()
  (:default-initargs
   :name "Blanket"
   :description "A Blanket you can carry around like a toddler"
   :value 200))
(defclass plushie (item) ()
  (:default-initargs
   :name "Plushie"
   :description "A Plushie you can carry around like a toddler"
   :value 200))
