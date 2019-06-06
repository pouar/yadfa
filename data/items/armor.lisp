(in-package :yadfa-items)
(defclass knights-armor (full-outfit) ()
    (:default-initargs
        :name "Knight's Armor"
        :plural-name "Knight's Armor"
        :value 1000
        :description "Steel armor that protects you in battle. Pants not included, you don't need em, especially when you have diapers"
        :wear-stats '(:defense 50)
        :thickness-capacity nil))
