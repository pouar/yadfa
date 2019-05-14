(in-package :yadfa-npcs)
(defclass egg-pawn (npc) ()
    (:default-initargs
        :name "Egg Pawn"
        :description "One of Eggman's robots"
        :species "Egg Pawn"
        :male t
        :attributes (list :not-ticklish t)
        :bitcoins-per-level 40))
