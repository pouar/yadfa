(in-package :yadfa-npcs)
(defclass egg-pawn (npc) ()
    (:default-initargs
        :name "Egg Pawn"
        :description "One of Eggman's robots"
        :species "Egg Pawn"
        :male t
        :bitcoins-per-level 40))
