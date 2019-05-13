(in-package :yadfa-items)
(defclass gold-bar (item) ()
    (:default-initargs
        :name "Gold Bar"
        :description "A Gold Bar"
        :value 50000))
(defclass gem (item) ()
    (:default-initargs
        :name "Gem"
        :description "A Valuable Gem"
        :value 25000))
(defclass gold-collar (headpiece) ()
    (:default-initargs
        :name "Gold Collar"
        :description "A very expensive collar with a gold tag and studded with gems"
        :value 25000))
(defclass collar (headpiece) ()
    (:default-initargs
        :name "Collar"
        :description "A collar that your pet furries can wear. Has an id tag on it for easy identification."))
(defclass magic-diaper-key (item) ()
    (:default-initargs
        :name "Magic Diaper Key"
        :tossable nil
        :sellable nil
        :description "This mysterious artifact seems to have the ability to prevent others from removing their diapers"))
(defclass pocket-map-machine (item) ()
    (:default-initargs
        :name "Pocket Map Machine"
        :tossable nil
        :sellable nil
        :description "So you're playing Pokemon and you're making your secret base. Then you're like `Damn, I wish I coult take this awesome base with me' or `I wish I could create my own decorations for this base instead of only being able to use what Nintendo provides me'. While Pouar can't do anything about Pokemon, he can create a similar feature for this game without these limitations. So here it is, the Pocket Map Machine"
        :use-script '(lambda (item user)
                         (declare (ignore item user))
                         (move-to-pocket-map item))))
(defclass warp-device (item) ()
    (:default-initargs
        :name "Warp Device"
        :tossable nil
        :sellable nil
        :description "This device can be used to summon a warp pipe to take you to the secret underground"
        :use-script '(lambda (item user)
                         (declare (ignore item user))
                         (move-to-secret-underground))))
(defclass macguffin (item) ()
    (:default-initargs
        :name "MacGuffin"
        :sellable nil
        :tossable nil
        :description "Collect as many of these fuckers as you possibly can. Don't ask why, just do it."))
(defclass itemfinder (item) ()
    (:default-initargs
        :name "Itemfinder"
        :description "Returns T anytime a hidden item is nearby. It is based on Pouar's ability to detect whatever he has to say is offensive or not. It uses the same algorithm, is about as effective, and has about as many happy customers. Also, if you wrap the function in a not function, it becomes the same algorithm SJWs use to decide whatever they hear is offensive or not."
        :use-script '(lambda (item user)
                         (declare (ignore item user))
                         nil)))
(defclass shine-star (item) ()
    (:default-initargs
        :name "Shine Star"
        :sellable nil
        :tossable nil
        :description "Collect as many of these fuckers as you possibly can. Don't ask why, just do it."))
