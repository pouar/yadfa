(in-package :yadfa/items)
(defclass generic-diapers (yadfa:diaper) ()
    (:default-initargs
        :sogginess-capacity 100
        :messiness-capacity 200
        :name "Store Brand Diapers"
        :plural-name "Store Brand Diapers"
        :wear-wet-text (list
                           "Not leaking yet"
                           "This thing might not last much longer"
                           "Damn thing is leaking already")
        :wear-mess-text (list
                            "You can feel a slight mess back there"
                            "You can feel a slight mess back there"
                            "This was never designed to hold a mess, and the mess leaking down the leg guards show it")
        :description "These are our new Super Absorbent Briefs!!! Built to the top American standards of adult diapers, meaning they're neither super, nor absorbent."))
(defclass generic-diapers-package (item) ()
    (:default-initargs
        :name "Package of Store Brand Diapers"
        :plural-name "Packages of Store Brand Diapers"
        :description "These are our new Super Absorbent Briefs!!! Built to the top American standards of adult diapers, meaning they're neither super, nor absorbent."
        :consumable t
        :value 200
        :use-script '(lambda (item user)
                         (declare (ignore item))
                         (format t "You tear open the package and dump all the diapers out of it.~%")
                         (loop for i from 1 to 20 do (push (make-instance 'yadfa/items:generic-diapers) (inventory-of user))))))
(defclass generic-pullons (pullon) ()
    (:default-initargs
        :sogginess-capacity 100
        :messiness-capacity 200
        :name "Store Brand Pullons"
        :plural-name "Store Brand Pullons"
        :wear-wet-text (list
                           "Not leaking yet"
                           "This thing might not last much longer"
                           "Damn thing is leaking already")
        :wear-mess-text (list
                            "You can feel a slight mess back there"
                            "You can feel a slight mess back there"
                            "This was never designed to hold a mess, and the mess leaking down the leg guards show it")
        :description "Our new Super Absorbant Briefs in Pullon form!!! They look just like real underwear!!! And is about as absorbent as real underwear too!!!!"))
(defclass generic-pullons-package (item) ()
    (:default-initargs
        :name "Package of Store Brand Pullons"
        :plural-name "Packages of Store Brand Pullons"
        :description "Our new Super Absorbant Briefs in Pullon form!!! They look just like real underwear!!! And is about as absorbent as real underwear too!!!!"
        :consumable t
        :value 200
        :use-script '(lambda (item user)
                         (declare (ignore item))
                         (format t "You tear open the package and dump all the diapers out of it.~%")
                         (loop for i from 1 to 20 do (push (make-instance 'yadfa/items:generic-pullons) (inventory-of user))))))
(defclass incontinence-pad (yadfa:incontinence-pad) ()
    (:default-initargs
        :sogginess-capacity 1000
        :messiness-capacity 1000
        :name "Incontinence Pad"
        :disposable t
        :wear-wet-text (list
                           "You can barely tell you wet it"
                           "It's swollen with urine"
                           "It's leaking")
        :wear-mess-text (list
                            "You can feel a slight mess back there"
                            "The mess is about to fall out"
                            "The mess is falling out of the stuffer")
        :description "For when you think you're too old for real padding, or if you just want your existing padding to last longer"))
(defclass incontinence-pad-package (item) ()
    (:default-initargs
        :name "Package of Incontinence Pads"
        :plural-name "Packages of Incontinence Pads"
        :description "For when you think you're too old for real padding, or if you just want your existing padding to last longer"
        :consumable t
        :value 200
        :use-script '(lambda (item user)
                         (declare (ignore item))
                         (format t "You tear open the package and dump all the diapers out of it.~%")
                         (loop for i from 1 to 20 do (push (make-instance 'yadfa/items:incontinence-pad) (inventory-of user))))))
(defclass cloth-incontinence-pad (yadfa:incontinence-pad) ()
    (:default-initargs
        :sogginess-capacity 1000
        :messiness-capacity 1000
        :name "Cloth Incontinence Pad"
        :wear-wet-text (list
                           "You can barely tell you wet it"
                           "It's swollen with urine"
                           "It's leaking")
        :wear-mess-text (list
                            "You can feel a slight mess back there"
                            "The mess is about to fall out"
                            "The mess is falling out of the stuffer")
        :description "For when you think you're too old for real padding, or if you just want your existing padding to last longer. Can be reused"))
(defclass diaper (yadfa:diaper) ()
    (:default-initargs
        :sogginess-capacity 1400
        :messiness-capacity 10000
        :name "Diaper"
        :disposable t
        :description "Your average diaper (Not by American standards)"
        :wear-wet-text (list
                           "You can barely tell you wet it"
                           "It squishes when you walk"
                           "little yellow streams are leaking down from the leg guards")
        :wear-mess-text (list
                            "You can feel a slight mess back there"
                            "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                            "Poo is leaking out of the leg guards")))
(defclass cloth-diaper (yadfa:diaper) ()
    (:default-initargs
        :sogginess-capacity 1400
        :messiness-capacity 10000
        :name "Cloth Diaper"
        :description "A poofy diaper that can hold an accident. Can be reused."
        :wear-wet-text (list
                           "You can barely tell you wet it"
                           "It squishes when you walk"
                           "little yellow streams are leaking down from the leg guards")
        :wear-mess-text (list
                            "You can feel a slight mess back there"
                            "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                            "Poo is leaking out of the leg guards")))
(defclass diaper-package (item) ()
    (:default-initargs
        :name "Package of Diapers"
        :plural-name "Packages of Diapers"
        :description "A package of poofy diapers that can hold an accident."
        :consumable t
        :value 250
        :use-script '(lambda (item user)
                         (declare (ignore item))
                         (format t "You tear open the package and dump all the diapers out of it.~%")
                         (loop for i from 1 to 20 do (push (make-instance 'yadfa/items:diaper) (inventory-of user))))))
(defclass kurikia-thick-diaper (yadfa:diaper) ()
    (:default-initargs
        :sogginess-capacity 10000
        :messiness-capacity 100000
        :name "Kurikia Thick Diaper"
        :value 60
        :thickness 300
        :thickness-capacity 100
        :disposable t
        :wear-wet-text (list
                           "The pee seems to have disappeared into the diaper"
                           "It sloshes when you walk"
                           "Pee flows down with every step")
        :wear-mess-text (list
                            "The diaper holds the mess easily"
                            "It smooshes when you walk"
                            "Poo is leaking out of the diaper")
        :description "It's like carrying a giant pillow between your legs forcing you to waddle"))
(defclass kurikia-thick-cloth-diaper (yadfa:diaper) ()
    (:default-initargs
        :sogginess-capacity 10000
        :messiness-capacity 100000
        :name "Kurikia Thick Cloth Diaper"
        :value 400
        :thickness 300
        :thickness-capacity 100
        :wear-wet-text (list
                           "The pee seems to have disappeared into the diaper"
                           "It sloshes when you walk"
                           "Pee flows down with every step")
        :wear-mess-text (list
                            "The diaper holds the mess easily"
                            "It smooshes when you walk"
                            "Poo is leaking out of the diaper")
        :description "It's like carrying a giant pillow between your legs forcing you to waddle. Can be reused."))
(defclass thick-latex-diaper (yadfa:diaper) ()
    (:default-initargs
        :sogginess-capacity 10000
        :messiness-capacity 100000
        :waterproof t
        :name "Thick Latex Diaper"
        :value 400
        :thickness 300
        :thickness-capacity 100
        :wear-wet-text (list
                           "The pee seems to have disappeared into the diaper"
                           "It sloshes when you walk"
                           "Pee flows down with every step")
        :wear-mess-text (list
                            "The diaper holds the mess easily"
                            "It smooshes when you walk"
                            "Poo is leaking out of the diaper")
        :description "A giant thick plastic diaper. Yes, the waddle is noticable"))
(defclass hyper-thick-diaper (yadfa:diaper) ()
    (:default-initargs
        :sogginess-capacity 50000
        :messiness-capacity 500000
        :name "Hyper Thick Diaper"
        :value 700
        :thickness 1000
        :thickness-capacity 380
        :disposable t
        :wear-wet-text (list
                           "The pee seems to have disappeared into the diaper"
                           "It sloshes when you walk"
                           "Pee flows down with every step")
        :wear-mess-text (list
                            "The diaper holds the mess easily"
                            "It smooshes when you walk"
                            "Poo is leaking out of the diaper")
        :description "When you leak so often that you need a diaper so thick it nearly touches the ground"))
(defclass hyper-thick-cloth-diaper (yadfa:diaper) ()
    (:default-initargs
        :sogginess-capacity 50000
        :messiness-capacity 500000
        :name "Hyper Thick Cloth Diaper"
        :value 1000
        :thickness 1000
        :thickness-capacity 380
        :wear-wet-text (list
                           "The pee seems to have disappeared into the diaper"
                           "It sloshes when you walk"
                           "Pee flows down with every step")
        :wear-mess-text (list
                            "The diaper holds the mess easily"
                            "It smooshes when you walk"
                            "Poo is leaking out of the diaper")
        :description "When you leak so often that you need a diaper so thick it nearly touches the ground. Can be reused."))
(defclass hyper-thick-latex-diaper (yadfa:diaper) ()
    (:default-initargs
        :sogginess-capacity 50000
        :messiness-capacity 500000
        :name "Hyper Thick Latex Diaper"
        :value 1000
        :thickness 1000
        :thickness-capacity 380
        :waterproof t
        :wear-wet-text (list
                           "The pee seems to have disappeared into the diaper"
                           "It sloshes when you walk"
                           "Pee flows down with every step")
        :wear-mess-text (list
                            "The diaper holds the mess easily"
                            "It smooshes when you walk"
                            "Poo is leaking out of the diaper")
        :description "For when the Thick Latex Diaper wasn't thick enough. At least it doubles as a bean bag chair."))
(defclass pullups (pullup) ()
    (:default-initargs
        :sogginess-capacity 800
        :messiness-capacity 10000
        :disposable t
        :name "Pullup"
        :description "Wear this and pretend to be a big kid"))
(defclass pullups-package (item) ()
    (:default-initargs
        :name "Package of Pullups"
        :plural-name "Packages of Pullups"
        :description "Wear these and pretend to be a big kid."
        :consumable t
        :value 250
        :use-script '(lambda (item user)
                         (declare (ignore item))
                         (format t "You tear open the package and dump all the pullups out of it.~%")
                         (loop for i from 1 to 20 do (push (make-instance 'yadfa/items:pullups) (inventory-of user))))))
(defclass cloth-pullups (pullup) ()
    (:default-initargs
        :value 200
        :sogginess-capacity 800
        :messiness-capacity 10000
        :name "Cloth Pullup"
        :description "Wear this and pretend to be a big kid. Can be reused."))
(defclass swim-diaper-cover (undies) ()
    (:default-initargs
        :waterproof t
        :value 200
        :name "Swim diaper cover"
        :description "Designed to be worn over diapers to avoid embarrassing diaper swelling incidents when in the water. Doesn't hold anything on its own"))
(defclass bandit-diaper (tabbed-briefs) ()
    (:default-initargs
        :sogginess-capacity 1400
        :messiness-capacity 10000
        :value 200
        :name "Bandit Diaper"
        :description "A diaper with the Diapered Raccoon Bandits's insignia on the front. the insignia turns yellow when the wearer wets it so the higher ups know when the lower ranks wet their diapers. Unlike most diapers, these are reusable."
        :wear-wet-text (list
                           "The insignia on the front is still blue"
                           "The insignia on the front has turned yellow"
                           "little yellow streams are leaking down from the leg guards")
        :wear-mess-text (list
                            "You can feel a slight mess back there"
                            "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                            "Poo is leaking out of the leg guards")))
(defclass bandit-adjustable-diaper (tabbed-briefs) ()
    (:default-initargs
        :sogginess-capacity 1400
        :messiness-capacity 10000
        :value 200
        :name "Bandit Adjustable Diaper"
        :description "A special diaper that can be pulled down like normal underwear so the wearer can still use the toilet. It has the bandit's insignia on the front but it doesn't change color when wet in order to make it easier to hide the state of their diaper. It has tabs on the sides for easy removal. Unlike most diapers, these are reusable."
        :wear-wet-text (list
                           "You can barely tell you wet it"
                           "It squishes when you walk"
                           "little yellow streams are leaking down from the leg guards")
        :wear-mess-text (list
                            "You can feel a slight mess back there"
                            "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                            "Poo is leaking out of the leg guards")))
(defclass bandit-female-diaper (tabbed-briefs) ()
    (:default-initargs
        :sogginess-capacity 10000
        :messiness-capacity 100000
        :thickness 300
        :thickness-capacity 100
        :value 200
        :name "Female's Bandit Diaper"
        :description "A diaper with the Diapered Raccoon Bandits's insignia on the front. the insignia turns yellow when the wearer wets it so the males know when the lower ranks wet their diapers. It is much thicker than what the males wear. Unlike most diapers, these are reusable."
        :wear-wet-text (list
                           "The insignia on the front is still pink"
                           "The insignia on the front has turned yellow"
                           "little yellow streams are leaking down from the leg guards")
        :wear-mess-text (list
                            "You can feel a slight mess back there"
                            "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                            "Poo is leaking out of the leg guards")))
(defclass bandit-swim-diaper-cover (undies) ()
    (:default-initargs
        :waterproof t
        :value 200
        :name "Bandit Swim diaper cover"
        :description "The higher ups wear these when in the water to prevent their diapers from swelling up. It has the bandits' insignia on it."))
(defclass lower-bandit-swim-diaper-cover (undies) ()
    (:default-initargs
        :waterproof t
        :value 200
        :name "Lower Bandit Swim diaper cover"
        :description "The lower ranks wear these when in the water to prevent their diapers from swelling up. It is transparent enough to show the state of the diaper and does nothing to hide the poofiness"))
(defclass female-bandit-swim-diaper-cover (undies) ()
    (:default-initargs
        :waterproof t
        :value 200
        :thickness-capacity 400
        :name "Lower Bandit Swim diaper cover"
        :description "The females wear these when in the water to prevent their diapers from swelling up. It is transparent enough to show the state of the diaper and does nothing to hide the poofiness. It is much larger than the ones the males wear to accommodate the thicker padding."))
