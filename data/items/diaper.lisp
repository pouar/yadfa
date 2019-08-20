;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-items"; coding: utf-8-unix; -*-
(in-package :yadfa-items)
(defclass generic-diapers (yadfa:diaper) ()
  (:default-initargs
   :sogginess-capacity 100
   :messiness-capacity 200
   :name "Store Brand Diapers"
   :plural-name "Store Brand Diapers"
   :wear-wet-text '(100 "Damn thing is leaking already"
                    25 "This thing might not last much longer"
                    1 "Not leaking yet")
   :wear-mess-text '(200 "This was never designed to hold a mess, and the mess leaking down the leg guards show it"
                     1 "You can feel a slight mess back there")
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
                 (iter (for i from 1 to 20)
                   (push (make-instance 'yadfa-items:generic-diapers) (inventory-of user))))))
(defclass generic-pullons (pullon) ()
  (:default-initargs
   :sogginess-capacity 100
   :messiness-capacity 200
   :name "Store Brand Pullons"
   :plural-name "Store Brand Pullons"
   :wear-wet-text '(100 "Damn thing is leaking already"
                    25 "This thing might not last much longer"
                    1 "Not leaking yet")
   :wear-mess-text '(200 "This was never designed to hold a mess, and the mess leaking down the leg guards show it"
                     1 "You can feel a slight mess back there")
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
                 (iter (for i from 1 to 20)
                   (push (make-instance 'yadfa-items:generic-pullons) (inventory-of user))))))
(defclass incontinence-pad (yadfa:incontinence-pad) ()
  (:default-initargs
   :sogginess-capacity 1000
   :messiness-capacity 1000
   :name "Incontinence Pad"
   :wear-wet-text '(
                    1000 "It's leaking"
                    250 "It's swollen with urine"
                    1 "You can barely tell you wet it")
   :wear-mess-text '(1000 "The mess is falling out of the stuffer"
                     250 "The mess is about to fall out"
                     1 "You can feel a slight mess back there")
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
                 (iter (for i from 1 to 20)
                   (push (make-instance 'yadfa-items:incontinence-pad) (inventory-of user))))))
(defclass cloth-incontinence-pad (yadfa:incontinence-pad) ()
  (:default-initargs
   :sogginess-capacity 1000
   :messiness-capacity 1000
   :disposable nil
   :sellable t
   :name "Cloth Incontinence Pad"
   :wear-wet-text '(1000 "It's leaking"
                    250 "It's swollen with urine"
                    1 "You can barely tell you wet it")
   :wear-mess-text '(1000 "The mess is falling out of the stuffer"
                     250 "The mess is about to fall out"
                     0 "You can feel a slight mess back there")
   :description "For when you think you're too old for real padding, or if you just want your existing padding to last longer. Can be reused"))
(defclass diaper (yadfa:diaper) ()
  (:default-initargs
   :sogginess-capacity 1400
   :messiness-capacity 10000
   :name "Diaper"
   :description "A poofy diaper that can hold an accident."
   :wear-wet-text '(1400 "little yellow streams are leaking down from the leg guards"
                    700 "The front is clearly stained yellow"
                    350 "It squishes when you walk"
                    1 "You can barely tell you wet it")
   :wear-mess-text '(10000 "Poo is leaking out of the leg guards and the back is stained brown"
                     9000 "The back is clearly stained brown"
                     2500 "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                     1 "You can feel a slight mess back there")))
(defclass thick-diaper (yadfa:diaper) ()
  (:default-initargs
   :sogginess-capacity 4000
   :messiness-capacity 17000
   :name "Thick Diaper"
   :thickness (+ 50 4/5)
   :thickness-capacity 100
   :description "A thick diaper that can hold more than an normal diaper"
   :wear-wet-text '(4000 "little yellow streams are leaking down from the leg guards"
                    2000 "The front is stained yellow"
                    1400 "It squishes when you walk"
                    1 "You can barely tell you wet it")
   :wear-mess-text '(17000 "Poo is leaking out of the leg guards"
                     8500 "The back is clearly stained brown"
                     4250 "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                     1 "You can feel a slight mess back there")))
(defclass midnight-diaper (thick-diaper) ()
  (:default-initargs
   :name "Midnight Diaper"
   :description "A thick black diaper with blue landing zone, blue leg guards, and red tapes"))
(defclass thick-diaper-package (item) ()
  (:default-initargs
   :name "Package of Thick Diapers"
   :plural-name "Packages of Thick Diapers"
   :description "A package of A thick diapers that can hold more than an normal diapers"
   :consumable t
   :value 250
   :use-script '(lambda (item user)
                 (declare (ignore item))
                 (format t "You tear open the package and dump all the diapers out of it.~%")
                 (loop for i from 1 to 20 do (push (make-instance 'yadfa-items:thick-diaper) (inventory-of user))))))
(defclass midnight-diaper-package (item) ()
  (:default-initargs
   :name "Package of Midnight Diapers"
   :plural-name "Packages of Midnight Diapers"
   :description "A package of thick black diapers with blue landing zone, blue leg guards, and red tapes"
   :consumable t
   :value 250
   :use-script '(lambda (item user)
                 (declare (ignore item))
                 (format t "You tear open the package and dump all the diapers out of it.~%")
                 (iter (for i from 1 to 20)
                   (push (make-instance 'yadfa-items:midnight-diaper) (inventory-of user))))))
(defclass cloth-diaper (yadfa:diaper) ()
  (:default-initargs
   :sogginess-capacity 1400
   :messiness-capacity 10000
   :value 200
   :disposable nil
   :sellable t
   :name "Cloth Diaper"
   :description "A poofy diaper that can hold an accident. Can be reused."
   :wear-wet-text '(1400 "little yellow streams are leaking down from the leg guards"
                    350 "It squishes when you walk"
                    1 "You can barely tell you wet it")
   :wear-mess-text '(10000 "Poo is leaking out of the leg guards"
                     2500 "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                     1 "You can feel a slight mess back there")))
(defclass rubber-diaper (yadfa:diaper) ()
  (:default-initargs
   :sogginess-capacity 2000
   :messiness-capacity 17000
   :value 250
   :waterproof t
   :disposable nil
   :sellable t
   :name "Rubber Diaper"
   :description "A poofy rubber diaper that can hold an accident. Can be reused."
   :wear-wet-text '(2000 "little yellow streams are leaking down from the leg guards"
                    350 "It squishes when you walk"
                    1 "You can barely tell you wet it")
   :wear-mess-text '(17000 "Poo is leaking out of the leg guards"
                     2500 "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                     1 "You can feel a slight mess back there")))
(defclass thick-cloth-diaper (thick-diaper cloth-diaper) ()
  (:default-initargs
   :name "Thick Cloth Diaper"
   :description "A thick diaper that can hold more than an normal diaper. Can be reused."))
(defclass thick-rubber-diaper (thick-diaper rubber-diaper) ()
  (:default-initargs
   :sogginess-capacity 6000
   :messiness-capacity 25000
   :name "Thick Rubber Diaper"
   :description "A thick rubber diaper that can hold more than an normal diaper. Can be reused."))
(defclass disposable-swim-diaper (yadfa:diaper) ()
  (:default-initargs
   :sogginess-capacity 1400
   :messiness-capacity 10000
   :waterproof t
   :name "Disposable Swim Diaper"
   :description "A swim diaper that actually holds just the urine you expel and not all the water in the pool"
   :wear-wet-text '(1400 "little yellow streams are leaking down from the leg guards"
                    350 "It squishes when you walk"
                    1 "You can barely tell you wet it")
   :wear-mess-text '(10000 "Poo is leaking out of the leg guards"
                     2500 "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                     1 "You can feel a slight mess back there")))
(defclass disposable-swim-diaper-package (item) ()
  (:default-initargs
   :name "Package of Disposable Swim Diapers"
   :plural-name "Packages of Disposable Swim Diapers"
   :description "A package of swim diapers. Unlike most swim diapers, they absorb your waste without absorbing all the water in the pool."
   :consumable t
   :value 300
   :use-script '(lambda (item user)
                 (declare (ignore item))
                 (format t "You tear open the package and dump all the diapers out of it.~%")
                 (iter (for i from 1 to 20)
                   (push (make-instance 'yadfa-items:disposable-swim-diaper) (inventory-of user))))))
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
                 (iter (for i from 1 to 20)
                   (push (make-instance 'yadfa-items:diaper) (inventory-of user))))))
(defclass kurikia-thick-diaper (yadfa:diaper) ()
  (:default-initargs
   :sogginess-capacity 10000
   :messiness-capacity 25000
   :name "Kurikia Thick Diaper"
   :value 60
   :thickness 300
   :thickness-capacity 100
   :wear-stats (list :speed -10)
   :wear-wet-text '(10000 "Pee flows down with every step"
                    2500 "It sloshes when you walk"
                    1 "The pee seems to have disappeared into the diaper")
   :wear-mess-text '(25000 "Poo is leaking out of the diaper"
                     10000 "It smooshes when you walk"
                     1 "The diaper holds the mess easily")
   :description "It's like carrying a giant pillow between your legs forcing you to waddle"))
(defclass kurikia-thick-cloth-diaper (kurikia-thick-diaper cloth-diaper) ()
  (:default-initargs
   :name "Kurikia Thick Cloth Diaper"
   :value 400
   :description "It's like carrying a giant pillow between your legs forcing you to waddle. Can be reused."))
(defclass kurikia-thick-rubber-diaper (kurikia-thick-diaper rubber-diaper) ()
  (:default-initargs
   :sogginess-capacity 15000
   :messiness-capacity 30000
   :waterproof t
   :name "Kurikia Thick Rubber Diaper"
   :value 400
   :wear-wet-text '(15000 "Pee flows down with every step"
                    2500 "It sloshes when you walk"
                    1 "The pee seems to have disappeared into the diaper")
   :wear-mess-text '(30000 "Poo is leaking out of the diaper"
                     10000 "It smooshes when you walk"
                     1 "The diaper holds the mess easily")
   :description "A giant thick waterproof diaper. Yes, the waddle is noticable"))
(defclass hyper-thick-diaper (yadfa:diaper) ()
  (:default-initargs
   :sogginess-capacity 50000
   :messiness-capacity 80000
   :name "Hyper Thick Diaper"
   :value 700
   :thickness 1000
   :thickness-capacity 380
   :wear-stats (list :speed -20)
   :wear-wet-text '(50000 "Pee flows down with every step"
                    12500 "It sloshes when you walk"
                    1 "The pee seems to have disappeared into the diaper")
   :wear-mess-text '(80000 "Poo is leaking out of the diaper"
                     20000 "It smooshes when you walk"
                     1 "The diaper holds the mess easily")
   :description "When you leak so often that you need a diaper so thick it nearly touches the ground"))
(defclass hyper-thick-cloth-diaper (hyper-thick-diaper cloth-diaper) ()
  (:default-initargs
   :name "Hyper Thick Cloth Diaper"
   :value 1000
   :description "When you leak so often that you need a diaper so thick it nearly touches the ground. Can be reused."))
(defclass hyper-thick-rubber-diaper (hyper-thick-diaper rubber-diaper) ()
  (:default-initargs
   :sogginess-capacity 60000
   :messiness-capacity 90000
   :name "Hyper Thick Rubber Diaper"
   :value 1000
   :wear-wet-text '(60000 "Pee flows down with every step"
                    12500 "It sloshes when you walk"
                    1 "The pee seems to have disappeared into the diaper")
   :wear-mess-text '(90000 "Poo is leaking out of the diaper"
                     20000 "It smooshes when you walk"
                     1 "The diaper holds the mess easily")
   :description "For when the Thick Rubber Diaper wasn't thick enough. At least it doubles as a bean bag chair."))
(defclass pullups (pullup) ()
  (:default-initargs
   :sogginess-capacity 800
   :messiness-capacity 10000
   :name "Pullup"
   :description "Wear this and pretend to be a big kid"
   :wear-wet-text '(800 "it is completely drenched, you sure you're ready for these?"
                    200 "The little pictures on the front have faded"
                    1 "the pictures haven't faded yet")
   :wear-mess-text '(10000 "Poo is leaking out of the pullup"
                     2500 "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                     1 "You can feel a slight mess back there")))
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
                 (iter (for i from 1 to 20)
                   (push (make-instance 'yadfa-items:pullups) (inventory-of user))))))
(defclass cloth-pullups (pullups) ()
  (:default-initargs
   :value 200
   :disposable nil
   :sellable t
   :name "Cloth Pullup"
   :description "Wear this and pretend to be a big kid. Can be reused."
   :wear-wet-text '(800 "it is completely drenched, you sure you're ready for these?"
                    200 "it is noticably wet"
                    1 "the pictures haven't faded yet")))
(defclass navy-pullups (cloth-pullups) ()
  (:default-initargs
   :value 200
   :name "Navy Pullup"
   :description "Has the Navy insignia on the front. The Navy apparently expects you to keep these dry, but keeps them around just in case. They're made of cloth because they don't expect you to use them that often."))
(defclass rubber-pullups (pullups) ()
  (:default-initargs
   :value 200
   :disposable nil
   :sellable t
   :waterproof t
   :name "Rubber Pullup"
   :description "Wear this and pretend to be a big kid. Can be reused."
   :wear-wet-text '(800 "it is completely drenched, you sure you're ready for these?"
                    200 "it is noticably wet"
                    1 "the pictures haven't faded yet")))
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
   :disposable nil
   :sellable t
   :name "Bandit Diaper"
   :description "A diaper with the Diapered Raccoon Bandits's insignia on the front. the insignia turns yellow when the wearer wets it so the higher ups know when the lower ranks wet their diapers. Unlike most diapers, these are reusable."
   :wear-wet-text '(1400 "little yellow streams are leaking down from the leg guards"
                    350 "The insignia on the front has turned yellow"
                    1 "The insignia on the front is still blue")
   :wear-mess-text '(10000 "Poo is leaking out of the leg guards"
                     2500 "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                     1 "You can feel a slight mess back there")))
(defclass bandit-adjustable-diaper (bandit-diaper) ()
  (:default-initargs
   :name "Bandit Adjustable Diaper"
   :description "A special diaper that can be pulled down like normal underwear so the wearer can still use the toilet. It has the bandit's insignia on the front whcih turns yellow when the diaper is used. It has tabs on the sides for easy removal. Unlike most diapers, these are reusable."))
(defclass bandit-female-diaper (bandit-diaper) ()
  (:default-initargs
   :sogginess-capacity 10000
   :messiness-capacity 25000
   :thickness 300
   :thickness-capacity 100
   :wear-stats (list :speed -10)
   :name "Female's Bandit Diaper"
   :description "A diaper with the Diapered Raccoon Bandits's insignia on the front. the insignia turns yellow when the wearer wets it so the males know when the lower ranks wet their diapers. It is much thicker than what the males wear. Unlike most diapers, these are reusable."
   :wear-wet-text '(10000 "little yellow streams are leaking down from the leg guards"
                    2500 "The insignia on the front has turned yellow"
                    1 "The insignia on the front is still pink")
   :wear-mess-text '(100000 "Poo is leaking out of the leg guards"
                     25000 "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                     1 "You can feel a slight mess back there")))
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
(defclass pink-frilly-diaper (yadfa:diaper) ()
  (:default-initargs
   :sogginess-capacity 5000
   :name "Pink Diaper"
   :thickness (+ 50 4/5)
   :thickness-capacity 100
   :description "A sissy pink frilly diaper. You look adorable with this"
   :wear-wet-text '(5000 "little yellow streams are leaking down from the leg guards"
                    2000 "It squishes when you walk"
                    500 "The hearts on the front have faded"
                    1 "You can barely tell you wet it")))
