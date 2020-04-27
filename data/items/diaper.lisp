;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-items"; coding: utf-8-unix; -*-
(in-package :yadfa-items)
(defclass cloth-mixin (incontinence-product) ()
  (:default-initargs
   :sogginess-capacity 1400
   :messiness-capacity 1000
   :disposable nil
   :sellable t
   :wear-wet-text '(1400 "little yellow streams are leaking down from the leg guards"
                    350 "It squishes when you walk"
                    1 "You can barely tell you wet it")
   :wear-mess-text '(1000 "Poo is leaking out of the leg guards"
                     250 "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                     1 "You can feel a slight mess back there")))
(defclass rubber-mixin (incontinence-product) ()
  (:default-initargs
   :sogginess-capacity 2000
   :messiness-capacity 1700
   :waterproof t
   :disposable nil
   :sellable t
   :wear-wet-text '(2000 "little yellow streams are leaking down from the leg guards"
                    350 "It squishes when you walk"
                    1 "You can barely tell you wet it")
   :wear-mess-text '(1700 "Poo is leaking out of the leg guards"
                     250 "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                     1 "You can feel a slight mess back there")))
(defclass thick-mixin (incontinence-product) ()
  (:default-initargs
   :sogginess-capacity 4000
   :messiness-capacity 1700
   :thickness (+ 50 4/5)
   :thickness-capacity 100
   :wear-wet-text '(4000 "little yellow streams are leaking down from the leg guards"
                    2000 "The front is stained yellow"
                    1400 "It squishes when you walk"
                    1 "You can barely tell you wet it")
   :wear-mess-text '(1700 "Poo is leaking out of the leg guards"
                     850 "The back is clearly stained brown"
                     425 "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                     1 "You can feel a slight mess back there")))
(defclass diaper-package-mixin (item)
  ((diaper :type symbol
           :initarg :diaper)))
(defmethod use-script ((item diaper-package-mixin) (user base-character))
  (format t "You tear open the package and dump all the diapers out of it.~%")
  (iter (for i from 1 to 20)
    (push (make-instance (slot-value item 'diaper)) (inventory-of user))))
(defclass generic-diapers (yadfa:diaper undies) ()
  (:default-initargs
   :sogginess-capacity 100
   :messiness-capacity 20
   :name "Store Brand Diapers"
   :plural-name "Store Brand Diapers"
   :wear-wet-text '(100 "Damn thing is leaking already"
                    25 "This thing might not last much longer"
                    1 "Not leaking yet")
   :wear-mess-text '(20 "This was never designed to hold a mess, and the mess leaking down the leg guards show it"
                     1 "You can feel a slight mess back there")
   :description "These are our new Super Absorbent Briefs!!! Built to the top American standards of adult diapers, meaning they're neither super, nor absorbent."))
(defclass generic-diapers-package (diaper-package-mixin) ()
  (:default-initargs
   :name "Package of Store Brand Diapers"
   :plural-name "Packages of Store Brand Diapers"
   :description "These are our new Super Absorbent Briefs!!! Built to the top American standards of adult diapers, meaning they're neither super, nor absorbent."
   :consumable t
   :value 200
   :diaper 'generic-diapers))
(defclass generic-pullons (pullup undies) ()
  (:default-initargs
   :sogginess-capacity 100
   :messiness-capacity 20
   :name "Store Brand Pullons"
   :plural-name "Store Brand Pullons"
   :wear-wet-text '(100 "Damn thing is leaking already"
                    25 "This thing might not last much longer"
                    1 "Not leaking yet")
   :wear-mess-text '(20 "This was never designed to hold a mess, and the mess leaking down the leg guards show it"
                     1 "You can feel a slight mess back there")
   :description "Our new Super Absorbent Briefs in Pullon form!!! They look just like real underwear!!! And is about as absorbent as real underwear too!!!!"))
(defclass generic-pullons-package (diaper-package-mixin) ()
  (:default-initargs
   :name "Package of Store Brand Pullons"
   :plural-name "Packages of Store Brand Pullons"
   :description "Our new Super Absorbent Briefs in Pullon form!!! They look just like real underwear!!! And is about as absorbent as real underwear too!!!!"
   :consumable t
   :value 200
   :diaper 'generic-pullons))
(defclass incontinence-pad (stuffer) ()
  (:default-initargs
   :sogginess-capacity 1000
   :messiness-capacity 100
   :name "Incontinence Pad"
   :wear-wet-text '(
                    1000 "It's leaking"
                    250 "It's swollen with urine"
                    1 "You can barely tell you wet it")
   :wear-mess-text '(100 "The mess is falling out of the stuffer"
                     25 "The mess is about to fall out"
                     1 "You can feel a slight mess back there")
   :description "For when you think you're too old for real padding, or if you just want your existing padding to last longer"))
(defclass incontinence-pad-package (diaper-package-mixin) ()
  (:default-initargs
   :name "Package of Incontinence Pads"
   :plural-name "Packages of Incontinence Pads"
   :description "For when you think you're too old for real padding, or if you just want your existing padding to last longer"
   :consumable t
   :value 200
   :diaper 'incontinence-pad))
(defclass cloth-incontinence-pad (cloth-mixin stuffer) ()
  (:default-initargs
   :sogginess-capacity 1000
   :messiness-capacity 100
   :name "Cloth Incontinence Pad"
   :wear-wet-text '(1000 "It's leaking"
                    250 "It's swollen with urine"
                    1 "You can barely tell you wet it")
   :wear-mess-text '(100 "The mess is falling out of the stuffer"
                     25 "The mess is about to fall out"
                     0 "You can feel a slight mess back there")
   :description "For when you think you're too old for real padding, or if you just want your existing padding to last longer. Can be reused"))
(defclass diaper (yadfa:diaper undies) ()
  (:default-initargs
   :sogginess-capacity 1400
   :messiness-capacity 1000
   :name "Diaper"
   :description "A poofy diaper that can hold an accident."
   :wear-wet-text '(1400 "little yellow streams are leaking down from the leg guards"
                    700 "The front is clearly stained yellow"
                    350 "It squishes when you walk"
                    1 "You can barely tell you wet it")
   :wear-mess-text '(1000 "Poo is leaking out of the leg guards and the back is stained brown"
                     700 "The back is clearly stained brown"
                     250 "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                     1 "You can feel a slight mess back there")))
(defclass high-capacity-diaper (yadfa:diaper undies) ()
  (:default-initargs
   :sogginess-capacity 4000
   :messiness-capacity 1700
   :name "High Capacity Diaper"
   :description "Has similar capacity to the thick diaper without the extra bulk"
   :wear-wet-text '(4000 "little yellow streams are leaking down from the leg guards"
                    700 "The front is stained yellow"
                    350 "It squishes when you walk"
                    1 "You can barely tell you wet it")
   :wear-mess-text '(1700 "Poo is leaking out of the leg guards"
                     700 "The back is clearly stained brown"
                     250 "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                     1 "You can feel a slight mess back there")))
(defclass black-diaper (high-capacity-diaper) ()
  (:default-initargs
   :name "Black Diaper"
   :description "A Black diaper with a blood red landing zone"))
(defclass thick-diaper (thick-mixin yadfa:diaper undies) ()
  (:default-initargs
   :name "Thick Diaper"
   :description "A thick diaper that can hold more than an normal diaper"))
(defclass diaper-corset (thick-mixin yadfa:diaper closed-full-outfit) ()
  (:default-initargs
   :name "Diaper Corset"
   :description "Full body suit that acts as a diaper. The only thing you need to wear."))
(defclass blackjack-uniform-diaper (diaper-corset) ()
  (:default-initargs
   :name "Blackjack Uniform Diaper"
   :description "Full body suit that acts as a diaper based on the diaper corset. Has the player's name on the back and a spade on the crotch that fades when wet so everyone knows who the loser is."
   :wear-wet-text '(4000 "little yellow streams are leaking down from the leg guards"
                    2000 "The front is stained yellow"
                    1400 "It squishes when you walk"
                    150 "The spade on the front has faded away"
                    50 "The spade on the front has partially faded"
                    1 "You can barely tell you wet it")
   :wear-mess-text '(1700 "Poo is leaking out of the leg guards"
                     850 "The back is clearly stained brown"
                     425 "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                     1 "You can feel a slight mess back there")))
(defclass midnight-diaper (thick-mixin yadfa:diaper) ()
  (:default-initargs
   :name "Midnight Diaper"
   :description "A thick black diaper with blue landing zone, blue leg guards, and red tapes"))
(defclass thick-diaper-package (diaper-package-mixin) ()
  (:default-initargs
   :name "Package of Thick Diapers"
   :plural-name "Packages of Thick Diapers"
   :description "A package of A thick diapers that can hold more than an normal diapers"
   :consumable t
   :value 250
   :diaper 'thick-diaper))
(defclass midnight-diaper-package (diaper-package-mixin) ()
  (:default-initargs
   :name "Package of Midnight Diapers"
   :plural-name "Packages of Midnight Diapers"
   :description "A package of thick black diapers with blue landing zone, blue leg guards, and red tapes"
   :consumable t
   :value 250
   :diaper 'midnight-diaper))
(defclass cloth-diaper (cloth-mixin yadfa:diaper undies) ()
  (:default-initargs
   :value 200
   :name "Cloth Diaper"
   :description "A poofy diaper that can hold an accident. Can be reused."))
(defclass temple-diaper (cloth-mixin yadfa:diaper) ()
  (:default-initargs
   :name "Temple Diaper"
   :description "A diaper with weird Egyptian hieroglyphics on it"))
(defclass cursed-diaper (cloth-mixin yadfa:diaper ab-clothing) ()
  (:default-initargs
   :name "Cursed Diaper"
   :description "The diaper's tapes have a glow in the shape of a lock in front of them. You can't seem to remove them. Better think of something quick."
   :locked t
   :key nil))
(defclass rubber-diaper (rubber-mixin yadfa:diaper) ()
  (:default-initargs
   :value 250
   :name "Rubber Diaper"
   :description "A poofy rubber diaper that can hold an accident. Can be reused."))
(defclass thick-cloth-diaper (thick-mixin cloth-mixin yadfa:diaper undies) ()
  (:default-initargs
   :name "Thick Cloth Diaper"
   :description "A thick diaper that can hold more than an normal diaper. Can be reused."))
(defclass infinity-diaper (thick-mixin cloth-mixin yadfa:diaper) ()
  (:default-initargs
   :name "Infinity Diaper"
   :description "A diaper that never leaks. It is very thick and has an Ankh on the front"
   :leakproof t
   :waterproof t
   :value 1000000000
   :wear-wet-text '(2000 "The front is stained yellow"
                    1400 "It squishes when you walk"
                    1 "You can barely tell you wet it")
   :wear-mess-text '(850 "The back is clearly stained brown"
                     425 "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                     1 "You can feel a slight mess back there")))
(defclass thick-rubber-diaper (thick-mixin rubber-mixin yadfa:diaper) ()
  (:default-initargs
   :sogginess-capacity 6000
   :messiness-capacity 25000
   :name "Thick Rubber Diaper"
   :description "A thick rubber diaper that can hold more than an normal diaper. Can be reused."
   :wear-wet-text '(6000 "little yellow streams are leaking down from the leg guards"
                    350 "It squishes when you walk"
                    1 "You can barely tell you wet it")
   :wear-mess-text '(2500 "Poo is leaking out of the leg guards"
                     250 "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                     1 "You can feel a slight mess back there")))
(defclass cloth-diaper-corset (diaper-corset cloth-mixin) ()
  (:default-initargs
   :name "Cloth Diaper Corset"
   :description "Full body suit that acts as a cloth diaper. The only thing you need to wear. Can be reused"))
(defclass rubber-diaper-corset (diaper-corset rubber-mixin) ()
  (:default-initargs
   :name "Rubber Diaper Corset"
   :description "Full body suit that acts as a rubber diaper. The only thing you need to wear. Can be reused."))
(defclass disposable-swim-diaper (yadfa:diaper) ()
  (:default-initargs
   :sogginess-capacity 1400
   :messiness-capacity 1000
   :waterproof t
   :name "Disposable Swim Diaper"
   :description "A swim diaper that actually holds just the urine you expel and not all the water in the pool"
   :wear-wet-text '(1400 "little yellow streams are leaking down from the leg guards"
                    350 "It squishes when you walk"
                    1 "You can barely tell you wet it")
   :wear-mess-text '(1000 "Poo is leaking out of the leg guards"
                     250 "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                     1 "You can feel a slight mess back there")))
(defclass disposable-swim-diaper-package (diaper-package-mixin) ()
  (:default-initargs
   :name "Package of Disposable Swim Diapers"
   :plural-name "Packages of Disposable Swim Diapers"
   :description "A package of swim diapers. Unlike most swim diapers, they absorb your waste without absorbing all the water in the pool."
   :consumable t
   :value 300
   :diaper 'disposable-swim-diaper))
(defclass diaper-package (diaper-package-mixin) ()
  (:default-initargs
   :name "Package of Diapers"
   :plural-name "Packages of Diapers"
   :description "A package of poofy diapers that can hold an accident."
   :consumable t
   :value 250
   :diaper 'diaper))
(defclass kurikia-thick-diaper (yadfa:diaper) ()
  (:default-initargs
   :sogginess-capacity 10000
   :messiness-capacity 2500
   :name "Kurikia Thick Diaper"
   :value 60
   :thickness 300
   :thickness-capacity 100
   :wear-stats (list :speed -10)
   :wear-wet-text '(10000 "Pee flows down with every step"
                    2500 "It sloshes when you walk"
                    1 "The pee seems to have disappeared into the diaper")
   :wear-mess-text '(2500 "Poo is leaking out of the diaper"
                     1000 "It smooshes when you walk"
                     1 "The diaper holds the mess easily")
   :description "It's like carrying a giant pillow between your legs forcing you to waddle"))
(defclass kurikia-thick-cloth-diaper (kurikia-thick-diaper cloth-mixin) ()
  (:default-initargs
   :name "Kurikia Thick Cloth Diaper"
   :value 400
   :description "It's like carrying a giant pillow between your legs forcing you to waddle. Can be reused."))
(defclass kurikia-thick-rubber-diaper (kurikia-thick-diaper rubber-mixin) ()
  (:default-initargs
   :sogginess-capacity 15000
   :messiness-capacity 3000
   :name "Kurikia Thick Rubber Diaper"
   :value 400
   :wear-wet-text '(15000 "Pee flows down with every step"
                    2500 "It sloshes when you walk"
                    1 "The pee seems to have disappeared into the diaper")
   :wear-mess-text '(3000 "Poo is leaking out of the diaper"
                     1000 "It smooshes when you walk"
                     1 "The diaper holds the mess easily")
   :description "A giant thick waterproof diaper. Yes, the waddle is noticeable"))
(defclass hyper-thick-diaper (yadfa:diaper) ()
  (:default-initargs
   :sogginess-capacity 50000
   :messiness-capacity 8000
   :name "Hyper Thick Diaper"
   :value 700
   :thickness 1000
   :thickness-capacity 380
   :wear-stats (list :speed -20)
   :wear-wet-text '(50000 "Pee flows down with every step"
                    12500 "It sloshes when you walk"
                    1 "The pee seems to have disappeared into the diaper")
   :wear-mess-text '(8000 "Poo is leaking out of the diaper"
                     2000 "It smooshes when you walk"
                     1 "The diaper holds the mess easily")
   :description "When you leak so often that you need a diaper so thick it nearly touches the ground"))
(defclass hyper-thick-cloth-diaper (hyper-thick-diaper cloth-mixin) ()
  (:default-initargs
   :name "Hyper Thick Cloth Diaper"
   :value 1000
   :description "When you leak so often that you need a diaper so thick it nearly touches the ground. Can be reused."))
(defclass hyper-thick-rubber-diaper (hyper-thick-diaper rubber-mixin) ()
  (:default-initargs
   :sogginess-capacity 60000
   :messiness-capacity 9000
   :name "Hyper Thick Rubber Diaper"
   :value 1000
   :wear-wet-text '(60000 "Pee flows down with every step"
                    12500 "It sloshes when you walk"
                    1 "The pee seems to have disappeared into the diaper")
   :wear-mess-text '(9000 "Poo is leaking out of the diaper"
                     2000 "It smooshes when you walk"
                     1 "The diaper holds the mess easily")
   :description "For when the Thick Rubber Diaper wasn't thick enough. At least it doubles as a bean bag chair."))
(defclass pullups (pullup undies) ()
  (:default-initargs
   :sogginess-capacity 800
   :messiness-capacity 1000
   :name "Pullup"
   :description "Wear this and pretend to be a big kid"
   :wear-wet-text '(800 "it is completely drenched, you sure you're ready for these?"
                    200 "The little pictures on the front have faded"
                    1 "the pictures haven't faded yet")
   :wear-mess-text '(1000 "Poo is leaking out of the pullup"
                     250 "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                     1 "You can feel a slight mess back there")))
(defclass pullups-package (diaper-package-mixin) ()
  (:default-initargs
   :name "Package of Pullups"
   :plural-name "Packages of Pullups"
   :description "Wear these and pretend to be a big kid."
   :consumable t
   :value 250
   :diaper 'pullups))
(defclass cloth-pullups (pullup cloth-mixin undies) ()
  (:default-initargs
   :value 200
   :disposable nil
   :sellable t
   :name "Cloth Pullup"
   :description "Wear this and pretend to be a big kid. Can be reused."
   :wear-wet-text '(800 "it is completely drenched, you sure you're ready for these?"
                    200 "it is noticeably wet"
                    1 "the pictures haven't faded yet")))
(defclass temple-pullups (pullup cloth-mixin ab-clothing) ()
  (:default-initargs
   :value 200
   :name "Temple Pullups"
   :description "A pullup with weird Egyptian hieroglyphics on it"
   :wear-wet-text '(800 "it is completely drenched, you sure you're ready for these?"
                    200 "it is noticeably wet"
                    1 "the pictures haven't faded yet")
   :wear-mess-text '(1000 "Poo is leaking out of the pullup"
                     250 "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                     1 "You can feel a slight mess back there")))
(defclass navy-pullups (pullup cloth-mixin) ()
  (:default-initargs
   :value 200
   :name "Navy Pullup"
   :description "Has the Navy insignia on the front. The Navy apparently expects you to keep these dry, but keeps them around just in case. They're made of cloth because they don't expect you to use them that often."
   :wear-wet-text '(800 "it is completely drenched, you sure you're ready for these?"
                    200 "it is noticeably wet"
                    1 "the pictures haven't faded yet")
   :wear-mess-text '(1000 "Poo is leaking out of the pullup"
                     250 "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                     1 "You can feel a slight mess back there")))
(defclass rubber-pullups (pullup rubber-mixin) ()
  (:default-initargs
   :value 200
   :name "Rubber Pullup"
   :description "Wear this and pretend to be a big kid. Can be reused."
   :wear-wet-text '(800 "it is completely drenched, you sure you're ready for these?"
                    200 "it is noticeably wet"
                    1 "the pictures haven't faded yet")
   :wear-mess-text '(1000 "Poo is leaking out of the pullup"
                     250 "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                     1 "You can feel a slight mess back there")))
(defclass swim-diaper-cover (closed-bottoms undies) ()
  (:default-initargs
   :waterproof t
   :value 200
   :name "Swim diaper cover"
   :description "Designed to be worn over diapers to avoid embarrassing diaper swelling incidents when in the water. Doesn't hold anything on its own"))
(defclass bandit-diaper (yadfa:diaper) ()
  (:default-initargs
   :sogginess-capacity 1400
   :messiness-capacity 1000
   :value 200
   :disposable nil
   :sellable t
   :name "Bandit Diaper"
   :description "A diaper with the Diapered Raccoon Bandits' insignia on the front. the insignia turns yellow when the wearer wets it so the higher ups know when the lower ranks wet their diapers. Unlike most diapers, these are reusable."
   :wear-wet-text '(1400 "little yellow streams are leaking down from the leg guards"
                    350 "The insignia on the front has turned yellow"
                    1 "The insignia on the front is still blue")
   :wear-mess-text '(1000 "Poo is leaking out of the leg guards"
                     250 "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                     1 "You can feel a slight mess back there")))
(defclass bandit-adjustable-diaper (bandit-diaper undies) ()
  (:default-initargs
   :name "Bandit Adjustable Diaper"
   :description "A special diaper that can be pulled down like normal underwear so the wearer can still use the toilet. It has the bandit's insignia on the front which turns yellow when the diaper is used. It has tabs on the sides for easy removal. Unlike most diapers, these are reusable."))
(defclass bandit-female-diaper (kurikia-thick-diaper bandit-diaper ab-clothing) ()
  (:default-initargs
   :name "Female's Bandit Diaper"
   :description "A diaper with the Diapered Raccoon Bandits' insignia on the front. the insignia turns yellow when the wearer wets it so the males know when the lower ranks wet their diapers. It is much thicker than what the males wear. Unlike most diapers, these are reusable."
   :wear-wet-text '(10000 "little yellow streams are leaking down from the leg guards"
                    2500 "The insignia on the front has turned yellow"
                    1 "The insignia on the front is still pink")
   :wear-mess-text '(10000 "Poo is leaking out of the leg guards"
                     2500 "There is a slight bulge in the back and it smells, but you'll be fine as long as you don't need to sit down"
                     1 "You can feel a slight mess back there")))
(defclass bandit-swim-diaper-cover (closed-bottoms) ()
  (:default-initargs
   :waterproof t
   :value 200
   :name "Bandit Swim diaper cover"
   :description "The higher ups wear these when in the water to prevent their diapers from swelling up. It has the bandits' insignia on it."))
(defclass lower-bandit-swim-diaper-cover (closed-bottoms) ()
  (:default-initargs
   :waterproof t
   :value 200
   :name "Lower Bandit Swim diaper cover"
   :description "The lower ranks wear these when in the water to prevent their diapers from swelling up. It is transparent enough to show the state of the diaper and does nothing to hide the poofiness"))
(defclass female-bandit-swim-diaper-cover (closed-bottoms) ()
  (:default-initargs
   :waterproof t
   :value 200
   :thickness-capacity 400
   :name "Lower Bandit Swim diaper cover"
   :description "The females wear these when in the water to prevent their diapers from swelling up. It is transparent enough to show the state of the diaper and does nothing to hide the poofiness. It is much larger than the ones the males wear to accommodate the thicker padding."))
(defclass pink-frilly-diaper (yadfa:diaper ab-clothing undies) ()
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
