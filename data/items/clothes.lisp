;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-items"; coding: utf-8-unix; -*-
(in-package :yadfa-items)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defonesie onesie (ab-clothing-mixin) ()
    (:default-initargs
     :name "Onesie"
     :description "A onesie"
     :value 400
     :onesie-bulge-text '((50 "It fits over your diaper so tightly it looks like the buttons are about to go flying off"
                           20 "It fits over your diaper quite nicely"
                           0 "It's so baggy that what you're wearing under there is quite visible")
                          .
                          (100 "You are unable to get the buttons to snap so you just leave the flaps open"
                           #.(* 12 25) "Your padding is clearly visible under there"
                           #.(* 11 25) "The flaps just barely cover the diapers"
                           #.(* 1/2 25) "The flaps hang over covering your padding like a dress"
                           0 "The flaps hang over covering your underwear like a dress")))))
(defonesie roomy-onesie (ab-clothing-mixin) ()
  (:default-initargs
   :name "Roomy Onesie"
   :description "A onesie intended to accommodate thicker diapers"
   :onesie-thickness-capacity (cons (* 16 (+ 25 2/5)) nil)
   :value 400
   :onesie-bulge-text '((60 "It fits over your diaper so tightly it looks like the buttons are about to go flying off"
                         20 "It fits over your diaper quite nicely"
                         0 "It's so baggy that what you're wearing under there is quite visible")
                        .
                        (#.(* 16 (+ 25 2/5)) "You are unable to get the buttons to snap so you just leave the flaps open"
                         #.(* 12 25) "Your padding is clearly visible under there"
                         #.(* 11 25) "The flaps just barely cover the diapers"
                         #.(* 1/2 25) "The flaps hang over covering your padding like a dress"
                         0 "The flaps hang over covering your underwear like a dress"))))
(defclass short-dress (yadfa:dress) ()
  (:default-initargs
   :name "Short Dress"
   :plural-name "Short Dresses"
   :value 150
   :description "A short breezy dress."
   :bulge-text '(225 "Your padding is clearly visible under your dress"
                 200 "Your padding is slightly visible under your dress"
                 175 "The dress does a good job hiding your padding, as long as you're standing still"
                 #.(+ 162 1/2) "The dress does a good job hiding your padding, unless a gust of wind happens to blow by"
                 #.(* 1/2 25) "The dress does a good job hiding your padding"
                 0 "It fits quite loosely")
   :thickness-capacity (* 16 (+ 25 2/5))))
(defclass dress (yadfa:dress) ()
  (:default-initargs
   :name "Dress"
   :plural-name "Dresses"
   :value 150
   :description "A cute dress."
   :bulge-text '(#.(* 4 25) "The bottom of your dress has poofed out humorously"
                 #.(* 3 25) "There is a slight bulge, but it's not too noticeable"
                 #.(* 1/2 25) "The dress does a good job hiding your padding"
                 0 "It fits snuggly")
   :thickness-capacity (* 16 (+ 25 2/5))))
(defclass sundress (yadfa:dress) ()
  (:default-initargs
   :name "Sundress"
   :plural-name "Sundresses"
   :value 200
   :description "A loose fitting dress."
   :bulge-text '(480 "Your padding is completely visible"
                 430 "Your padding is slightly visible under your dress"
                 #.(* 1/2 25) "The dress does a good job hiding your padding"
                 0 "It fits quite loosely")
   :thickness-capacity nil))
(defclass toddler-dress (yadfa:dress ab-clothing-mixin) ()
  (:default-initargs
   :name "Toddler's Dress"
   :plural-name "Toddler Dresses"
   :value 600
   :description "A frilly pink dress fit for a big toddler."
   :bulge-text '(75 "Your padding is clearly visible under your dress"
                 50 "Your padding is slightly visible under your dress"
                 25 "The dress does a good job hiding your padding, as long as you're standing still"
                 12 "The dress does a good job hiding your padding, unless a gust of wind happens to blow by"
                 0 "The dress easily covers your underwear")
   :thickness-capacity nil))
(defclass tshirt (shirt) ()
  (:default-initargs
   :name "T-Shirt"
   :value 50
   :description "A simple plain t-shirt."))
(defclass black-leather-jacket (shirt) ()
  (:default-initargs
   :name "Black Leather Jacket"
   :value 500
   :description "Makes you look tough"))
(defclass cannibal-corpse-tshirt (tshirt) ()
  (:default-initargs
   :name "Cannibal Corpse T-Shirt"
   :value 200
   :description "T-Shirt with Cannibal Corpse's logo on it"))
(defclass jeans (pants) ()
  (:default-initargs
   :name "Jeans"
   :plural-name "Jeans"
   :value 100
   :description "A simple pair of jeans."
   :bulge-text '(12 "Your padding keeps poking out of the top of your pants"
                 0 "It fits snuggly")))
(defclass snap-jeans (jeans snap-bottoms) ()
  (:default-initargs
   :name "Snap Jeans"
   :plural-name "Snap Jeans"
   :value 200
   :description "These jeans have snaps on them so they simply unsnap when your diaper expands"))
(defclass baggy-jeans (jeans) ()
  (:default-initargs
   :name "Baggy Jeans"
   :plural-name "Baggy Jeans"
   :value 150
   :description "For when you need to hide that diaper of yours, sorta"
   :bulge-text '(50 "Your pants puff out humorously"
                 12 "Your padding is visibly poking out of the top of your pants"
                 0 "It fits loosely")
   :thickness-capacity (* 16 (+ 25 2/5))))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defonesie rubber-onesie () ()
    (:default-initargs
     :onesie-waterproof t
     :value 600
     :name "Black Rubber Onesie"
     :onesie-thickness-capacity (cons (* 16 (+ 25 2/5)) nil)
     :onesie-bulge-text '((60 "It fits over your diaper so tightly it looks like the buttons are about to go flying off"
                           50 "It stretches which helps accommodate your thick padding"
                           20 "It fits over your diaper quite nicely"
                           0 "It's so baggy that what you're wearing under there is quite visible")
                          .
                          (#.(* 16 (+ 25 2/5)) "You are unable to get the buttons to snap so you just leave the flaps open"
                           #.(* 12 25) "Your padding is clearly visible under there"
                           #.(* 11 25) "The flaps just barely cover the diapers"
                           #.(* 1/2 25) "The flaps hang over covering your padding like a dress"
                           0 "The flaps hang over covering your underwear like a dress"))
     :description "An awesome black rubber onesie")))
(defonesie stretchable-rubber-onesie (rubber-onesie) ()
  (:default-initargs
   :onesie-thickness-capacity (cons nil nil)
   :onesie-thickness-capacity-threshold (cons nil nil)
   :onesie-bulge-text '((60 "The onesie has easily stretched to accommodate your padding"
                         20 "The diaper bulge makes it clear what you're wearing under there"
                         0 "It fits snuggly")
                        .
                        (#.(* 12 25) "Your padding is clearly visible under there"
                         #.(* 11 25) "The flaps just barely cover the diapers"
                         #.(* 1/2 25) "The flaps hang over covering your padding like a dress"
                         0 "The flaps hang over covering your underwear like a dress"))
   :name "Black Rubber Onesie"
   :description "An awesome black rubber onesie that stretches to fit your humongous diapers"))
(defclass orca-suit (closed-full-outfit) ()
  (:default-initargs
   :waterproof t
   :value 1000
   :thickness-capacity (* 16 (+ 25 2/5))
   :bulge-text '(60 "You look like one of those pictures drawn by Kurikia"
                 20 "The diaper bulge makes it clear what you're wearing under there"
                 0 "It fits snuggly")
   :name "Orca Suit"
   :description "An orca suit similar to the one Gabby wears."))
(defclass stretchable-orca-suit (orca-suit) ()
  (:default-initargs
   :value 1500
   :thickness-capacity nil
   :thickness-capacity-threshold nil
   :name "Stretchable Orca Suit"
   :description "A variant of the Orca Suit that stretches to fit your humongous diapers"))
(defclass orca-suit-lite (orca-suit) ()
  (:default-initargs
   :value 1000
   :thickness-capacity (* 16 (+ 25 2/5))
   :name "Orca Suit Lite"
   :description "An orca suit similar to the one Gabby wears, minus the swim boots and arm covers. You don't need 'em"))
(defclass stretchable-orca-suit-lite (stretchable-orca-suit orca-suit-lite) ()
  (:default-initargs
   :name "Stretchable Orca Suit Lite"
   :description "A variant of the Orca Suit Lite that stretches to fit your humongous diapers"))
(defclass boxers (undies closed-bottoms) ()
  (:default-initargs
   :name "Boxers"
   :plural-name "Boxers"
   :description "You sure wearing these is a good idea piddler?"
   :value 100))
(defclass panties (undies closed-bottoms) ()
  (:default-initargs
   :name "Panties"
   :plural-name "Panties"
   :description "You sure wearing these is a good idea piddler?"
   :value 100))
(defclass bra (undies) ()
  (:default-initargs
   :name "Bra"
   :description "Prevents bouncing and indecent exposure, whether this is a good thing or not depends on your point of view."
   :value 100))
(defclass bikini-top (shirt) ()
  (:default-initargs
   :name "Bikini Top"
   :description "A sports bikini top"
   :value 50))
(defclass tunic (yadfa:dress) ()
  (:default-initargs
   :name "Tunic"
   :value 100
   :description "For when a toddler's dress is too sissy."
   :bulge-text '(75 "Your padding is clearly visible under your tunic"
                 50 "Your padding is slightly visible under your tunic"
                 25 "The tunic does a good job hiding your padding, as long as you're standing still"
                 12 "The tunic does a good job hiding your padding, unless a gust of wind happens to blow by"
                 0 "The tunic easily covers your underwear")
   :thickness-capacity nil))

(defclass bandit-uniform-tunic (tunic) ()
  (:default-initargs
   :name "Bandit Uniform Tunic"
   :value 200
   :description "This tunic has the Diapered Raccoon Bandits' insignia on it. The Diapered Raccoon Bandits usually don't wear pants since it easier to change their diapers without them. The tunics are for the higher ups as it allows them to conceal the state of their diaper. A privilege the lower ranks don't have."
   :bulge-text '(75 "Your padding is clearly visible under your tunic"
                 50 "Your padding is slightly visible under your tunic"
                 12 "The tunic easily covers your padding"
                 0 "The tunic easily covers your underwear")
   :thickness-capacity-threshold nil))

(defclass bandit-uniform-shirt (shirt) ()
  (:default-initargs
   :name "Bandit Uniform Shirt"
   :value 100
   :description "This shirt has the Diapered Raccoon Bandits' insignia on it. It's for the lower ranks as they're not allowed to conceal the state of their diaper, unlike the higher ranks."))
(defclass bandit-uniform-sports-bikini-top (bikini-top) ()
  (:default-initargs
   :name "Bandit Uniform Sports Bikini Top"
   :description "A sports bikini top that has the Raccoon Bandits' insignia on it."))
(defonesie bandit-swimsuit () ()
  (:default-initargs
   :onesie-waterproof t
   :value 600
   :name "Bandit Swimsuit"
   :onesie-thickness-capacity (cons (* 16 (+ 25 2/5)) nil)
   :onesie-bulge-text '((60 "It fits over your diaper so tightly it looks like the buttons are about to go flying off"
                         50 "It stretches which helps accommodate your thick padding"
                         20 "It fits over your diaper quite nicely"
                         0 "It's so baggy that what you're wearing under there is quite visible")
                        .
                        (#.(* 16 (+ 25 2/5)) "You are unable to get the buttons to snap so you just leave the flaps open"
                         #.(* 12 25) "Your padding is clearly visible under there"
                         #.(* 11 25) "The flaps just barely cover the diapers"
                         #.(* 1/2 25) "The flaps hang over covering your padding like a dress"
                         0 "The flaps hang over covering your underwear like a dress"))
   :description "A one piece swimsuit for the higher ups to wear when in the water. It resembles a woman's one piece swimsuit, but has snaps on the bottom to make diaper changes easier. It has the bandits' insignia on the front. This model has a skirt in order to hide the poofiness of their diaper."))

(defclass skirt (yadfa:skirt) ()
  (:default-initargs
   :name "Skirt"
   :value 100
   :description "A loose fitting skirt."
   :bulge-text '(75 "Your padding is clearly visible under your dress"
                 50 "Your padding is slightly visible under your dress"
                 25 "The dress does a good job hiding your padding, as long as you're standing still"
                 12 "The dress does a good job hiding your padding, unless a gust of wind happens to blow by"
                 0 "It fits quite loosely")
   :thickness-capacity nil
   :thickness-capacity-threshold nil))
(defclass navy-skirt (skirt) ()
  (:default-initargs
   :name "Navy Skirt"
   :value 200
   :description "A loose fitting skirt part of the Navy uniform for girls."))
(defclass denim-skirt (skirt jeans) ()
  (:default-initargs
   :name "Denim Skirt"
   :value 100
   :description "A skirt made from the same fabric as jeans."))
(defclass navy-shirt (yadfa:shirt) ()
  (:default-initargs
   :name "Navy Shirt"
   :value 200
   :description "The top half of a Navy uniform"))
(defclass pirate-shirt (yadfa:shirt) ()
  (:default-initargs
   :name "Pirate Shirt"
   :value 100
   :description "The top half of a pirate outfit"))
(defclass pirate-dress (short-dress) ()
  (:default-initargs
   :name "Pirate Dress"
   :plural-name "Pirate Dresses"
   :value 100
   :description "A Pirate Dress"))
(defclass fursuit (closed-full-outfit) ()
  (:default-initargs
   :name "Fursuit"
   :value 15000
   :thickness-capacity 400
   :sogginess-capacity 200
   :messiness-capacity 100
   :description "A warm and snuggly Fursuit. Makes you cute and huggable."
   :bulge-text '(200 "The circular bulge around your waist makes it obvious what you're wearing under there."
                 0 "It's warm and snuggly")
   :wear-wet-text '(200 "Just wipe off the excess with a towel and you'll be fine."
                    100 "You're wet and smelly but at least you're not leaving a trail"
                    10 "Well it's not noticeable"
                    0 "It's clean")))
(defclass watertight-fursuit (fursuit) ()
  (:default-initargs
   :name "Watertight Fursuit"
   :value 25000
   :sogginess-capacity 5000
   :messiness-capacity 50000
   :waterproof t
   :description "So you're fursuiting, but then all of a sudden you gotta go, but then don't make it, and then end up in a smelly soggy fursuit, and then no one wants to touch you cause you're soggy and smelly. Well with this baby, all those nasty smells and waste stay inside your suit, usually. Now the only one who has to suffer is you. Isn't that great?"
   :wear-wet-text '(400 "Even though no one else can tell, you're drenched in your own bodily fluids from the waist down."
                    100 "You're wet and smelly underneath, but at least no one else will notice."
                    10 "Well it's not noticeable"
                    0 "It's clean")))
(defclass koopa-shell (closed-full-outfit) ()
  (:default-initargs
   :name "Koopa Shell"
   :value 10000
   :thickness-capacity 600
   :wear-stats (list :speed -1 :defence 20)
   :description "A hard koopa shell"))
(defclass cheerleader-outfit (yadfa:dress) ()
  (:default-initargs
   :name "Cheerleader Dress"
   :plural-name "Cheerleader Dresses"
   :value 150
   :description "A pretty cheerleader dress. Looks great with a diaper to show everyone off with."
   :bulge-text '(25 "The skirt does absolutely nothing to hide your padding"
                 #.(* 1/2 25) "The skirt does a good job hiding your padding, as long as you're standing still"
                 0 "It fits quite loosely")
   :thickness-capacity (* 16 (+ 25 2/5))))
(defclass ballerina-dress (yadfa:dress) ()
  (:default-initargs
   :name "Ballerina Dress"
   :plural-name "Ballerina Dresses"
   :value 150
   :description "A pretty ballerina dress. Looks great with a diaper to show everyone off with."
   :bulge-text '(25 "The skirt does absolutely nothing to hide your padding"
                 #.(* 1/2 25) "The skirt does a good job hiding your padding, as long as you're standing still"
                 0 "It fits quite loosely")))
(defclass braixen-dress (yadfa:dress) ()
  (:default-initargs
   :name "Braixen Dress"
   :plural-name "Ballerina Dresses"
   :value 150
   :description "A furry dress that looks like the fur of a Braixen"
   :bulge-text '(25 "The skirt does absolutely nothing to hide your padding"
                 #.(* 1/2 25) "The skirt does a good job hiding your padding, as long as you're standing still"
                 0 "It fits quite nicely")))
(defclass shendyt (skirt) ()
  (:default-initargs
   :name "Shendyt"
   :description "A skirt like loincloth similar to what the ancient Egyptians wear"))
(defclass kalasiris (yadfa:dress) ()
  (:default-initargs
   :name "Kalasiris"
   :plural-name "Kalasirises"
   :value 200
   :description "A dress similar to what the ancient Egyptians wear"
   :bulge-text '(480 "Your padding is completely visible"
                 430 "Your padding is slightly visible under your dress"
                 #.(* 1/2 25) "The dress does a good job hiding your padding"
                 0 "It fits quite loosely")
   :thickness-capacity nil))
(defonesie shortalls (onesie ab-clothing-mixin) ()
  (:default-initargs
   :name "Shortalls"
   :plural-name "Shortalls"
   :description "Denim shortalls with snaps on the front"))
