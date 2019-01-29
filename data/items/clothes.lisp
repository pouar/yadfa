(in-package :yadfa/items)
(defonesie onesie ()
    (:default-initargs
        :name "Onesie"
        :description "A onesie"
        :value 400
        :onesie-bulge-text (cons
                               (list
                                   60
                                   "It fits over your diaper so tightly it looks like the buttons are about to go flying off"
                                   20
                                   "It fits over your diaper quite nicely"
                                   0
                                   "It's so baggy that what you're wearing under there is quite visible")
                               (list
                                   80
                                   "You are unable to get the buttons to snap so you just leave the flaps open"
                                   (* 12 25)
                                   "Your padding is clearly visible under there"
                                   (* 11 25)
                                   "The flaps just barely cover the diapers"
                                   (* 1/2 25)
                                   "The flaps hang over covering your padding like a dress"
                                   0
                                   "The flaps hang over covering your underwear like a dress"))))
(defclass short-dress (yadfa:dress) ()
    (:default-initargs
        :name "Short Dress"
        :plural-name "Short Dresses"
        :value 150
        :description "A short breezy dress."
        :bulge-text (list
                        225
                        "Your padding is clearly visible under your dress"
                        200
                        "Your padding is slightly visible under your dress"
                        175
                        "The dress does a good job hiding your padding, as long as you're standing still"
                        162.5
                        "The dress does a good job hiding your padding, unless a gust of wind happens to blow by"
                        (* 1/2 25)
                        "The dress does a good job hiding your padding"
                        0
                        "It fits quite loosely")
        :thickness-capacity (* 16 25.4)))
(defclass dress (yadfa:dress) ()
    (:default-initargs
        :name "Dress"
        :plural-name "Dresses"
        :value 150
        :description "A cute dress."
        :bulge-text (list
                        (* 4 25)
                        "The bottom of your dress has poofed out humorously"
                        (* 3 25)
                        "There is a slight bulge, but it's not too noticeable"
                        (* 1/2 25)
                        "The dress does a good job hiding your padding"
                        0
                        "It fits snuggly")
        :thickness-capacity (* 16 25.4)))
(defclass sundress (yadfa:dress) ()
    (:default-initargs
        :name "Sundress"
        :plural-name "Sundresses"
        :value 200
        :description "A loose fitting dress."
        :bulge-text (list
                        480
                        "Your padding is completely visible"
                        430
                        "Your padding is slightly visible under your dress"
                        (* 1/2 25)
                        "The dress does a good job hiding your padding"
                        0
                        "It fits quite loosely")
        :thickness-capacity t))
(defclass toddler-dress (yadfa:dress) ()
    (:default-initargs
        :name "Toddler's Dress"
        :plural-name "Toddler Dresses"
        :value 600
        :description "A frilly pink dress fit for a big toddler."
        :bulge-text (list
                        75
                        "Your padding is clearly visible under your dress"
                        50
                        "Your padding is slightly visible under your dress"
                        25
                        "The dress does a good job hiding your padding, as long as you're standing still"
                        12
                        "The dress does a good job hiding your padding, unless a gust of wind happens to blow by"
                        0
                        "The dress easily covers your underwear")
        :thickness-capacity t))
(defclass tshirt (shirt) ()
    (:default-initargs
        :name "T-Shirt"
        :value 50
        :description "A simple plain t-shirt."))
(defclass jeans (pants) ()
    (:default-initargs
        :name "Jeans"
        :plural-name "Jeans"
        :value 100
        :description "A simple pair of jeans."
        :bulge-text (list
                        12
                        "Your padding keeps poking out of the top of your pants"
                        0
                        "It fits snuggly")))
(defclass baggy-jeans (pants) ()
    (:default-initargs
        :name "Baggy Jeans"
        :plural-name "Baggy Jeans"
        :value 150
        :description "For when you need to hide that diaper of yours, sorta"
        :bulge-text (list
                        50
                        "Your pants puff out humorously"
                        12
                        "Your padding keeps poking out of the top of your pants"
                        0
                        "It fits loosely")
        :thickness-capacity (* 16 25.4)))
(defonesie latex-onesie ()
    (:default-initargs
        :onesie-waterproof t
        :value 600
        :name "Black Latex Onesie"
        :onesie-bulge-text (cons
                               (list
                                   60
                                   "It fits over your diaper so tightly it looks like the buttons are about to go flying off"
                                   20
                                   "It fits over your diaper quite nicely"
                                   0
                                   "It's so baggy that what you're wearing under there is quite visible")
                               (list
                                   80
                                   "You are unable to get the buttons to snap so you just leave the flaps open"
                                   (* 12 25)
                                   "Your padding is clearly visible under there"
                                   (* 11 25)
                                   "The flaps just barely cover the diapers"
                                   (* 1/2 25)
                                   "The flaps hang over covering your padding like a dress"
                                   0
                                   "The flaps hang over covering your underwear like a dress"))
        :description "An awesome black latex onesie"))
(defonesie stretchable-latex-onesie ()
    (:default-initargs
        :onesie-waterproof t
        :value 600
        :onesie-thickness-capacity (cons t t)
        :onesie-bulge-text (cons
                               (list
                                   60
                                   "The onesie has easily stretched to accommodate your padding"
                                   20
                                   "The diaper bulge makes it clear what you're wearing under there"
                                   0
                                   "It fits snuggly")
                               (list
                                   (* 12 25)
                                   "Your padding is clearly visible under there"
                                   (* 11 25)
                                   "The flaps just barely cover the diapers"
                                   (* 1/2 25)
                                   "The flaps hang over covering your padding like a dress"
                                   0
                                   "The flaps hang over covering your underwear like a dress"))
        :name "Black Latex Onesie"
        :description "An awesome black latex onesie that stretches to fit your humongous diapers"))
(defclass orca-suit (closed-full-outfit) ()
    (:default-initargs
        :waterproof t
        :value 1000
        :thickness-capacity (* 16 25.4)
        :bulge-text (list
                        60
                        "You look like one of those pictures drawn by Kurikia"
                        20
                        "The diaper bulge makes it clear what you're wearing under there"
                        0
                        "It fits snuggly")
        :name "Orca Suit"
        :description "An orca suit similar to the one Gabby wears."))
(defclass stretchable-orca-suit (closed-full-outfit) ()
    (:default-initargs
        :waterproof t
        :value 1000
        :thickness-capacity t
        :bulge-text (list
                        60
                        "You look like one of those pictures drawn by Kurikia"
                        20
                        "The diaper bulge makes it clear what you're wearing under there"
                        0
                        "It fits snuggly")
        :name "Stretchable Orca Suit"
        :description "A variant of the Orca Suit that stretches to fit your humongous diapers"))
(defclass orca-suit-lite (closed-full-outfit) ()
    (:default-initargs
        :waterproof t
        :value 1000
        :thickness-capacity (* 16 25.4)
        :bulge-text (list
                        60
                        "You look like one of those pictures drawn by Kurikia"
                        20
                        "The diaper bulge makes it clear what you're wearing under there"
                        0
                        "It fits snuggly")
        :name "Orca Suit Lite"
        :description "An orca suit similar to the one Gabby wears, minus the swim boots and arm covers. You don't need 'em"))
(defclass stretchable-orca-suit-lite (closed-full-outfit) ()
    (:default-initargs
        :waterproof t
        :value 1000
        :thickness-capacity t
        :bulge-text (list
                        60
                        "You look like one of those pictures drawn by Kurikia"
                        20
                        "The diaper bulge makes it clear what you're wearing under there"
                        0
                        "It fits snuggly")
        :name "Stretchable Orca Suit Lite"
        :description "A variant of the Orca Suit Lite that stretches to fit your humongous diapers"))
(defclass boxers (undies) ()
    (:default-initargs
        :name "Boxers"
        :plural-name "Boxers"
        :description "You sure wearing these is a good idea piddler?"
        :value 100))
(defclass panties (undies) ()
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
(defclass tunic (yadfa:dress) ()
    (:default-initargs
        :name "Tunic"
        :value 100
        :description "For when a toddler's dress is too sissy."
        :bulge-text (list
                        75
                        "Your padding is clearly visible under your tunic"
                        50
                        "Your padding is slightly visible under your tunic"
                        25
                        "The tunic does a good job hiding your padding, as long as you're standing still"
                        12
                        "The tunic does a good job hiding your padding, unless a gust of wind happens to blow by"
                        0
                        "The tunic easily covers your underwear")
        :thickness-capacity t))

(defclass bandit-uniform-tunic (yadfa:dress) ()
    (:default-initargs
        :name "Bandit Uniform Tunic"
        :value 200
        :description "This tunic has the Diapered Raccoon Bandits' insignia on it. The Diapered Raccoon Bandits usually don't wear pants since it easier to change their diapers without them. The tunics are for the higher ups as it allows them to conceal the state of their diaper. A privilege the lower ranks don't have."
        :bulge-text (list
                        75
                        "Your padding is clearly visible under your tunic"
                        50
                        "Your padding is slightly visible under your tunic"
                        12
                        "The tunic easily covers your padding"
                        0
                        "The tunic easily covers your underwear")
        :thickness-capacity t))

(defclass bandit-uniform-shirt (shirt) ()
    (:default-initargs
        :name "Bandit Uniform Shirt"
        :value 100
        :description "This shirt has the Diapered Raccoon Bandits' insignia on it. It's for the lower ranks as they're not allowed to conceal the state of their diaper, unlike the higher ranks."))
(defclass bandit-uniform-sports-bikini-top (shirt) ()
    (:default-initargs
        :name "Bandit Uniform Sports Bikini Top"
        :value 50
        :description "A sports bikini top that has the Raccoon Bandits' insignia on it."))
(defonesie bandit-swimsuit ()
    (:default-initargs
        :onesie-waterproof t
        :value 600
        :name "Bandit Swimsuit"
        :onesie-bulge-text (cons
                               (list
                                   60
                                   "It fits over your diaper so tightly it looks like the buttons are about to go flying off"
                                   20
                                   "It fits over your diaper quite nicely"
                                   0
                                   "It's so baggy that what you're wearing under there is quite visible")
                               (list
                                   80
                                   "You are unable to get the buttons to snap so you just leave the flaps open"
                                   (* 12 25)
                                   "Your padding is clearly visible under there"
                                   (* 11 25)
                                   "The flaps just barely cover the diapers"
                                   (* 1/2 25)
                                   "The flaps hang over covering your padding like a dress"
                                   0
                                   "The flaps hang over covering your underwear like a dress"))
        :description "A one piece swimsuit for the higher ups to wear when in the water. It resembles a woman's one piece swimsuit, but has snaps on the bottom to make diaper changes easier. It has the bandits' insignia on the front. This model has a skirt in order to hide the poofiness of their diaper."))
(defclass navy-skirt (yadfa:skirt) ()
    (:default-initargs
        :name "Navy Skirt"
        :plural-name "Navy Skirts"
        :value 200
        :description "A loose fitting skirt part of the Navy uniform for girls."
        :bulge-text (list
                        480
                        "Your padding is completely visible"
                        430
                        "Your padding is slightly visible under your dress"
                        (* 1/2 25)
                        "The dress does a good job hiding your padding"
                        0
                        "It fits quite loosely")
        :thickness-capacity t))
(defclass navy-pants (yadfa:pants) ()
    (:default-initargs
        :name "Navy Pants"
        :plural-name "Navy Pants"
        :value 200
        :description "The bottom half of a Navy uniform."
        :bulge-text (list
                        12
                        "Your padding keeps poking out of the top of your pants"
                        0
                        "It fits snuggly")))
(defclass navy-shirt (yadfa:shirt) ()
    (:default-initargs
        :name "Navy Shirt"
        :value 200
        :description "The top half of a Navy uniform"))
