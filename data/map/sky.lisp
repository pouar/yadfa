(in-package :yadfa-zones)
(ensure-zone (0 0 0 candle-carnival)
  :name "Candle Carnival Entrance"
  :description "Welcome to Candle Carnival. An awesome theme park in the sky"
  :enter-text "Welcome to Candle Carnival. An awesome theme park in the sky"
  :events '(yadfa-events:secret-underground-pipe-candle-carnival))
(ensure-zone (0 -1 0 candle-carnival)
  :name "Candle Carnival Pool"
  :description "The entrance to the pool"
  :enter-text "You're swimming in the pool"
  :underwater t)
(macro-level
  `(progn
     ,@(iter (for x from -10 to 10)
         (iter (for y from -2 downto -17)
           (collect `(ensure-zone (,x ,y 0 candle-carnival)
                       :name "Candle Carnival Pool"
                       :description "This pool makes up most of this floor"
                       :enter-text "You're swimming in the pool"
                       :underwater t))))))
(ensure-zone (0 -18 0 candle-carnival)
  :name "Elevator"
  :description "An elevator to the upper deck"
  :enter-text "You enter the elevator"
  :stairs (list :up)
  :direction-attributes (list :up (list :exit-text "Going up")))
(ensure-zone (0 -18 1 candle-carnival)
  :name "Elevator"
  :description "An elevator to the upper deck"
  :enter-text "You enter the elevator"
  :stairs (list :down)
  :direction-attributes (list :down (list :exit-text "Going down")))
(macro-level
  `(progn
     ,@(iter (for i from -17 to -2)
         (collect `(ensure-zone (0 i 1 candle-carnival)
                     :name "Catwalk"
                     :description "A catwalk over the pool"
                     :enter-text "You're swimming in the pool"
                     :warp-points '(dive (0 ,i 0 candle-carnival)))))))
(macro-level
  `(progn
     ,@(iter (for i from -10 to 10)
         (unless (= i -10)
           (collect `(ensure-zone (i -10 1 candle-carnival)
                       :name "Catwalk"
                       :description "A catwalk over the pool"
                       :enter-text "You're swimming in the pool"
                       :warp-points '(dive (i -10 0 candle-carnival))))))))
(ensure-zone (-11 -10 1 candle-carnival)
  :name "Water slide"
  :description "A water slide that lets you slide to the bottom"
  :enter-text "You look down the water slide"
  :warp-points '(slide-down (-10 -9 0 candle-carnival)))
(ensure-zone (11 -10 1 candle-carnival)
  :name "Power room"
  :description "Apparently this place is powered by monkeys in hamster wheels"
  :enter-text "You're inside the power room")
(ensure-zone (4 -1 1 candle-carnival)
  :name "Rocket Pad"
  :description "A rocket pad"
  :enter-text "You enter the rocket pad"
  :warp-points (list 'rocket '(0 0 0 sky-base-landing-pad))
  :direction-attributes (list 'rocket (list :exit-text "You fly over to Sky Base then drop off the rocket. The base's anti-gravity slingshot device emits a force that pulls you back up and lands you on a platform like an invisible bungee cord, but one that pulls you to different platforms instead of just one. It seems this is the primary mode of transportation here.")))
(ensure-zone (4 -18 0 candle-carnival)
  :name "Changing room"
  :description "Apparently this place is powered by monkeys in hamster wheels"
  :enter-text "You're inside the power room"
  :props (list
          :vending-machine (make-instance 'yadfa-props:shop
                                          :items-for-sale '((yadfa-items:disposable-swim-diaper . (list :value 10))
                                                            (yadfa-items:diaper . (list :value 10))))))
(ensure-zone (4 -18 0 candle-carnival)
  :name "Changing room"
  :description "A place where you can change your clothes for swimming. There's a vending machine for people to buy diapers from while they're here"
  :enter-text "You enter the changing room"
  :props (list
          :vending-machine (make-instance 'yadfa-props:vending-machine
                                          :items-for-sale '((yadfa-items:disposable-swim-diaper . (list :value 11))
                                                            (yadfa-items:diaper . (list :value 10))
                                                            (yadfa-items:pullups . (list :value 5))))))
(ensure-zone (-4 -18 0 candle-carnival)
  :name "Gift Shop"
  :description "Here you can buy stuff"
  :enter-text "You enter the shop"
  :props (list
          :shop (make-instance 'yadfa-props:shop
                               :items-for-sale '((yadfa-items:disposable-swim-diaper-package)
                                                 (yadfa-items:swim-diaper-cover)
                                                 (yadfa-items:blanket)
                                                 (yadfa-items:plushie)
                                                 (yadfa-items:pirate-dress)
                                                 (yadfa-items:pirate-shirt)
                                                 (yadfa-items:orca-suit-lite)))))
(uiop:define-package #:sky-base
  (:export
   #:main-office
   #:living-quarters
   #:shop
   #:landing-pad
   #:nursery
   #:mansion
   #:launch-pad)
  (:documentation "Contains symbols for the sky base"))
(macro-level
  (let ((syms '(sky-base:landing-pad
                sky-base:living-quarters
                sky-base:main-office
                sky-base:shop
                sky-base:nursery
                sky-base:mansion
                sky-base:launch-pad))
        (names '("Sky Base Landing Pad"
                 "Sky Base Living Quarters"
                 "Sky Base Main Office"
                 "Sky Base Shop"
                 "Sky Base Nursery"
                 "Sky Base Mansion"
                 "Sky Base Launch Pad")))
    `(progn
       ,@(iter
           (for sym in syms)
           (collect `(defparameter ,sym ',sym)))
       ,@(iter
           (for sym in syms)
           (for name in names)
           (for desc in '("The entrance to Sky Base"
                          "Where many residents of the sky base stay"
                          "The entrance to the main office"
                          "The entrance to Sky Base Shop"
                          "A place for ABDLs to hang out"
                          "Your own personal quarters"
                          "Use this to head to the next area"))
           (collect `(ensure-zone (0 0 0 ,sym)
                       :name ,name
                       :description ,desc
                       :enter-text ,(format nil "You step on the ~a entrance" name)
                       :direction-attributes ',(iter (for i in syms)
                                                 (for i-name in names)
                                                 (unless (eq i sym)
                                                   (collect i)
                                                   (collect `(:exit-text ,(format nil "You jump from the ~a to the ~a" name i-name)))))
                       :warp-points ',(iter (for i in syms)
                                        (unless (eq i sym)
                                          (collect i)
                                          (collect `(0 0 0 ,i))))
                       ,@(when (eq sym 'sky-base:landing-pad)
                           '(:events '(yadfa-events:secret-underground-pipe-sky-base)))))))))
(ensure-zone (0 -1 0 sky-base:launch-pad)
  :name "Rocket Pad"
  :description "A rocket pad"
  :enter-text "You enter the rocket pad"
  :warp-points (list 'rocket '(0 0 0 star-city))
  :direction-attributes (list 'rocket (list :exit-text "You fly over to Star City")))
(ensure-zone (0 0 0 star-city)
  :name "Star City"
  :description "A city orbiting the planet on a giant platform"
  :enter-text "You're on the part of the pathway that acts as the city's gangway"
  :events '(yadfa-events:secret-underground-pipe-star-city))
(macro-level
  `(progn
     ,@(iter
         (with a = ())
         (for y from 1 to 21)
         (alexandria:appendf
          a
          (iter (for x from -10 to 10)
            (when (or (= x 0) (= y 11))
              (collect `(ensure-zone (,x ,y 0 star-city)
                          :name "Star City"
                          :description "A city orbiting the planet on a giant platform"
                          :enter-text "You're wondering across the platform")))))
         (finally (return a)))))
(ensure-zone (-2 3 0 star-city)
  :name "Star City Hotel Lobby"
  :description "A luxurious hotel"
  :enter-text "you're in the hotel lobby"
  :stairs (list :up))
(macro-level
  `(progn
     ,@(iter (for x from -3 downto -7)
         (collect `(ensure-zone (,x 3 0 star-city)
                     :name "Star City Hotel Hall"
                     :description "A luxurious hotel"
                     :enter-text "you're in the hall")))))
(ensure-zone (-3 2 0 star-city)
  :name "Star City Hotel Diner"
  :description "A luxurious hotel"
  :enter-text "Welcome to the diner"
  :direction-attributes (list :east (list :hidden t)
                              :west (list :hidden t)))
(ensure-zone (-4 2 0 star-city)
  :name "Star City Hotel Shop"
  :description "A luxurious hotel"
  :enter-text "Welcome to the shop"
  :direction-attributes (list :east (list :hidden t)
                              :west (list :hidden t)))
(ensure-zone (-5 2 0 star-city)
  :name "Star City Hotel Spa"
  :description "A luxurious hotel"
  :enter-text "Welcome to the spa"
  :direction-attributes (list :east (list :hidden t)
                              :west (list :hidden t)))
(ensure-zone (-6 2 0 star-city)
  :name "Star City Hotel Gym"
  :description "A luxurious hotel"
  :enter-text "Welcome to the gym"
  :direction-attributes (list :east (list :hidden t)
                              :west (list :hidden t)))
(ensure-zone (-7 2 0 star-city)
  :name "Star City Hotel Pool"
  :description "Welcome to the pool"
  :enter-text "you're in the hall"
  :direction-attributes (list :east (list :hidden t)
                              :west (list :hidden t)))
(ensure-zone (-1 3 0 star-city)
  :name "Star City"
  :description "A city orbiting the planet on a giant platform"
  :enter-text "You're wondering across the platform")
(ensure-zone (-2 3 1 star-city)
  :name "Star City Hotel Hallway"
  :description "A luxurious hotel"
  :enter-text "you're in the hall"
  :stairs (list :up :down))
(macro-level
  `(progn
     ,@(iter (for x from -1 downto -5)
         (collect `(ensure-zone (,x 3 1 star-city)
                     :name "Star City Hotel Hall"
                     :description "A luxurious hotel"
                     :enter-text "you're in the hall")))))
(ensure-zone (0 22 0 star-city)
  :name "Star City"
  :description "A city orbiting the planet on a giant platform"
  :enter-text "You're wondering across the platform"
  :warp-points (list 'rainbow-slide '(6 11 0 silver-cape))
  :direction-attributes (list 'rainbow-slide (list :exit-text "You slide down the slide back to the planet.")))
