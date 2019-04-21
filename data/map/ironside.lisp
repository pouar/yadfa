(in-package :yadfa-zones)
(ensure-zone (0 0 0 ironside)
    :name "Ironside Street"
    :description "Your typical suburban street. Some furries are driving in cars, some are walking, and some are riding on top of other furries treating them like a horse."
    :enter-text "You enter the street. You see a sign that says `Keep our city and your pants clean. Always go potty in the toilet and not in your pants and don't leave puddles on the floor. Anyone who doesn't abide by this rule will be assumed to have no potty training whatsoever and will be immediately diapered by the diaper police to prevent further puddles.'"
    :no-puddles t
    :warp-points (list 'home '(0 1 0 home)))
(ensure-zone (1 0 0 ironside)
    :name "Ironside Street"
    :description "Your typical suburban street. Some furries are driving in cars, some are walking, and some are riding on top of other furries treating them like a horse."
    :no-puddles t
    :enter-text "You enter the street")
(ensure-zone (2 0 0 ironside)
    :name "Ironside Street"
    :description "Your typical suburban street. Some furries are driving in cars, some are walking, and some are riding on top of other furries treating them like a horse."
    :no-puddles t
    :warp-points (list 'bandits-way '(0 0 0 bandits-domain))
    :enter-text "You enter the street")
(ensure-zone (0 1 0 downtown)
    :name "Ironside Crap-Mart"
    :description "Your local Crap-Mart store. We don't have as big of selection as other stores, but we carry a lot more of what we do actually stock. Our products are built with love, and by love, I mean the sweat and tears of cheap child labor from China. We pass the savings down to you, in theory, but not in practice. We now stock incontinence products that you can depend on, unless you plan to use them, since they can't hold anything worth a crap. Why do people keep buying the ones from across the street?"
    :enter-text "You enter the Crap-Mart"
    :no-puddles t
    :props (list
               :shop (make-instance 'shop
                         :items-for-sale (list
                                             '(yadfa-items:monster-energy-drink)
                                             '(yadfa-items:generic-diapers-package)
                                             '(yadfa-items:generic-pullons-package)
                                             '(yadfa-items:dress)
                                             '(yadfa-items:jeans)
                                             '(yadfa-items:tshirt)
                                             '(yadfa-items:boxers)
                                             '(yadfa-items:panties)
                                             '(yadfa-items:knights-armor)
                                             '(yadfa-items:potion)))
               :changing-table (make-instance 'automatic-changing-table)))

(ensure-zone (0 -1 0 ironside)
    :name "Ironside ABDL-Mart"
    :description "Welcome to ABDL-Mart"
    :enter-text "You enter the ABDL-Mart."
    :no-puddles t
    :props (list
               :shop (make-instance 'shop
                         :items-for-sale (list
                                             '(yadfa-items:bottle-of-milk)
                                             '(yadfa-items:incontinence-pad-package)
                                             '(yadfa-items:diaper-package)
                                             '(yadfa-items:pullups-package)
                                             '(yadfa-items:toddler-dress)
                                             '(yadfa-items:onesie/opened)))
               :changing-table (make-instance 'automatic-changing-table)))
(ensure-zone (2 1 0 ironside)
    :name "Ironside University Entrance"
    :description "An old school university back when universities actually innovated, instead of being dumbed down, commercialized, and simply taught how to use proprietary products."
    :enter-text "You enter the university"
    :no-puddles t
    :diapers-only t)
(ensure-zone (2 2 0 ironside)
    :name "Ironside University"
    :description "A hallway"
    :enter-text "You're in the hallway"
    :no-puddles t
    :diapers-only t)
(ensure-zone (2 3 0 ironside)
    :name "Ironside University"
    :description "A hallway"
    :enter-text "You're in the hallway"
    :no-puddles t
    :diapers-only t)
(ensure-zone (3 2 0 ironside)
    :name "6.001/6.037"
    :description "This is the classroom for Structure and Interpretation of Computer Programs"
    :enter-text "You enter the classroom"
    :no-puddles t
    :diapers-only t
    :direction-attributes (list :south (list :hidden t))
    :props
    (list
        :desk
        (make-instance 'prop
            :name "Desk"
            :description "One of the desks in the classroom"
            :actions
            (list
                :sit-through-lecture
                (make-action
                    :documentation "Sit through a lecture"
                    :lambda '(lambda
                                 (prop &rest keys &key &allow-other-keys)
                                 (declare (ignore prop keys))
                                 (format t "Instructor: Today's topic is Linear Recursion and Iteration, let's look at the following factorial function~%~%")
                                 (write-line "*writes")
                                 (write-line "(define (factorial n)")
                                 (write-line "    (if (= n 1)")
                                 (write-line "        1")
                                 (write-line "        (* n (factorial (- n 1)))))")
                                 (format t "on the blackboard, then walks through the process in the following substitution model~%~%(factorial 6)~%")
                                 (write-line "(* 6 (factorial 5))")
                                 (write-line "(* 6 (* 5 (factorial 4)))")
                                 (write-line "(* 6 (* 5 (* 4 (factorial 3))))")
                                 (write-line "(* 6 (* 5 (* 4 (* 3 (factorial 2)))))")
                                 (write-line "(* 6 (* 5 (* 4 (* 3 (* 2 (factorial 1))))))")
                                 (write-line "(* 6 (* 5 (* 4 (* 3 (* 2 1)))))")
                                 (write-line "(* 6 (* 5 (* 4 (* 3 2))))")
                                 (write-line "(* 6 (* 5 (* 4 6)))")
                                 (write-line "(* 6 (* 5 24))")
                                 (write-line "(* 6 120)")
                                 (write-line "720")
                                 (format t "on the blackboard*~%~%")
                                 (format t "Instructor: This is a recursive process, but isn't very efficient though, as the interpreter needs to keep track of all the operations, instead we can redefine the function as follows~%~%")
                                 (write-line "*writes")
                                 (write-line "(define (factorial n)")
                                 (write-line "    (define (iter product counter)")
                                 (write-line "        (if (> counter n)")
                                 (write-line "            product")
                                 (write-line "            (iter (* counter product)")
                                 (write-line "                (+ counter 1))))")
                                 (write-line "    (iter 1 1))")
                                 (write-line "on the chalkboard, then walks through the process")
                                 (write-line "(factorial 6)")
                                 (write-line "(iter 1 1)")
                                 (write-line "(iter 1 2)")
                                 (write-line "(iter 2 3)")
                                 (write-line "(iter 6 4)")
                                 (write-line "(iter 24 5)")
                                 (write-line "(iter 120 6)")
                                 (write-line "(iter 720 7)")
                                 (write-line "720")
                                 (write-line "on the blackboard*")
                                 (format t "Instructor: This is an iterative process, as the interpreter now only needs to keep track of the variables product and counter for each n~%~%")
                                 (trigger-event 'yadfa-events:ironside-university-joke-1)))))))
(ensure-zone (3 3 0 ironside)
    :name "Ironside University Dormitory"
    :description ""
    :enter-text ""
    :no-puddles t
    :diapers-only t
    :direction-attributes (list :north (list :hidden t))
    :props (list
               :bed (make-instance 'bed
                        :name "Your bed"
                        :description "Pouar wasn't sure what design to put on the sheets, so he decided to leave that up to the player's interpretation.")
               :dresser (make-instance 'prop
                            :name "Dresser"
                            :placeable t
                            :description "Has all your clothes and diapers in here, until you take them out.")
               :checkpoint (make-instance 'checkpoint)
               :washer (make-instance 'washer
                           :name "Washer"
                           :description "A place to wash all the clothes that you've ruined")
               :diaper-dispenser
               (make-instance 'prop
                   :name "Diaper Dispenser"
                   :description "Provides diapers for the students here just in case they can't sit at their desks and hold it."
                   :actions
                   (list :get-diaper
                       (make-action
                           :documentation "Get a diaper from the dispenser"
                           :lambda '(lambda
                                        (prop &rest keys &key &allow-other-keys)
                                        (declare (type prop prop) (ignore prop))
                                        (check-type prop prop)
                                        (push (make-instance 'yadfa-items:diaper) (inventory-of (player-of *game*)))))))))
