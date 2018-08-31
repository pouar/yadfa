(in-package :yadfa/zones)
(defzone (0 0 0 downtown) ()
    (:default-initargs
        :name "Street"
        :description "Your typical suburban street. Some furries are driving in cars, some are walking, and some are riding on top of other furries treating them like a horse."
        :enter-text "You enter the street. You see a sign that says `Keep our city and your pants clean. Always go potty in the toilet and not in your pants and don't leave puddles on the floor. Anyone who doesn't abide by this rule will be assumed to have no potty training whatsoever and will be immediately diapered by the diaper police to prevent further puddles.'"
        :no-puddles t
        :warp-points '(home (0 1 0 home))))
(defzone (1 0 0 downtown) ()
    (:default-initargs
        :name "Street"
        :description "Your typical suburban street. Some furries are driving in cars, some are walking, and some are riding on top of other furries treating them like a horse."
        :no-puddles t
        :enter-text "You enter the street"))
(defzone (2 0 0 downtown) ()
    (:default-initargs
        :name "Street"
        :description "Your typical suburban street. Some furries are driving in cars, some are walking, and some are riding on top of other furries treating them like a horse."
        :no-puddles t
        :warp-points '(bandits-way (0 0 0 bandits-domain))
        :enter-text "You enter the street"))
(defzone (0 1 0 downtown) ()
    (:default-initargs
        :name "Crap-Mart"
        :description "Your local Crap-Mart store. We don't have as big of selection as other stores, but we carry a lot more of what we do actually stock. Our products are built with love, and by love, I mean the sweat and tears of cheap child labor from China. We pass the savings down to you, in theory, but not in practice. We now stock incontinence products that you can depend on, unless you plan to use them, since they can't hold anything worth a crap. Why do people keep buying the ones from across the street?"
        :enter-text "You enter the Crap-Mart"
        :no-puddles t
        :props (list
                   :shop (make-instance 'shop
                             :items-for-sale '((yadfa/items:monster-energy-drink)
                                                  (yadfa/items:generic-diapers-package)
                                                  (yadfa/items:generic-pullons-package)
                                                  (yadfa/items:dress)
                                                  (yadfa/items:jeans)
                                                  (yadfa/items:tshirt)
                                                  (yadfa/items:boxers)
                                                  (yadfa/items:panties)
                                                  (yadfa/items:knights-armor)
                                                  (yadfa/items:potion)))
                   :changing-table (make-instance 'automatic-changing-table))))

(defzone (0 -1 0 downtown) ()
    (:default-initargs
        :name "ABDL-Mart"
        :description "Welcome to ABDL-Mart"
        :enter-text "You enter the ABDL-Mart."
        :no-puddles t
        :props (list
                   :shop (make-instance 'shop
                             :items-for-sale '((yadfa/items:bottle-of-milk)
                                                  (yadfa/items:incontinence-pad-package)
                                                  (yadfa/items:diaper-package)
                                                  (yadfa/items:pullups-package)
                                                  (yadfa/items:toddler-dress)
                                                  (yadfa/items:onesie/opened)))
                   :changing-table (make-instance 'automatic-changing-table))))
(defzone (2 1 0 downtown) ()
    (:default-initargs
        :name "University Entrance"
        :description "An old school university back when universities actually innovated, instead of being dumbed down, commercialized, and simply taught how to use proprietary products. Since the children here have wet the bed at night, they're obviously not potty trained. So everyone is required to wear diapers so they don't make puddles. Since pants make this difficult to determine that everyone is compliant with this rule, no one is allowed to wear them."
        :enter-text "You enter the university"
        :no-puddles t
        :diapers-only t))
(defzone (2 2 0 downtown) ()
    (:default-initargs
        :name "University"
        :description ""
        :enter-text ""
        :no-puddles t
        :diapers-only t))
(defzone (3 2 0 downtown) ()
    (:default-initargs
        :name "6.001/6.037"
        :description "This is the classroom for Structure and Interpretation of Computer Programs"
        :enter-text "You enter the classroom"
        :no-puddles t
        :diapers-only t
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
                                     (declare (ignore prop))
                                     (format t "I~%structor: Today's topic is Li~%ear Recursio~% a~%d Iteratio~%, let's look at the followi~%g factorial fu~%ctio~%~%~%*writes~%(defi~%e (factorial ~%)~%    (if (= ~% 1)~%        1~%        (* ~% (factorial (- ~% 1)))))~%o~% the blackboard, the~% walks through the process i~% the followi~%g substitutio~% model~%~%(factorial 6)~%(* 6 (factorial 5))~%(* 6 (* 5 (factorial 4)))~%(* 6 (* 5 (* 4 (factorial 3))))~%(* 6 (* 5 (* 4 (* 3 (factorial 2)))))~%(* 6 (* 5 (* 4 (* 3 (* 2 (factorial 1))))))~%(* 6 (* 5 (* 4 (* 3 (* 2 1)))))~%(* 6 (* 5 (* 4 (* 3 2))))~%(* 6 (* 5 (* 4 6)))~%(* 6 (* 5 24))~%(* 6 120)~%720~%o~% the blackboard*~%~%I~%structor: This is a recursive process, but is~%'t very efficie~%t though, as the i~%terpreter ~%eeds to keep track of all the operatio~%s, i~%stead we ca~% redefi~%e the fu~%ctio~% as follows~%~%*writes~%(defi~%e (factorial ~%)~%    (defi~%e (iter product cou~%ter)~%        (if (> cou~%ter ~%)~%            product~%            (iter (* cou~%ter product)~%                (+ cou~%ter 1))))~%    (iter 1 1))~%o~% the chalkboard, the~% walks through the process~%(factorial 6)~%(iter 1 1)~%(iter 1 2)~%(iter 2 3)~%(iter 6 4)~%(iter 24 5)~%(iter 120 6)~%(iter 720 7)~%720~%o~% the blackboard*~%I~%structor: This is a~% iterative process, as the i~%terpreter ~%ow o~%ly ~%eeds to keep track of the variables product a~%d cou~%ter for each ~%"))))))))
