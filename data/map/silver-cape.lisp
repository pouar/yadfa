(in-package :yadfa/zones)
(macro-level
    `(progn
         ,@(iter (for i from 0 to 20)
               (collect
                   `(ensure-zone (0 ,i 0 silver-cape)
                        :name "Silver Cape Street"
                        :description "A busy street with various furries moving back and forth"
                        :enter-text "You enter the street"
                        :warp-points ',(when (= i 0) '(bandits-domain (0 30 0 bandits-domain)))
                        :hidden ,(when (= i 0) t)
                        :events ',(when (= i 0) '(yadfa/events:enter-silver-cape-1)))))))
(macro-level
    `(progn
         ,@(iter (for i from -10 to 10)
               (unless (= i 0)
                   (collect
                       `(ensure-zone (,i 10 0 silver-cape)
                            :name "Silver Cape Street"
                            :description "A busy street with various furries moving back and forth"
                            :enter-text "You enter the street"))))))
(ensure-zone (1 5 0 silver-cape)
    :name "Silver Cape Pokemon Center"
    :description "A place to heal your pokemon"
    :enter-text "You enter the street"
    :props (list
               :magic-healing-machine
               (make-instance 'prop
                   :name "Magic Healing Machine"
                   :description "Heal your pokemon here"
                   :actions (list
                                :use (make-action
                                         :documentation "Heal your pokemon"
                                         :lambda '(lambda
                                                      (prop &rest keys &key &allow-other-keys)
                                                      (declare (type prop prop) (ignore prop))
                                                      (check-type prop prop)
                                                      (format t "~a~%" "https://youtu.be/wcg5n2UVMss?t=134")
                                                      (setf (health-of user) (calculate-stat user :health))
                                                      (setf (energy-of user) (calculate-stat user :energy))))))))
(ensure-zone (0 21 0 silver-cape)
    :name "Silver Cape Dock"
    :description "A Dock that heads to the ocean"
    :enter-text "You enter the street"
    :warp-points '(:your-ship (-1 6 0 yadfa/zones:your-ship)))
