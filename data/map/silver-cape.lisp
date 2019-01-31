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
(ensure-zone (-1 6 0 silver-cape)
    :name "Silver Cape Navy HQ Entrance"
    :description "The entrance to Navy HQ."
    :enter-text "You're inside Navy HQ. The navy here seems to mostly consist of various aquatic creatures. They're mostly potty trained but still wear pullups just in case they don't make it in time, or if they don't want to hold it any longer. Due to pullups having a lower capacity than diapers, some of them suppliment pullups with stuffers.")
(ensure-zone (-2 6 -1 silver-cape)
    :name "Silver Cape Jail"
    :description "The jail beneith Navy HQ"
    :enter-text "You're inside Navy HQ"
    :events '(yadfa/events:get-location-to-pirate-cove-1))
(ensure-zone (-2 6 0 silver-cape)
    :name "Silver Cape Navy HQ Lobby"
    :description "The lobby of Navy HQ"
    :enter-text "You're inside Navy HQ"
    :props (list
               :receptionist
               (make-instance 'prop
                   :name "Dolphin Receptionist"
                   :description "The dolphin is hopping around while holding the front of her pullups"
                   :actions
                   (list
                       :talk
                       (make-action
                           :documentation "Talk to the Receptionist"
                           :lambda
                           '(lambda
                                (prop &rest keys &key &allow-other-keys)
                                (declare (type prop prop) (ignore prop))
                                (check-type prop prop)
                                (write-line "Dolphin: Welcome to navy HQ")
                                (format t "~a: Why don't you go to the bathroom? Or use your pullups?~%" (name-of (player-of *game*)))
                                (write-line "Dolphin: I'm not allowed to go to the bathroom during my shift and if I have another accident they'll put me back in diapers.")
                                (format t "~a: Ok~%" (name-of (player-of *game*)))
                                (setf (getf-action-from-prop
                                          (position-of (player-of *game*))
                                          :receptionist
                                          :tickle)
                                    (make-action
                                        :documentation "Tickle the dolphin"
                                        :lambda
                                        '(lambda
                                             (prop &rest keys &key &allow-other-keys)
                                             (declare (type prop prop) (ignore prop))
                                             (write-line "Dolphin: ACK!! NO!! PLEASE!!! DON'T!!!")
                                             (write-line "*The dolphin giggles and thrashes about until the wetness indicator on the front changes color*")
                                             (write-line "*One of the Navy orcas take notice and crinkles over*")
                                             (write-line "Orca: Looks, like the baby dolphin still hasn't learned to keep her pullups dry")
                                             (write-line "Dolphin: I'm not a baby!!!")
                                             (write-line "Orca: Says the baby in soggy pullups. Since you can't keep them dry, we're putting you back in diapers.")
                                             (write-line "*The Orca sets the dolphin on the desk*")
                                             (write-line "Dolphin: Please don't change me here!!! Everyone can see me!!!!!")
                                             (write-line "*The Orca ignores her pleas and changes her soggy pullups and puts her in a thick diaper then puts her back on the floor. The diaper is so thick that her legs are forced apart. The dolphin hides her face in embarrassment*")
                                             (setf (actions-of
                                                       (getf (get-props-from-zone (position-of (player-of *game*)))
                                                           :receptionist))
                                                 (list
                                                     :talk
                                                     (make-action
                                                         :documentation "Talk to the receptionist"
                                                         :lambda
                                                         '(lambda
                                                              (prop &rest keys &key &allow-other-keys)
                                                              (declare (type prop prop) (ignore prop))
                                                              (write-line "Dolphin: Welcome to navy HQ")))))
                                             (setf (description-of
                                                       (getf (get-props-from-zone (position-of (player-of *game*)))
                                                           :receptionist))
                                                 "A dolphin in a diaper so thick it spreads her legs apart forcing her to waddle"))))
                                (setf (getf-action-from-prop
                                          (position-of (player-of *game*))
                                          :receptionist
                                          :give-pad)
                                    (make-action
                                        :documentation "Give the dolphin a stuffer so she can go without ruining her pullups"
                                        :lambda
                                        '(lambda
                                             (prop &rest keys &key &allow-other-keys)
                                             (declare (type prop prop) (ignore prop))
                                             (block nil
                                                 (let
                                                     ((a
                                                          (iter (for i in (inventory-of (player-of *game*)))
                                                              (when (and
                                                                        (typep i
                                                                            'yadfa:incontinence-pad)
                                                                        (<= (sogginess-of i)
                                                                            0))
                                                                  (collect i)))))
                                                     (unless
                                                         a
                                                         (write-line "You don't have a clean stuffer to give her")
                                                         (return))
                                                     (write-line "*You hand the dolphin a stuffer*")
                                                     (format t "~a: Here, you might want this")
                                                     (write-line "Dolphin: But... but...")
                                                     (write-line "*Unable hold it much longer, she reluctantly takes the stuffer, pulls her pullups down halfway to her knees, puts the stuffer in, then pulls them back up and floods herself*")
                                                     (write-line "Dolphin: Well, at least my pullups are dry")
                                                     (removef (inventory-of (player-of *game*)) (car a))
                                                     (setf (actions-of
                                                               (getf (get-props-from-zone
                                                                         (position-of (player-of *game*)))
                                                                   :receptionist))
                                                         (list
                                                             :talk
                                                             (make-action
                                                                 :documentation "Talk to the receptionist"
                                                                 :lambda
                                                                 '(lambda
                                                                      (prop &rest keys &key &allow-other-keys)
                                                                      (declare (type prop prop) (ignore prop))
                                                                      (write-line "Dolphin: Welcome to navy HQ")))))
                                                     (setf (description-of
                                                               (getf (get-props-from-zone
                                                                         (position-of (player-of *game*)))
                                                                   :receptionist))
                                                         "A dolphin wearing pullups"))))))))))))
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
