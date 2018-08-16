(in-package :yadfa/zones)
(defzone (0 0 0 debug-map) ()
    (:default-initargs
        :name "zone-0-0-0-debug-map"
        :description "zone-0-0-0-debug-map"
        :enter-text "zone-0-0-0-debug-map"
        :warp-points '(|1| (1 1 0 debug-map) |2| (0 -1 -1 debug-map))
        :props (list
                   :dresser (make-instance 'prop
                                :name "Dresser"
                                :description "A dresser"
                                :items (append
                                           (loop for i from 1 to 20 collect (make-instance 'yadfa/items:diaper))
                                           (loop for i from 1 to 20 collect (make-instance 'yadfa/items:pullups))
                                           (loop for i from 1 to 5 collect (make-instance 'yadfa/items:thick-latex-diaper))
                                           (list
                                               (make-instance 'yadfa/items:sundress)
                                               (make-instance 'yadfa/items:toddler-dress)
                                               '(make-instance 'yadfa/items:latex-onesie))))
                   :toilet (make-instance 'toilet)
                   :bed (make-instance 'bed)
                   :shop (make-instance 'shop
                             :items-for-sale '((yadfa/items:diaper)
                                                  (yadfa/items:generic-diapers)
                                                  (yadfa/items:generic-diapers-package)
                                                  (yadfa/items:incontinence-pad)
                                                  (yadfa/items:incontinence-pad-package)
                                                  (yadfa/items:cloth-incontinence-pad)
                                                  (yadfa/items:cloth-diaper)
                                                  (yadfa/items:diaper-package)
                                                  (yadfa/items:kurikia-thick-diaper)
                                                  (yadfa/items:kurikia-thick-cloth-diaper)
                                                  (yadfa/items:thick-latex-diaper)
                                                  (yadfa/items:hyper-thick-diaper)
                                                  (yadfa/items:hyper-thick-cloth-diaper)
                                                  (yadfa/items:hyper-thick-latex-diaper)
                                                  (yadfa/items:pullups)
                                                  (yadfa/items:cloth-pullups)
                                                  (yadfa/items:swim-diaper-cover)
                                                  (yadfa/items:onesie/opened)
                                                  (yadfa/items:dress)
                                                  (yadfa/items:sundress)
                                                  (yadfa/items:toddler-dress)
                                                  (yadfa/items:tshirt)
                                                  (yadfa/items:jeans)
                                                  (yadfa/items:baggy-jeans)
                                                  (yadfa/items:latex-onesie/opened)
                                                  (yadfa/items:stretchable-latex-onesie/opened)
                                                  (yadfa/items:orca-suit)
                                                  (yadfa/items:stretchable-orca-suit)
                                                  (yadfa/items:boxers)
                                                  (yadfa/items:panties))))))
(setf-init-hook/zone (0 0 0 debug-map) :main-1
    (setf
        (getf (actions-of (getf (props-of zone) :shop)) :talk)
        (make-action
            :documentation "Say hi"
            :lambda '(lambda
                         (prop &rest keys &key &allow-other-keys)
                         (declare (ignore prop))
                         (format t "Hello World~%")))))
(defzone (0 0 -1 debug-map) ()
    (:default-initargs
        :name "zone-0-0--1-debug-map"
        :description "zone-0-0--1-debug-map"
        :enter-text "zone-0-0--1-debug-map"
        :underwater t))
(defzone (0 -1 -1 debug-map) ()
    (:default-initargs
        :name "zone-0--1--1-debug-map"
        :description "zone-0--1--1-debug-map"
        :enter-text "zone-0--1--1-debug-map"
        :hidden t))
(defzone (0 1 0 debug-map) ()
    (:default-initargs
        :name "zone-0-1-0-debug-map"
        :description "zone-0-1-0-debug-map"
        :enter-text "zone-0-1-0-debug-map"
        :enemy-spawn-list (list
                              (list :max-random 1 :enemies '((enemy))))))
(defzone (1 0 0 debug-map) ()
    (:default-initargs
        :name "zone-1-0-0-debug-map"
        :description "zone-1-0-0-debug-map"
        :enter-text "zone-1-0-0-debug-map"))
(defzone (1 1 0 debug-map) ()
    (:default-initargs
        :name "zone-1-1-0-debug-map"
        :description "zone-1-1-0-debug-map"
        :enter-text "zone-1-1-0-debug-map"
        :events (list (gethash 'yadfa/events:test-battle-1 (events-of-game)))
        :warp-points '(|1| (0 0 0 debug-map))))
(defzone (1 1 1 debug-map) ()
    (:default-initargs
        :name "zone-1-1-1-debug-map"
        :description "zone-1-1-1-debug-map"
        :enter-text "zone-1-1-1-debug-map"))
