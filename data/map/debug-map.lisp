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
                                :placeable t
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
                   :shop (make-instance 'debug-shop))))
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
        :events '(yadfa/events:test-battle-1)
        :warp-points '(|1| (0 0 0 debug-map))))
(defzone (1 1 1 debug-map) ()
    (:default-initargs
        :name "zone-1-1-1-debug-map"
        :description "zone-1-1-1-debug-map"
        :enter-text "zone-1-1-1-debug-map"))
