;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-zones"; coding: utf-8-unix; -*-
(in-package :yadfa-zones)
(macro-level
  `(progn
     ,@(iter (for y from 0 to 2)
         (iter (for x from (- y) to y)
           (collect `(ensure-zone (,x ,y 0 your-ship)
                       :name "Emacs"
                       :description "The bow of your ship"
                       :direction-attributes (list :south (list
                                                           :hidden ,(and
                                                                     (or (= x 1) (= x -1))
                                                                     (= y 2)))
                                                   :down (list :hidden t)
                                                   :up (list :hidden t))))))))
(macro-level
  `(progn
     ,@(iter (for i from -2 to 2)
         (collect `(ensure-zone (,i 11 0 your-ship)
                     :name "Emacs"
                     :description "The stern of your ship"
                     :direction-attributes (list :north (list :hidden ,(or (= i 1) (= i -1)))
                                                 :down (list :hidden t)
                                                 :up (list :hidden t)))))))
(macro-level
  `(progn
     ,@(iter (for i from 3 to 10)
         (collect `(ensure-zone (-2 ,i 0 your-ship)
                     :name "Emacs"
                     :description "The port of your ship"
                     :direction-attributes (list :east (list :hidden ,(not (= i 6)))
                                                 :down (list :hidden t)
                                                 :up (list :hidden t))))
         (collect `(ensure-zone (2 ,i 0 your-ship)
                     :name "Emacs"
                     :description "The starboard of your ship"
                     :direction-attributes (list :west (list :hidden ,(not (= i 6)))
                                                 :down (list :hidden t)
                                                 :up (list :hidden t))))
         (collect `(ensure-zone (0 ,i 0 your-ship)
                     :name "Passage Way"
                     :description "The passage way of your ship"
                     :direction-attributes ,(if
                                             (= i 3)
                                             ()
                                             '(list :down (list :hidden t)
                                               :up (list :hidden t))))))))
(ensure-zone (0 3 1 your-ship)
  :name "Bridge"
  :description "You can steer your ship from here"
  :props (list
          :controls (make-instance 'prop
                                   :name "Controls"
                                   :description "The ships controls"
                                   :attributes (list :destinations (list '(0 21 0 silver-cape) '(1 21 0 bandits-domain)))
                                   :actions
                                   (list
                                    :list-places-to-sail (make-action
                                                          :documentation "List valid destinations"
                                                          :lambda
                                                          '(lambda
                                                            (prop &rest keys &key &allow-other-keys)
                                                            (format t "~4a~30a~30a" "Index" "Name of destination" "Coordinates")
                                                            (iter (for i from 0 to (1- (list-length (getf (attributes-of prop) :destinations))))
                                                              (format t "~4d~30a~30s"
                                                                      i
                                                                      (name-of (get-zone (nth i (getf (attributes-of prop) :destinations))))
                                                                      (nth i (getf (attributes-of prop) :destinations))))))
                                    :describe-place (make-action
                                                     :documentation "display the description of a destination. INDEX is an index from :list-places-to-sail"
                                                     :lambda
                                                     '(lambda
                                                       (prop &rest keys &key index &allow-other-keys)
                                                       (if (nth index (getf (attributes-of prop) :destinations))
                                                           (progn
                                                             (format t "Name: ~a~%~%Description: ~a~%~%Coordinates: ~s~%~%"
                                                                     (name-of (nth index (getf (attributes-of prop) :destinations)))
                                                                     (description-of (nth index (getf (attributes-of prop) :destinations)))
                                                                     (nth index (getf (attributes-of prop) :destinations))))
                                                           (format t "That's not a valid destination~%"))))
                                    :set-sail (make-action
                                               :documentation "Set sail to a place. INDEX is an index from :list-places-to-sail"
                                               :lambda '(lambda
                                                         (prop &rest keys &key index &allow-other-keys)
                                                         (if (nth index (getf (attributes-of prop) :destinations))
                                                             (progn (remf (warp-points-of (get-zone (getf (warp-points-of (get-zone '(-1 6 0 your-ship)))
                                                                                                          :exit)))
                                                                          :your-ship)
                                                                    (setf (getf (warp-points-of (get-zone '(-1 6 0 your-ship))) :exit)
                                                                          (nth index (getf (attributes-of prop) :destinations))
                                                                          (getf (warp-points-of (get-zone (nth index (getf (attributes-of prop) :destinations))))
                                                                                :your-ship)
                                                                          '(-1 6 0 your-ship)))
                                                             (format t "That's not a valid destination~%"))))))))
(ensure-zone (0 3 -1 your-ship)
  :name "Hold"
  :description "You have a chest here that can hold your crap"
  :props (list
          :chest (make-instance 'prop
                                :name "Chest"
                                :description "Place all your crap here"
                                :placeable t)))
(macro-level
  `(progn
     ,@(iter (for i from -1 to 1)
         (unless (= i 0)
           (collect `(ensure-zone (,i 6 0 your-ship)
                       :name "Passage Way"
                       :description "The passage way of your ship"
                       :direction-attributes (list :north (list :hidden t)
                                                   :south (list :hidden t)
                                                   :down (list :hidden t)
                                                   :up (list :hidden t))
                       :warp-points ,(if (= i -1)
                                         '(list :exit '(0 21 0 silver-cape))
                                         ())))))))
(ensure-zone (-1 5 0 your-ship)
  :name "Cabin"
  :description "A Cabin of your ship"
  :direction-attributes (list :north (list :hidden t)
                              :south (list :hidden t)
                              :west (list :hidden t)
                              :down (list :hidden t)
                              :up (list :hidden t)))
(ensure-zone (1 5 0 your-ship)
  :name "Cabin"
  :description "A Cabin of your ship"
  :direction-attributes (list :north (list :hidden t)
                              :south (list :hidden t)
                              :east (list :hidden t)
                              :down (list :hidden t)
                              :up (list :hidden t)))
(ensure-zone (-1 6 0 your-ship)
  :name "Cabin"
  :description "A Cabin of your ship"
  :direction-attributes (list :north (list :hidden t)
                              :south (list :hidden t)
                              :west (list :hidden t)
                              :down (list :hidden t)
                              :up (list :hidden t)))
(ensure-zone (1 6 0 your-ship)
  :name "Galley"
  :description "A Cabin of your ship"
  :direction-attributes (list :north (list :hidden t)
                              :south (list :hidden t)
                              :east (list :hidden t)
                              :down (list :hidden t)
                              :up (list :hidden t)))
(ensure-zone (-1 7 0 your-ship)
  :name "Cabin"
  :description "A Cabin of your ship"
  :direction-attributes (list :north (list :hidden t)
                              :south (list :hidden t)
                              :west (list :hidden t)
                              :down (list :hidden t)
                              :up (list :hidden t)))
(ensure-zone (1 7 0 your-ship)
  :name "Cabin"
  :description "A Cabin of your ship"
  :direction-attributes (list :north (list :hidden t)
                              :south (list :hidden t)
                              :east (list :hidden t)
                              :down (list :hidden t)))
(ensure-zone (-1 8 0 your-ship)
  :name "Cabin"
  :description "A Cabin of your ship"
  :direction-attributes (list :north (list :hidden t)
                              :south (list :hidden t)
                              :west (list :hidden t)
                              :down (list :hidden t)
                              :up (list :hidden t)))
(ensure-zone (1 8 0 your-ship)
  :name "Cabin"
  :description "A Cabin of your ship"
  :direction-attributes (list :north (list :hidden t)
                              :south (list :hidden t)
                              :east (list :hidden t)
                              :down (list :hidden t)))
(ensure-zone (-1 9 0 your-ship)
  :name "Cabin"
  :description "A Cabin of your ship"
  :direction-attributes (list :north (list :hidden t)
                              :south (list :hidden t)
                              :west (list :hidden t)
                              :down (list :hidden t)))
(ensure-zone (1 9 0 your-ship)
  :name "Cabin"
  :description "A Cabin of your ship"
  :direction-attributes (list :north (list :hidden t)
                              :south (list :hidden t)
                              :east (list :hidden t)
                              :down (list :hidden t)))
(ensure-zone (-1 10 0 your-ship)
  :name "Cabin"
  :description "A Cabin of your ship"
  :direction-attributes (list :north (list :hidden t)
                              :south (list :hidden t)
                              :west (list :hidden t)
                              :down (list :hidden t)))
(ensure-zone (1 10 0 your-ship)
  :name "Cabin"
  :description "A Cabin of your ship"
  :direction-attributes (list :north (list :hidden t)
                              :south (list :hidden t)
                              :east (list :hidden t)
                              :down (list :hidden t)
                              :up (list :hidden t)))
