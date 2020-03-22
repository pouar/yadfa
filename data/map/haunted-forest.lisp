;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-zones"; coding: utf-8-unix; -*-
(in-package :yadfa-zones)
(ensure-zone (0 0 0 haunted-forest)
  :name "Haunted Forest Entrance"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You enter the haunted forest"
  :events '(yadfa-events:secret-underground-pipe-haunted-forest)
  :warp-points (list 'rpgmaker-dungeon '(5 0 0 rpgmaker-dungeon)))
(ensure-zone (0 -1 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest)
(ensure-zone (1 -1 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest)
(ensure-zone (-1 -1 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest
  :direction-attributes (list :west (list :hidden t)))
(ensure-zone (-2 -1 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest
  :direction-attributes (list :east (list :hidden t)))
(ensure-zone (-2 0 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest)
(ensure-zone (-1 -2 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest
  :direction-attributes (list :west (list :hidden t)
                              :north (list :hidden t)))
(ensure-zone (-2 -2 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest
  :direction-attributes (list :east (list :hidden t)))
(ensure-zone (-2 -3 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest)
(ensure-zone (-1 -3 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest
  :direction-attributes (list :south (list :hidden t)))
(ensure-zone (0 -3 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest)
(ensure-zone (1 -3 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest)
(ensure-zone (1 -2 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest)
(ensure-zone (2 -2 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest
  :direction-attributes (list :south (list :hidden t)))
(ensure-zone (1 -1 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest)
(ensure-zone (2 -1 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest
  :direction-attributes (list :north (list :hidden t)))
(ensure-zone (2 0 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest)
(ensure-zone (3 0 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest)
(ensure-zone (3 -2 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest)
(ensure-zone (3 -3 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest)
(ensure-zone (3 -4 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest)
(ensure-zone (3 -5 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest)
(ensure-zone (5 -2 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest
  :direction-attributes (list :east (list :hidden t)))
(ensure-zone (6 -3 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest
  :direction-attributes (list :east (list :hidden t)))
(ensure-zone (6 -2 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're in front of a haunted house"
  :enemy-spawn-list 'haunted-forest
  :direction-attributes (list :west (list :hidden t)
                              :south (list :hidden t)
                              :east (list :hidden t)
                              'haunted-house (list :exit-text "You enter the haunted house"))
  :events '(yadfa-events:secret-underground-pipe-haunted-house)
  :warp-points (list 'haunted-house '(0 0 0 haunted-house)))
(ensure-zone (5 -3 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest)
(ensure-zone (5 -1 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest)
(ensure-zone (6 -1 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest
  :direction-attributes (list :north (list :hidden t)))
(ensure-zone (7 -1 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest)
(ensure-zone (7 -2 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest
  :direction-attributes (list :west (list :hidden t)))
(ensure-zone (7 -3 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest
  :direction-attributes (list :west (list :hidden t)))
(ensure-zone (7 -4 0 haunted-forest)
  :name "Haunted Forest"
  :description "You're in a strange forest. Spooky sounds and scary eyes all around."
  :enter-text "You're wondering around the haunted forest"
  :enemy-spawn-list 'haunted-forest)
(macro-level `(progn ,@(iter (for i from 3 to 7)
                         (collect `(ensure-zone (,i -5 0 haunted-forest)
                                     :name "Haunted Forest"
                                     :description "You're in a strange forest. Spooky sounds and scary eyes all around."
                                     :enter-text "You're wondering around the haunted forest"
                                     :enemy-spawn-list 'haunted-forest)))))
