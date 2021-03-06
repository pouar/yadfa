;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-zones"; coding: utf-8-unix; -*-
(in-package :yadfa-zones)
(ensure-zone (0 0 0 pirates-cove)
             :name "Pirate's Cove Entrance"
             :description "The entrance to Pirate's Cove"
             :enter-text "You're inside Pirate's Cove"
             :enemy-spawn-list (list '(:chance 1/8
                                       :enemies ((yadfa-enemies:diaper-pirate . (list :level (random-from-range 4 8)))))))
(ensure-zone (0 1 0 pirates-cove)
             :name "Pirate's Cove"
             :description "Where a bunch of pirates live"
             :enter-text "You're inside Pirate's Cove"
             :enemy-spawn-list (list '(:chance 1/8
                                       :enemies ((yadfa-enemies:diaper-pirate . (list :level (random-from-range 4 8)))))))
(ensure-zone (0 2 0 pirates-cove)
             :name "Pirate's Cove"
             :description "Where a bunch of pirates live"
             :enter-text "You're inside Pirate's Cove"
             :enemy-spawn-list (list '(:chance 1/8
                                       :enemies ((yadfa-enemies:diaper-pirate . (list :level (random-from-range 4 8)))))))
(ensure-zone (0 3 0 pirates-cove)
             :name "Pirate's Cove"
             :description "Where a bunch of pirates live"
             :enter-text "You're inside Pirate's Cove"
             :events '(yadfa-events:pirates-cove-1)
             :enemy-spawn-list (list '(:chance 1/8
                                       :enemies ((yadfa-enemies:diaper-pirate . (list :level (random-from-range 4 8)))))))
(ensure-zone (1 0 0 pirates-cove)
             :name "Pirate's Cove Lighthouse"
             :description "A lighthouse"
             :enter-text "You're inside Pirate's Cove"
             :enemy-spawn-list (list '(:chance 1/8
                                       :enemies ((yadfa-enemies:diaper-pirate . (list :level (random-from-range 4 8)))))))
#.`(progn
     ,@(iter (for i from 0 to 10)
             (collect `(ensure-zone (1 0 ,i pirates-cove)
                                    :name "Pirate's Cove Lighthouse"
                                    :description "A lighthouse"
                                    :enter-text "You're inside Pirate's Cove"
                                    :stairs (list ,@(typecase i
                                                      ((eql 0)
                                                       '(:up))
                                                      ((eql 10)
                                                       '(:down))
                                                      (t '(:up :down))))
                                    :enemy-spawn-list '((:chance 1/8
                                                         :enemies ((yadfa-enemies:diaper-pirate .
                                                                    (list :level (random-from-range 4 8))))))
                                    ,@(when (= i 10)
                                        '(:events '(yadfa-events:pirates-cove-2)))))))
