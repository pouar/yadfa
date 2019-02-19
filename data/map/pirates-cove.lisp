(in-package :yadfa/zones)
(ensure-zone (0 0 0 pirates-cove)
    :name "Pirate's Cove Entrance"
    :description "The entrance to Pirate's Cove"
    :enter-text "You're inside Pirate's Cove"
    :enemy-spawn-list '((:max-random 8
                            :enemies ((yadfa/npcs:female-diapered-raccoon-bandit .
                                          (list :level (random-from-range 2 5)))))
                           (:max-random 8
                               :enemies ((yadfa/npcs:rookie-diapered-raccoon-bandit .
                                             (list :level (random-from-range 2 5)))))))
(ensure-zone (0 1 0 pirates-cove)
    :name "Pirate's Cove"
    :description "Where a bunch of pirates live"
    :enter-text "You're inside Pirate's Cove"
    :enemy-spawn-list '((:max-random 8
                            :enemies ((yadfa/npcs:female-diapered-raccoon-bandit .
                                          (list :level (random-from-range 2 5)))))
                           (:max-random 8
                               :enemies ((yadfa/npcs:rookie-diapered-raccoon-bandit .
                                             (list :level (random-from-range 2 5)))))))
(ensure-zone (1 0 0 pirates-cove)
    :name "Pirate's Cove Lighthouse"
    :description "A lighthouse"
    :enter-text "You're inside Pirate's Cove"
    :enemy-spawn-list '((:max-random 8
                            :enemies ((yadfa/npcs:female-diapered-raccoon-bandit .
                                          (list :level (random-from-range 2 5)))))
                           (:max-random 8
                               :enemies ((yadfa/npcs:rookie-diapered-raccoon-bandit .
                                             (list :level (random-from-range 2 5)))))))
(macro-level
    `(progn
         ,@(iter (for i from 0 to 10)
               (collect `(ensure-zone (1 0 ,i pirates-cove)
                             :name "Pirate's Cove Lighthouse"
                             :description "A lighthouse"
                             :enter-text "You're inside Pirate's Cove"
                             :enemy-spawn-list '((:max-random 8
                                                     :enemies ((yadfa/npcs:female-diapered-raccoon-bandit .
                                                                   (list :level (random-from-range 2 5)))))
                                                    (:max-random 8
                                                        :enemies ((yadfa/npcs:rookie-diapered-raccoon-bandit .
                                                                      (list :level (random-from-range 2 5)))))))))))
