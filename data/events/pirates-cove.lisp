(in-package :yadfa/events)
(defevent pirates-cove-1
    :lambda '(lambda (self)
                 (declare (ignorable self))
                 ;; The class has to be finalized before we can call COMPUTE-DEFAULT-INITARGS on it
                 (unless (class-finalized-p (find-class 'yadfa/npcs:diaper-pirate))
                     (finalize-inheritance (find-class 'yadfa/npcs:diaper-pirate)))
                 (unless (class-finalized-p (find-class 'yadfa/npcs:thickly-diaper-pirate))
                     (finalize-inheritance (find-class 'yadfa/npcs:thickly-diaper-pirate)))
                 (let ((a-male (funcall (third (assoc (compute-default-initargs
                                                          (find-class 'yadfa/npcs:diaper-pirate))
                                                   :male))))
                          (b-male (funcall (third (assoc (compute-default-initargs
                                                             (find-class 'yadfa/npcs:thickly-diaper-pirate))
                                                      :male))))
                          (a-species (funcall (third (assoc (compute-default-initargs
                                                                (find-class 'yadfa/npcs:diaper-pirate))
                                                         :species))))
                          (a-species (funcall (third (assoc (compute-default-initargs
                                                                (find-class 'yadfa/npcs:thickly-diaper-pirate))
                                                         :species))))))
                 (format t "*You see one of the Pirate ~as changing an ~a into multiple layers of padding*~%"
                     a-species b-species)
                 (format t "*The ~a struggles to stand and waddles with the thick padding spreading ~a legs apart*~%"
                     b-species
                     (if b-male "his" "her"))
                 (format t "Padded ~a: Aww, looks like the baby ~a is taking ~a first steps.~%"
                     a-species
                     b-species
                     (if b-male "his" "her"))
                 (format t "Thickly Padded ~a: Shut it. Hey, isn't that an intruder?~%"
                     b-species)
                 (format t "Padded ~a: Apparently.~%"
                     a-species)
                 (set-new-battle
                     '((yadfa/npcs:diaper-pirate . (list
                                                       :level (random-from-range 2 5)
                                                       :species a-species
                                                       :male a-male))
                          (yadfa/npcs:thickly-diaper-pirate . (list
                                                                  :level (random-from-range 2 5)
                                                                  :species b-species
                                                                  :male b-male))))))
