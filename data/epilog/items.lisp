;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-items"; coding: utf-8-unix; -*-
(in-package :yadfa-items)
(defmethod use-script ((item enemy-catcher) (user base-character) (target yadfa-enemies:catchable-enemy))
  (cond
    ((>= (list-length (contained-enemies-of item)) (contained-enemies-max-length-of item))
     (f:fmt t (name-of item) " can't hold anymore enemies" #\Newline #\Newline))
    ((not (< (random 1.0l0) (* (catch-chance-multiplier-of item) (+ (catch-chance-delta-of item) (yadfa-enemies:catch-chance target)))))
     (f:fmt t "You failed to catch the " (name-of target) #\Newline #\Newline)
     (cond ((eq (device-health-of item) t) nil)
           ((<= (device-health-of item) 1)
            (alexandria:deletef (inventory-of (player-of *game*)) item :count 1))
           (t (decf (device-health-of item)))))
    (t
     (f:fmt t "You caught the " (name-of target) #\Newline #\Newline)

     ;; prevent the enemy from going again during the battle
     (alexandria:deletef (enemies-of *battle*) target)
     (alexandria:deletef (turn-queue-of *battle*) target)
     

     (push target (contained-enemies-of item))
     (unless (getf (special-actions-of item) :take-items)
       (setf (getf (special-actions-of item) :take-items)
             '(lambda (item user &key &allow-other-keys)
               (declare (ignore user))
               (setf (inventory-of (player-of *game*))
                (append (iter (for enemy in (contained-enemies-of item))
                          (dolist (item (inventory-of enemy))
                            (collect item))
                          (dolist (item (wear-of enemy))
                            (collect item))
                          (setf (inventory-of enemy) nil
                                (wear-of enemy) nil))
                 (inventory-of (player-of *game*)))))))
     (unless (getf (special-actions-of item) :adopt-enemies)
       (setf (getf (special-actions-of item) :adopt-enemies)
             '(lambda (item user &allow-other-keys :enemies enemies)
               (if (iter (for i in (contained-enemies-of item))
                     (when (typep (class-of i) 'yadfa-enemies:adoptable-enemy)
                       (return t)))
                   (progn
                     (setf enemies
                           (typecase enemies
                             (null (accept-with-effective-frame
                                     (clim:accepting-values (*query-io*  :resynchronize-every-pass t)
                                       (setf enemies (clim:accept `(clim:subset-alist ,(iter (for enemy in (contained-enemies-of item))
                                                                                         (when (typep (class-of i) 'yadfa-enemies:adoptable-enemy)
                                                                                           (collect (cons (name-of enemy) enemy)))))
                                                                  :prompt "Enemies to adopt"
                                                                  :stream *query-io*
                                                                  :view clim:+check-box-view+)))))
                             (type-specifier (iter (for enemy in (contained-enemies-of item))
                                               (when (typep i enemies)
                                                 (collect i))))
                             (list (iter
                                     (for enemy in (contained-enemies-of item))
                                     (generate current in enemies)
                                     (for index upfrom 0)
                                     (cond ((typep current '(not unsigned-byte))
                                            (error "ENEMIES must be a list of unsigned-bytes"))
                                           ((eql index current)
                                            (collect enemy)
                                            (next current)))))
                             (t (error "ENEMIES must either be a list of unsigned-bytes or a type specifier"))))
                     (alexandria:removef (contained-enemies-of item) enemies
                                         :test (lambda (o e)
                                                 (member e o)))
                     (alexandria:appendf (allies-of *game*) (iter (for i in enemies)
                                                              (write-line (yadfa-enemies:change-class-text i))
                                                              (collect (change-class i (get (class-name i) 'yadfa-enemies:change-class-target))))))
                   (format t "No enemies in there to adopt"))))))))
(defmethod use-script ((item enemy-catcher) (user base-character) (target yadfa-enemies:ghost))
  (f:fmt t "You failed to catch " (name-of target) #\Newline #\Newline)
  (cond ((eq (device-health-of item) t) nil)
        ((<= (device-health-of item) 1)
         (alexandria:deletef (inventory-of (player-of *game*)) item :count 1))
        (t (decf (device-health-of item)))))
(defmethod use-script ((item ghost-catcher) (user base-character) (target yadfa-enemies:ghost))
  (cond
    ((>= (list-length (contained-enemies-of item)) (contained-enemies-max-length-of item))
     (f:fmt t (name-of item) " can't hold anymore enemies" #\Newline #\Newline))
    ((not (< (random 1.0l0) (* (catch-chance-multiplier-of item) (+ (catch-chance-delta-of item) (yadfa-enemies:catch-chance target)))))
     (f:fmt t "You failed to catch the " (name-of target) #\Newline #\Newline)
     (cond ((eq (device-health-of item) t) nil)
           ((<= (device-health-of item) 1)
            (alexandria:deletef (inventory-of (player-of *game*)) item :count 1))
           (t (decf (device-health-of item)))))
    (t
     (f:fmt t "You caught the " (name-of target) #\Newline #\Newline)

     ;; prevent the enemy from going again during the battle
     (alexandria:deletef (enemies-of *battle*) target)
     (alexandria:deletef (turn-queue-of *battle*) target)

     (push target (contained-enemies-of item)))))
(defunassert yadfa-battle-commands:catch-enemy (&optional (target 'yadfa-enemies:catchable-enemy) (item 'enemy-catcher))
    (item type-specifier
          target (or unsigned-byte type-specifier))
  "Catches an enemy using. @var{ITEM} which is a type specifier. @var{TARGET} is an index or type specifier of an enemy in battle or a type specifier"
  (let ((selected-item (find item (inventory-of (player-of *game*))
                             :test (lambda (type-specifier obj)
                                     (and (typep obj `(and enemy-catcher ,type-specifier))
                                          (< (list-length (contained-enemies-of item))
                                             (contained-enemies-max-length-of item))))))
        (selected-target (let ((a (typecase target
                                    (unsigned-byte (nth target (enemies-of *battle*)))
                                    (type-specifier (find target (enemies-of *battle*)
                                                          :test (lambda (type-specifier obj)
                                                                  (typep obj `(and yadfa-enemies:catchable-enemy ,type-specifier))))))))
                           (or a
                               (progn
                                 (write-line "That target doesn't exist")
                                 (return-from yadfa-battle-commands:catch-enemy))))))
    (cond ((not item)
           (format t "You don't have an item with that type specifier that can catch that enemy~%")
           (return-from yadfa-battle-commands:catch-enemy))
          ((typep selected-target '(not yadfa-enemies:catchable-enemy))
           (format t "That enemy can't be caught~%")
           (return-from yadfa-battle-commands:catch-enemy)))
    (process-battle
     :item selected-item
     :selected-target selected-target)))
(defunassert yadfa-world-commands:loot-caught-enemies (&optional item)
    (item (or null unsigned-byte type-specifier))
  "Loots the enemies you caught. @var{ITEM} is either a type specifier or an unsiged-byte of the item. Don't specify if you want to loot the enemies of all items"
  (cond ((null item)
         (iter (for item in (inventory-of *game*))
           (when (typep item 'enemy-catcher)
             (funcall (coerce (action-lambda (getf (special-actions-of item) :take-items)) 'function)
                      item (player-of *game*) :action :take-items))))
        ((typep item 'unsigned-byte)
         (let* ((inventory-length (list-length (inventory-of (player-of *game*))))
                (selected-item (and (< item inventory-length) (nth item (inventory-of (player-of *game*))))))
           (cond ((>= item inventory-length)
                  (f:fmt t "You only have " inventory-length " items" #\Newline))
                 ((not (typep selected-item 'enemy-catcher))
                  (f:fmt t "That item isn't an enemy catcher" #\Newline))
                 (t (funcall (coerce (action-lambda (getf (special-actions-of selected-item) :take-items)) 'function)
                             selected-item (player-of *game*) :action :take-items)))))
        (t (let ((selected-item (find item (inventory-of *game*) :test (lambda (specifier item)
                                                                         (typep item `(and enemy-catcher ,specifier))))))
             (if selected-item
                 (funcall (coerce (action-lambda (getf (special-actions-of selected-item) :take-items)) 'function)
                          selected-item (player-of *game*) :action :take-items)
                 (f:fmt t "Either you don't have that item or it isn't an enemy catcher" #\Newline))))))
