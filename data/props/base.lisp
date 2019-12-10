(in-package :yadfa-props)
(defclass toilet (prop) ()
  (:default-initargs
   :name "Toilet"
   :description "A toilet"
   :actions (list :use (make-action
                        :documentation "Use the toilet. if WET or MESS is T, the player will empty his bladder/bowels completely. If a real is given, the player will empty his bladder by that amount, however the player will mess completely no matter what number you give it if you provide a number. If ALLY number is specified, that ALLY uses the toilet, otherwise it's the player"
                        :lambda '(lambda (prop &rest keys &key wet mess pull-pants-down ally &allow-other-keys)
                                  (declare (type prop prop)
                                   (type boolean pull-pants-down)
                                   (type (or integer null) ally)
                                   (type (or boolean real) wet mess)
                                   (ignore keys))
                                  (check-type prop prop)
                                  (check-type pull-pants-down boolean)
                                  (check-type ally (or integer null))
                                  (check-type wet (or boolean real))
                                  (check-type mess (or boolean real))
                                  (block nil
                                    (when (and ally (>= ally (list-length (allies-of *game*))))
                                      (format t "That ally doesn't exist~%")
                                      (return))
                                    (potty-on-toilet prop
                                                     :wet wet
                                                     :mess mess
                                                     :pants-down pull-pants-down
                                                     :user (if ally
                                                               (nth ally (allies-of *game*))
                                                               (player-of *game*))))))))
  (:documentation "Class for toilets. I'm pretty sure I don't need to tell you what these are for."))
(defclass washer (prop) ()
  (:default-initargs
   :name "Washer"
   :description "A place to clean your reusable diapers and all the clothes you've ruined"
   :actions (list :use (make-action
                        :documentation "Wash your clothes in this"
                        :lambda '(lambda
                                  (prop &rest keys &key &allow-other-keys)
                                  (declare (type prop prop) (ignore keys))
                                  (check-type prop prop)
                                  (yadfa-world:wash-all-in prop)))))
  (:documentation "Class for washers, you can wash your diapers and all the clothes you've ruined in these."))

(defclass automatic-changing-table (prop) ()
  (:default-initargs
   :name "Automatic Changing Table"
   :description "A changing table that automatically changes you"
   :actions (list :use (make-action
                        :documentation "Turn it on"
                        :lambda '(lambda (prop &rest keys &key &allow-other-keys)
                                  (declare (type prop prop)
                                   (ignore keys))
                                  (check-type prop prop)
                                  (iter (for j in (append (list (player-of *game*)) (allies-of *game*)))
                                    (let ((a (calculate-diaper-usage j)))
                                      (when (and
                                             (or
                                              (>=
                                               (getf a :sogginess)
                                               (/ (getf a :sogginess-capacity) 4))
                                              (>=
                                               (getf a :messiness)
                                               (/ (getf a :messiness-capacity) 4)))
                                             (filter-items (wear-of j) 'closed-bottoms))
                                        (format t "Mechanical arms come out of the changing table and strap ~a down on the table to prevent ~a from escaping and proceeds to change ~a~%~%"
                                                (name-of j)
                                                (if (malep j) "him" "her")
                                                (if (malep j) "him" "her"))
                                        (if (filter-items (wear-of j) 'padding)
                                            (progn
                                              (format t "~a: Hey!!! Don't change me here!!! People can see me!!! Stop!!!~%~%"
                                                      (name-of j)))
                                            (progn
                                              (format t "~a: Hey!!! I don't need diapers!!! Stop!!!~%~%"
                                                      (name-of j))))
                                        (change-the-baby j 'yadfa-items:kurikia-thick-diaper :locked t)
                                        (format t "*The machine removes ~a's soggy clothing (and any clothing that doesn't fit over the new diaper) and puts a thick diaper on ~a, then locks it to prevent the baby from removing it.*~%~%"
                                                (name-of j)
                                                (if (malep j) "him" "her"))
                                        (format t "*The machine unstraps ~a from the table and lets ~a go. The diaper is so thick ~a's legs are spread apart forcing ~a to waddle*~%~%"
                                                (name-of j)
                                                (if (malep j) "him" "her")
                                                (name-of j)
                                                (if (malep j) "him" "her"))
                                        (when (trigger-event 'yadfa-events:get-diaper-locked-1)
                                          (format t "*~a tugs at the tabs trying to remove them, but they won't budge. Better find a solution before its too late*~%~%" (name-of j))))))))))
  (:documentation "Class for washers, you can wash your diapers and all the clothes you've ruined in these."))
(defclass checkpoint (prop) ()
  (:default-initargs
   :name "Checkpoint"
   :description "You can use this to set this zone as a checkpoint so when you lose a battle, you'll warp to here rather than at the beginning of the game"
   :actions (list :set-checkpoint (make-action :documentation "Set checkpoint"
                                               :lambda '(lambda (prop &rest keys &key &allow-other-keys)
                                                         (declare
                                                          #+sbcl (type prop prop)
                                                          (ignore keys prop))
                                                         (setf (warp-on-death-point-of (player-of *game*)) (position-of (player-of *game*)))
                                                         (format t "You will now teleport here when you black out")))))
  (:documentation "Class for washers, you can wash your diapers and all the clothes you've ruined in these."))
(defclass shop (prop)
  ((items-for-sale
    :initarg :items-for-sale
    :initform ()
    :accessor items-for-sale-of
    :documentation "Quoted list of class names for sale"))
  (:default-initargs
   :name "Shop"
   :description "A place to buy crap with your bitcoins")
  (:documentation "Class for shops, you can buy stuff from these."))
(defmethod initialize-instance :after
    ((c shop) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  (appendf (actions-of c)
           (list :list-items-for-sale (make-action :documentation "List items for sale"
                                                   :lambda '(lambda (prop &rest keys &key &allow-other-keys)
                                                             (declare (type prop prop)
                                                              (ignore keys))
                                                             (check-type prop prop)
                                                             (shopfun (items-for-sale-of prop) :format-items t)))
                 :buy-items (make-action :documentation "Buy items. ITEMS is a list of conses where each cons is in the form of (INDEX-OF-ITEM-TO-BUY . QUANTITY-OF-ITEMS-TO-BUY)"
                                         :lambda '(lambda (prop &rest keys &key items &allow-other-keys)
                                                   (declare (type prop prop) (type list items) (ignore keys))
                                                   (check-type prop prop)
                                                   (check-type items list)
                                                   (shopfun (items-for-sale-of prop)
                                                    :items-to-buy items
                                                    :user (player-of *game*))))
                 :sell-items (make-action :documentation "Sell items. ITEMS is a list of indexes where each index corresponds to an item in your inventory"
                                          :lambda '(lambda (prop &rest keys &key items &allow-other-keys)
                                                    (declare (type prop prop) (type list items) (ignore keys))
                                                    (check-type prop prop)
                                                    (check-type items list)
                                                    (shopfun (items-for-sale-of prop)
                                                     :items-to-sell items
                                                     :user (player-of *game*)))))))
(defclass vending-machine (prop)
  ((items-for-sale
    :initarg :items-for-sale
    :initform ()
    :accessor items-for-sale-of
    :documentation "Quoted list of class names for sale"))
  (:default-initargs
   :name "Vending Machine"
   :description "An automated machine where you can buy items from")
  (:documentation "Class for vending machines, Functions like a shop, but only lets you buy items instead of selling them"))
(defmethod initialize-instance :after
    ((c vending-machine) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  (appendf (actions-of c)
           (list :list-items-for-sale (make-action :documentation "List items for sale"
                                                   :lambda '(lambda (prop &rest keys &key &allow-other-keys)
                                                             (declare #+sbcl (type prop prop)
                                                                      (ignore keys))
                                                             #-sbcl (check-type prop prop)
                                                             (shopfun (items-for-sale-of prop) :format-items t)))
                 :buy-items (make-action :documentation "Buy items. ITEMS is a list of conses where each cons is in the form of (INDEX-OF-ITEM-TO-BUY . QUANTITY-OF-ITEMS-TO-BUY)"
                                         :lambda '(lambda (prop &rest keys &key items &allow-other-keys)
                                                   (declare #+sbcl (type prop prop)
                                                            #+sbcl (type list items)
                                                            (ignore keys))
                                                   #-sbcl (check-type prop prop)
                                                   #-sbcl (check-type items list)
                                                   (shopfun (items-for-sale-of prop)
                                                    :items-to-buy items
                                                    :user (player-of *game*)))))))
(defclass debug-shop (prop) ()
  (:default-initargs
   :name "Shop"
   :description "A place to buy crap with your bitcoins")
  (:documentation "Class for shops, you can buy stuff from these."))
(defmethod initialize-instance :after
    ((c debug-shop) &rest args &key &allow-other-keys)
  (declare (ignorable args))
  (appendf (actions-of c)
           (list :list-items-for-sale (make-action :documentation "List items for sale"
                                                   :lambda '(lambda (prop &rest keys &key &allow-other-keys)
                                                             (declare #+sbcl (type prop prop)
                                                                      (ignore keys)
                                                              (ignorable prop))
                                                             #-sbcl (check-type prop prop)
                                                             (shopfun (let ((a ()))
                                                                        (iter (for i in (list-all-packages))
                                                                          (unless (equal i (find-package :yadfa))
                                                                            (do-external-symbols  (s i)
                                                                              (when (and
                                                                                     (find-class s nil)
                                                                                     (c2mop:subclassp
                                                                                      (find-class s)
                                                                                      (find-class 'item))
                                                                                     (tossablep (make-instance s)))
                                                                                (push (cons s nil) a)))))
                                                                        a)
                                                              :format-items t)))
                 :buy-items (make-action :documentation "Buy items. ITEMS is a list of conses where each cons is in the form of (INDEX-OF-ITEM-TO-BUY . QUANTITY-OF-ITEMS-TO-BUY)"
                                         :lambda '(lambda (prop &rest keys &key items &allow-other-keys)
                                                   (declare #+sbcl (type prop prop)
                                                            #+sbcl (type list items)
                                                            (ignore keys)
                                                    (ignorable prop))
                                                   #-sbcl (check-type prop prop)
                                                   #-sbcl (check-type items list)
                                                   (shopfun
                                                    (let ((a ()))
                                                      (iter (for i in (list-all-packages))
                                                        (unless
                                                            (equal i (find-package :yadfa))
                                                          (do-external-symbols (s i)
                                                            (when (and
                                                                   (find-class s nil)
                                                                   (c2mop:subclassp
                                                                    (find-class s)
                                                                    (find-class 'item))
                                                                   (tossablep (make-instance s)))
                                                              (push (cons s nil) a)))))
                                                      a)
                                                    :items-to-buy items
                                                    :user (player-of *game*))))
                 :sell-items (make-action :documentation "Sell items. ITEMS is a list of indexes where each index corresponds to an item in your inventory"
                                          :lambda '(lambda (prop &rest keys &key items &allow-other-keys)
                                                    (declare #+sbcl (type prop prop)
                                                             #+sbcl (type list items)
                                                             (ignore keys)
                                                     (ignorable prop))
                                                    #-sbcl (check-type prop prop)
                                                    #-sbcl (check-type items list)
                                                    (shopfun
                                                     (let ((a ()))
                                                       (iter
                                                         (for i in (list-all-packages))
                                                         (unless (equal i (find-package :yadfa))
                                                           (do-external-symbols (s i)
                                                             (when (and
                                                                    (find-class s nil)
                                                                    (c2mop:subclassp
                                                                     (find-class s)
                                                                     (find-class 'item))
                                                                    (tossablep (make-instance s)))
                                                               (push (cons s nil) a)))))
                                                       a)
                                                     :items-to-sell items
                                                     :user (player-of *game*)))))))
(defclass bed (prop) ()
  (:default-initargs
   :name "Bed"
   :description "A place to sleep and recover. Be sure to go potty so you don't wet it."
   :actions (list :sleep (make-action :documentation "Sleep in this bed and recover your health and energy. Be sure to go potty before you go to bed so you don't wet it"
                                      :lambda '(lambda (prop &rest keys &key &allow-other-keys)
                                                (declare #+sbcl (type prop prop)
                                                         (ignore keys)
                                                 (ignorable prop))
                                                #-sbcl (check-type prop prop)
                                                (go-to-sleep)))))
  (:documentation "Class for beds, you can sleep in these."))
