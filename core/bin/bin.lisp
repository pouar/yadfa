;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa"; coding: utf-8-unix; -*-
(in-package :yadfa)
(defunassert yadfa-bin:get-inventory-of-type (type)
  (type type-specifier)
  (get-positions-of-type type (inventory-of (player-of *game*))))
(defun yadfa-bin:reload-files (&rest keys &key compiler-verbose &allow-other-keys)
  "Intended for developers. Use this to recompile the game without having to close it. Accepts the same keyword arguments as @code{ASDF:LOAD-SYSTEM} and @code{ASDF:OPERATE}. Set @var{COMPILER-VERBOSE} to @code{T} to print the compiling messages. setting @var{LOAD-SOURCE} to @code{T} will avoid creating fasls"
  (let ((*compile-verbose* compiler-verbose) (*compile-print* compiler-verbose))
    (apply #'asdf:load-system :yadfa :allow-other-keys t keys)
    (apply #'load-mods :allow-other-keys t keys))
  (switch-user-packages))
(defun yadfa-bin:enable-mods (systems)
  #.(format nil "Enable a mod, the modding system is mostly just asdf, @var{SYSTEM} is a keyword which is the name of the system you want to enable

~a."
            (xref yadfa-bin:disable-mods :function))
  (let ((systems (iter (for i in (a:ensure-list systems))
                       (collect (asdf:coerce-name i)))))
    (dolist (system (remove-duplicates systems :test #'string=))
      (asdf:find-system system))
    (dolist (system systems)
      (pushnew system *mods* :test #'string=)
      (asdf:load-system system))
    (a:with-output-to-file (stream #P"yadfa:config;mods.conf"
                                   :if-exists :supersede
                                   :external-format :utf-8)
      (write *mods* :stream stream)))
  systems)
(defun yadfa-bin:disable-mods (systems)
  #.(format nil "Disable a mod, the modding system is mostly just asdf, @var{SYSTEM} is a keyword which is the name of the system you want to enable

~a."
            (xref yadfa-bin:enable-mods :function))
  (let ((systems (delete-duplicates (iter (for i in (a:ensure-list systems))
                                          (collect (asdf:coerce-name i)))
                                    :test #'string=)))
    (a:deletef *mods* systems :test (lambda (o e)
                                      (member e o :test #'string=)))
    (a:with-output-to-file (stream #P"yadfa:config;mods.conf"
                                   :if-exists :supersede
                                   :external-format :utf-8)
      (write *mods* :stream stream)))
  systems)
(defunassert yadfa-bin:toggle-onesie (&key wear user)
  (wear (or type-specifier unsigned-byte null) user (or type-specifier  unsigned-byte null))
  "Open or closes your onesie. @var{WEAR} is the index of a onesie. Leave @code{NIL} for the outermost onesie. @var{USER} is the index of an ally. Leave @code{NIL} to refer to yourself"
  (handle-user-input ((allies-length (list-length (allies-of *game*)))
                      (inventory-length (list-length (wear-of (player-of *game*))))
                      (selected-user (if user (if (numberp user)
                                                  (nth user (allies-of *game*))
                                                  (find user (allies-of *game*) :test (lambda (o e)
                                                                                        (typep e o))))
                                         (player-of *game*)))
                      (selected-wear (when wear (if (numberp wear)
                                                    (nthcdr wear (wear-of (player-of *game*)))
                                                    (member wear (wear-of (player-of *game*)) :test (lambda (o e)
                                                                                                      (typep e o)))))))
                     (*query-io* ((and user (numberp user) (>= user allies-length))
                                  (user)
                                  :prompt-text "Enter a different ally"
                                  :error-text (format nil "You only have ~d allies" allies-length))
                                 ((and user (typep user 'type-specifier) (not selected-user))
                                  (user)
                                  :prompt-text "Enter a different ally"
                                  :error-text (format nil "Ally ~s doesn't exist" user))
                                 ((and wear (numberp wear) (>= wear inventory-length))
                                  (wear)
                                  :prompt-text "Select a different clothing"
                                  :error-text (format nil "You're only wearing ~a items" inventory-length))
                                 ((and wear (typep wear 'type-specifier) (not selected-wear))
                                  (wear)
                                  :prompt-text "Select a different clothing"
                                  :error-text (format nil "You're not wearing that item"))
                                 ((let ((selected-wear (if wear
                                                           selected-wear
                                                           (iter (for item on (wear-of selected-user))
                                                                 (when (typep (car item) 'onesie)
                                                                   (leave item))
                                                                 (finally (format t "~a isn't wearing a onesie"
                                                                                  (name-of selected-user)))))))
                                    (handler-case (progn (toggle-onesie (car selected-wear) selected-wear selected-user)
                                                         (let* ((male (malep selected-user))
                                                                (hisher (if male "his" "her"))
                                                                (onesie (car selected-wear)))
                                                           (if (typep (car selected-wear) 'onesie/closed)
                                                               (format t "~a snaps ~a ~a~%~%"
                                                                       (name-of selected-user)
                                                                       hisher
                                                                       (name-of onesie))
                                                               (format t "~a unsnaps ~a ~a~%~%"
                                                                       (name-of selected-user)
                                                                       hisher
                                                                       (name-of onesie)))))
                                      (onesie-too-thick (c)
                                        (let* ((user (user-of c))
                                               (clothes (clothes-of c))
                                               (male (malep user))
                                               (hisher (if male "his" "her")))
                                          (format t "~a struggles to snap the bottom of ~a ~a like a toddler who can't dress ~aself but ~a ~a is too thick~%~%"
                                                  (name-of user)
                                                  hisher
                                                  (name-of (car clothes))
                                                  (if male "him" "her")
                                                  hisher
                                                  (name-of (thickest (cdr clothes))))))
                                      (onesie-locked (c)
                                        (let ((user (user-of c)))
                                          (format t "~a can't unsnap ~a ~a as it's locked~%~%"
                                                  (name-of user)
                                                  (if (malep user) "his" "her")
                                                  (name-of (car (clothes-of c)))))))
                                    nil)
                                  (wear)
                                  :prompt-text "Select a different clothing"))))
(defunassert yadfa-bin:lst (&key inventory inventory-group props wear user directions moves position map descriptions describe-zone)
  (user (or unsigned-byte boolean)
        map (or boolean integer)
        inventory type-specifier)
  "used to list various objects and properties, @var{INVENTORY} takes a type specifier for the items you want to list in your inventory. setting @var{INVENTORY} to @code{T} will list all the items. @var{INVENTORY-GROUP} is similar to @var{INVENTORY}, but will group the items by class name. @var{WEAR} is similar to @var{INVENTORY} but lists clothes you're wearing instead. setting @var{DIRECTIONS} to non-NIL will list the directions you can walk.setting @var{MOVES} to non-NIL will list the moves you know. setting @var{USER} to @code{T} will cause @var{MOVES} and @var{WEAR} to apply to the player, setting it to an integer will cause it to apply it to an ally. Leaving it at @code{NIL} will cause it to apply to everyone. setting @var{POSITION} to true will print your current position. Setting @var{MAP} to a number will print the map with the floor number set to @var{MAP}, setting @var{MAP} to @code{T} will print the map of the current floor you're on. When printing the map in McCLIM, red means there's a warp point, dark green is the zone with the player, blue means there are stairs. These 3 colors will blend with each other to make the final color"
  (let ((allies-length (list-length (allies-of *game*))))
    (labels ((format-table (header &rest body)
               (c:formatting-table (t :x-spacing 20)
                                   (c:with-text-style (*query-io* (c:make-text-style nil :bold nil))
                                     (c:formatting-row ()
                                                       (iter (for cell in header)
                                                             (c:formatting-cell ()
                                                                                (typecase cell
                                                                                  (string (write-string cell))
                                                                                  (t (write cell)))))))
                                   (iter (for row in body)
                                         (c:formatting-row ()
                                                           (iter (for cell in row)
                                                                 (c:formatting-cell ()
                                                                                    (typecase cell
                                                                                      (string (write-string cell))
                                                                                      (t (write cell)))))))))
             (format-items (list item &optional user)
               (format t "Number of items listed: ~a~%~%" (iter (with j = 0)
                                                                (for i in list)
                                                                (when (typep i item)
                                                                  (incf j))
                                                                (finally (return j))))
               (when user
                 (format t "~a:~%~%" (name-of user)))
               (apply #'format-table '("Index" "Name" "Class" "Wet" "Wetcap" "Mess" "Messcap")
                      (let ((j 0)) (iter (for i in list)
                                         (when (typep i item)
                                           (collect (list j
                                                          (name-of i)
                                                          (type-of i)
                                                          (if (typep i 'closed-bottoms) (coerce (sogginess-of i) 'long-float) nil)
                                                          (if (typep i 'closed-bottoms) (coerce (sogginess-capacity-of i) 'long-float) nil)
                                                          (if (typep i 'closed-bottoms) (coerce (messiness-of i) 'long-float) nil)
                                                          (if (typep i 'closed-bottoms) (coerce (messiness-capacity-of i) 'long-float) nil))))
                                         (incf j)))))
             (format-moves (user)
               (format t "~a:~%~%" (name-of user))
               (apply #'format-table '("Symbol" "Name" "Description")
                      (iter (for i in (moves-of user))
                            (when i (collect (list (class-name (class-of i)) (name-of i) (description-of i)))))))
             (format-user (user)
               (format t "Name: ~a~%Species: ~a~%Description: ~a~%~%"
                       (name-of user)
                       (species-of user)
                       (description-of user)))
             (check-allies ()
               (when (and (typep user 'unsigned-byte) (< allies-length user))
                 (format t "You only have ~d allies~%" allies-length)
                 (return-from yadfa-bin:lst))))
      (check-allies)
      (when inventory
        (with-effective-frame
            (format-items (inventory-of (player-of *game*)) inventory)))
      (when describe-zone
        (format t "~a~%" (get-zone-text (description-of (typecase describe-zone
                                                          (zone describe-zone)
                                                          (list (get-zone describe-zone))
                                                          (t (get-zone (position-of (player-of *game*)))))))))
      (when inventory-group
        (with-effective-frame
            (let ((a ()))
              (iter (for i in (inventory-of (player-of *game*)))
                    (when (typep i inventory-group)
                      (if (getf a (class-name (class-of i)))
                          (incf (second (getf a (class-name (class-of i)))))
                          (setf (getf a (class-name (class-of i))) (list (name-of (make-instance (class-name (class-of i)))) 1)))))
              (apply #'format-table '("Class Name" "Name" "Quantity")
                     (iter (for (key value) on a by #'cddr)
                           (collect (apply 'list key value)))))))
      (when wear
        (with-effective-frame
            (cond ((not user)
                   (format-items (wear-of (player-of *game*)) wear (player-of *game*))
                   (iter (for k in (allies-of *game*))
                         (format-items (wear-of k) wear k)))
                  ((typep user 'integer)
                   (let ((selected-ally (nth user (allies-of *game*))))
                     (check-allies)
                     (format-items (wear-of selected-ally) wear selected-ally)))
                  (t
                   (format-items (wear-of (player-of *game*)) wear (player-of *game*))))))
      (when moves
        (with-effective-frame
            (cond ((typep user 'real)
                   (let ((selected-ally (nth user (allies-of *game*))))
                     (format-moves selected-ally)))
                  ((not user)
                   (format-moves (player-of *game*))
                   (iter (for k in (allies-of *game*))
                         (format-moves k)))
                  (t (format-moves (player-of *game*))))))
      (when props
        (with-effective-frame
            (apply #'format-table '("Keyword" "Object")
                   (iter (for (a b) on (get-props-from-zone (position-of (player-of *game*))) by #'cddr)
                         (when b
                           (collect (list a (name-of b))))))))
      (let ((player-position (position-of (player-of *game*))))
        (declare (type list player-position))
        (destructuring-bind (x y z map) player-position
          (declare (type integer x y z)
                   (type symbol map))
          (let ((x-y-z (list x y z)))
            (declare (type list x-y-z))
            (flet ((z (delta direction x-y-z player-position map)
                     (declare (type keyword direction)
                              (type list delta x-y-z player-position)
                              (type symbol map))
                     (let ((position `(,@(mapcar #'+ x-y-z delta) ,map)))
                       (declare (type list position))
                       (when (and (get-zone position)
                                  (get-zone player-position)
                                  (not (getf-direction player-position direction :hidden))
                                  (not (hiddenp (get-zone position)))
                                  (or (and (s:memq direction '(:up :down)) (s:memq direction (stairs-of (get-zone player-position))))
                                      (not (s:memq direction '(:up :down)))))
                         (format t "~s ~a~%"
                                 direction
                                 (name-of (get-zone position)))))))
              (when directions
                (z '(1 0 0) :east x-y-z player-position map)
                (z '(-1 0 0) :west x-y-z player-position map)
                (z '(0 -1 0) :north x-y-z player-position map)
                (z '(0 1 0) :south x-y-z player-position map)
                (z '(0 0 1) :up x-y-z player-position map)
                (z '(0 0 -1) :down x-y-z player-position map)
                (when (warp-points-of (get-zone (position-of (player-of *game*))))
                  (iter (for (a b) on (warp-points-of (get-zone (position-of (player-of *game*)))) by #'cddr)
                        (when (and (get-zone b) (not (hiddenp (get-zone b))))
                          (format t "~s ~a~%" a (name-of (get-zone b)))))))))))
      (when position
        (format t "Your current position is ~s~%" (position-of (player-of *game*))))
      (when map
        (cond ((eq map t)
               (print-map t))
              (t (print-map
                  (destructuring-bind (x y z m) (position-of (player-of *game*))
                    (declare (type integer x y z)
                             (type symbol m)
                             (ignore z))
                    (list x y map m))))))
      (when descriptions
        (cond ((eq user t)
               (format-user (player-of *game*)))
              ((typep user 'unsigned-byte)
               (format-user (nth user (allies-of *game*))))
              (t
               (format-user (player-of *game*))
               (iter (for i in (allies-of *game*))
                     (format t "Name: ~a~%Species: ~a~%Description: ~a~%~%" (name-of i) (species-of i) (description-of i)))))))))
(defunassert yadfa-bin:get-stats (&key inventory wear prop item attack ally wield enemy)
  (ally (or null unsigned-byte type-specifier)
        wear (or null unsigned-byte type-specifier)
        inventory (or null unsigned-byte type-specifier)
        enemy (or null unsigned-byte type-specifier))
  "lists stats about various items in various places. @var{INVENTORY} is the index of an item in your inventory. @var{WEAR} is the index of what you or your ally is wearing. @var{PROP} is a keyword that refers to the prop you're selecting. @var{ITEM} is the index of an item that a prop has and is used to print information about that prop. @var{ATTACK} is a keyword referring to the move you or your ally has when showing that move. @var{ALLY} is the index of an ally on your team when selecting @var{INVENTORY} or @var{MOVE}, don't set @var{ALLY} if you want to select yourself."
  (when (and ally (list-length-> ally (allies-of *game*)))
    (write-line "That ally doesn't exist")
    (return-from yadfa-bin:get-stats))
  (let* ((selected-user (cond (ally (if (typep ally 'type-specifier)
                                        (find ally (allies-of *game*)
                                              :test (lambda (o e)
                                                      (typep e o)))
                                        (nth ally (allies-of *game*))))
                              ((and enemy *battle*)
                               (if (typep enemy 'type-specifier)
                                   (find enemy (enemies-of *battle*)
                                         :test (lambda (o e)
                                                 (typep e o)))
                                   (nth enemy (enemies-of *battle*))))
                              (t (player-of *game*))))
         (wear (typecase wear
                 (type-specifier (find wear (wear-of selected-user)
                                       :test (lambda (o e)
                                               (typep e o))))
                 (unsigned-byte (nth wear (wear-of selected-user)))))
         (inventory (typecase inventory
                      (type-specifier
                       (find inventory (inventory-of (player-of *game*))
                             :test (lambda (o e)
                                     (typep e o))))
                      (unsigned-byte
                       (nth inventory (inventory-of (player-of *game*)))))))
    (when wield
      (describe-item (wield-of selected-user)))
    (when inventory
      (describe-item inventory))
    (when wear
      (describe-item (find wear (wear-of selected-user)) t))
    (when attack
      (format t "Name:~a~%Description~a~%Energy Cost: ~f~%~%"
              (name-of (get-move attack selected-user))
              (description-of (get-move attack selected-user))
              (energy-cost-of (get-move attack selected-user))))
    (when prop
      (handle-user-input ()
                         (*query-io* ((or (check-type prop (and (not null) symbol)) (null (getf (get-props-from-zone (position-of (player-of *game*))) prop)))
                                      (prop)
                                      :prompt-text "Enter a different prop, or exit and use (lst :props t) to get the list of props and try again"
                                      :error-text "That prop doesn't exist")
                                     ((null (nth item (items-of (getf (get-props-from-zone (position-of (player-of *game*)))
                                                                      (the (and (not null) symbol) prop)))))
                                      (item)
                                      :prompt-text "Enter a different item"
                                      :error-text "That item doesn't exist"))
                         (describe-item (nth (the unsigned-byte item)
                                             (items-of (getf (get-props-from-zone (position-of (player-of *game*)))
                                                             (the (and (not null) symbol) prop)))))))))
(defunassert yadfa-bin:wear (&key (inventory 0) (wear 0) user)
  (user (or null unsigned-byte)
        wear unsigned-byte
        inventory (or type-specifier unsigned-byte))
  #.(format nil "Wear an item in your inventory. @var{WEAR} is the index you want to place this item. Smaller index refers to outer clothing. @var{INVENTORY} is an index in your inventory of the item you want to wear. You can also give it a type specifier which will pick the first item in your inventory of that type. @var{USER} is an index of an ally. Leave this at @code{NIL} to refer to yourself.

~a, ~a, and ~a."
            (xref yadfa-bin:unwear :function) (xref yadfa-bin:change :function) (xref yadfa-bin:lst :function))
  (handle-user-input ((selected-user (if user
                                         (nth user (allies-of *game*))
                                         (player-of *game*)))
                      (item (typecase inventory
                              (unsigned-byte
                               (nth inventory (inventory-of (player-of *game*))))
                              (type-specifier
                               (find inventory (inventory-of (player-of *game*))
                                     :test #'(lambda (type-specifier obj)
                                               (typep obj type-specifier))))))
                      i a
                      (wear-length (list-length (wear-of selected-user))))
                     (*query-io* ((when (list-length-> 1 (inventory-of (player-of *game*)))
                                    (format t "~a doesn't have any clothes to put on~%" (name-of selected-user))
                                    (return-from yadfa-bin:wear))
                                  ())
                                 ((not item)
                                  (inventory)
                                  :prompt-text "Enter a different item"
                                  :error-text  "INVENTORY isn't a valid item")
                                 ((not (typep item 'clothing))
                                  (inventory)
                                  :prompt-text "Enter a different item"
                                  :error-text (format nil "That ~a isn't something you can wear~%" (name-of item)))
                                 ((< wear-length wear)
                                  (wear)
                                  :prompt-text "Enter a different index"
                                  :error-text (format nil "“:WEAR ~d” doesn't refer to a valid position as it can't go past the items you're current wearing which is currently ~d"
                                                      wear
                                                      wear-length)))
                     (cond ((let ((not-wear (typecase (must-not-wear*-of (get-zone (position-of (player-of *game*))))
                                              (cons (must-not-wear*-of (get-zone (position-of (player-of *game*)))))
                                              (symbol (gethash (must-not-wear*-of *game*) (must-not-wear*-of (get-zone (position-of (player-of *game*)))))))))
                              (and (typep item (car not-wear)) (not (funcall (coerce (cdr not-wear) 'function) selected-user))))
                            (return-from yadfa-bin:wear))
                           ((and (> wear 0) (iter (for i in (butlast (wear-of selected-user) (- wear-length wear)))
                                                  (when (and (typep i 'closed-bottoms) (lockedp i))
                                                    (format t "~a can't remove ~a ~a to put on ~a ~a as it's locked~%"
                                                            (name-of selected-user)
                                                            (if (malep selected-user) "his" "her")
                                                            (name-of i)
                                                            (if (malep selected-user) "his" "her")
                                                            (name-of item))
                                                    (leave t))))
                            (return-from yadfa-bin:wear)))
                     (setf a (insert (wear-of selected-user) item wear)
                           i (iter (for outer in (reverse (subseq a 0 (1+ wear))))
                                   (with b = (reverse a))
                                   (when (and (typep outer 'bottoms) (thickness-capacity-of outer) (> (fast-thickness b outer) (thickness-capacity-of outer)))
                                     (leave (thickest (cdr (s:memq outer a)))))))
                     (if i
                         (format t "~a struggles to fit ~a ~a over ~a ~a in a hilarious fashion but fail to do so.~%"
                                 (name-of selected-user)
                                 (if (malep selected-user) "his" "her")
                                 (name-of item)
                                 (if (malep selected-user) "his" "her")
                                 (name-of i))
                         (progn (when *battle*
                                  (format t "The ~a you're battling stops and waits for you to put on your ~a because Pouar never prevented this function from being called in battle~%"
                                          (if (list-length-< 1 (enemies-of *battle*)) "enemies" "enemy")
                                          (name-of item)))
                                (format t "~a puts on ~a ~a~%" (name-of selected-user) (if (malep selected-user) "his" "her") (name-of item))
                                (a:deletef (inventory-of (player-of *game*)) item :count 1)
                                (setf (wear-of selected-user) a)))))
(defunassert yadfa-bin:unwear (&key (inventory 0) (wear 0) user)
  (user (or unsigned-byte null)
        inventory unsigned-byte
        wear (or type-specifier unsigned-byte))
  #.(format nil "Unwear an item you're wearing. @var{INVENTORY} is the index you want to place this item. @var{WEAR} is the index of the item you're wearing that you want to remove. You can also set @var{WEAR} to a type specifier for the outer most clothing of that type. @var{USER} is a integer referring to the index of an ally. Leave at @code{NIL} to refer to yourself

~a, ~a, and ~a."
            (xref yadfa-bin:wear :function) (xref yadfa-bin:change :function) (xref yadfa-bin:lst :function))
  (handle-user-input ((selected-user (if user
                                         (nth user (allies-of *game*))
                                         (player-of *game*)))
                      (item (typecase wear
                              (unsigned-byte
                               (nth wear (wear-of (player-of *game*))))
                              (type-specifier
                               (find wear (wear-of (player-of *game*))
                                     :test #'(lambda (type-specifier obj)
                                               (typep obj type-specifier))))))
                      (inventory-length (list-length (inventory-of (player-of *game*)))))
                     (*query-io* ((when (list-length-> 1 (wear-of selected-user))
                                    (format t "~a isn't wearing any clothes to remove~%" (name-of selected-user))
                                    (return-from yadfa-bin:unwear))
                                  ())
                                 ((not item)
                                  (wear)
                                  :prompt-text "Enter a different item"
                                  :error-text "WEAR isn't a valid item")
                                 ((< inventory-length inventory)
                                  (inventory)
                                  :prompt-text "Enter a different index"
                                  :error-text (format nil "“:INVENTORY ~d” doesn't refer to a valid position as it can't go past the items you currently have in your inventory which is currently ~d~%"
                                                      inventory inventory-length)))
                     (cond ((and
                             (not (eq (player-of *game*) selected-user))
                             (typep item 'diaper)
                             (typep user '(not potty-trained-team-member))
                             (list-length-> 2 (filter-items (wear-of selected-user) 'diaper)))
                            (format t "Letting ~a go without padding is a really bad idea. Don't do it.~%"
                                    (name-of selected-user))
                            (return-from yadfa-bin:unwear))
                           ((let ((wear (typecase (must-wear*-of (get-zone (position-of (player-of *game*))))
                                          (cons (must-wear*-of (get-zone (position-of (player-of *game*)))))
                                          (symbol (gethash (must-wear*-of *game*)
                                                           (must-wear*-of (get-zone (position-of (player-of *game*)))))))))
                              (and (typep item (car wear))
                                   (list-length->= 1 (filter-items (wear-of selected-user) (car wear)))
                                   (not (funcall (coerce (cdr wear) 'function) selected-user))))
                            (return-from yadfa-bin:unwear))
                           ((iter (for i in (butlast (wear-of selected-user) (- (list-length (wear-of selected-user)) (position item (wear-of selected-user)) 1)))
                                  (when (and (typep i 'closed-bottoms) (lockedp i))
                                    (format t "~a can't remove ~a ~a to take off ~a ~a as it's locked~%"
                                            (name-of selected-user)
                                            (if (malep selected-user) "his" "her")
                                            (name-of i)
                                            (if (malep selected-user) "his" "her")
                                            (name-of item))
                                    (leave t)))
                            (return-from yadfa-bin:unwear)))
                     (when *battle*
                       (format t "The ~a you're battling stops and waits for you to take off your ~a because Pouar never prevented this function from being called in battle~%"
                               (if (list-length-< 1 (enemies-of *battle*))
                                   "enemies"
                                   "enemy")
                               (name-of item)))
                     (format t "~a takes off ~a ~a~%" (name-of selected-user) (if (malep selected-user) "his" "her") (name-of item))
                     (a:deletef (wear-of (player-of *game*)) item :count 1)
                     (insertf (inventory-of (player-of *game*)) item inventory)))
(defunassert yadfa-bin:change (&key (inventory 0) (wear 0) user)
  (user (or null unsigned-byte)
        inventory (or type-specifier unsigned-byte)
        wear (or type-specifier unsigned-byte))
  #.(format nil "Change one of the clothes you're wearing with one in your inventory. @var{WEAR} is the index of the clothing you want to replace. Smaller index refers to outer clothing. @var{INVENTORY} is an index in your inventory of the item you want to replace it with. You can also give @var{INVENTORY} and @var{WEAR} a quoted symbol which can act as a type specifier which will pick the first item in your inventory of that type. @var{USER} is an index of an ally. Leave this at @code{NIL} to refer to yourself.

~a, ~a, and ~a."
            (xref yadfa-bin:unwear :function) (xref yadfa-bin:wear :function) (xref yadfa-bin:lst :function))
  (handle-user-input ((selected-user (if user
                                         (nth user (allies-of *game*))
                                         (player-of *game*)))
                      (inventory (typecase inventory
                                   (unsigned-byte
                                    (nth inventory (inventory-of (player-of *game*))))
                                   (type-specifier
                                    (find inventory (inventory-of (player-of *game*))
                                          :test #'(lambda (type-specifier obj)
                                                    (typep obj type-specifier))))))
                      (wear (typecase wear
                              (unsigned-byte
                               (nth wear (wear-of (player-of *game*))))
                              (type-specifier
                               (find wear (wear-of (player-of *game*))
                                     :test #'(lambda (type-specifier obj)
                                               (typep obj type-specifier))))))
                      i a)
                     (*query-io* ((when (list-length-> 1 (wear-of selected-user))
                                    (format t "~a isn't wearing any clothes to change~%" (name-of selected-user))
                                    (return-from yadfa-bin:change))
                                  ())
                                 ((not inventory)
                                  (inventory)
                                  :prompt-text "Enter a different item"
                                  :error-text "INVENTORY isn't valid")
                                 ((not wear)
                                  (inventory)
                                  :prompt-text "Enter a different item"
                                  :error-text  "WEAR isn't valid")
                                 ((not (typep inventory 'clothing))
                                  (inventory)
                                  :prompt-text "Enter a different item"
                                  :error-text (format nil "That ~a isn't something you can wear" (name-of inventory))))
                     (cond ((and
                             (typep selected-user '(not potty-trained-team-member))
                             (typep inventory 'pullup)
                             (typep wear 'diaper)
                             (list-length-> 2 (filter-items (wear-of selected-user) 'diaper)))
                            (format t "Does ~a look ready for pullups to you?~%" (name-of selected-user))
                            (return-from yadfa-bin:change))
                           ((and
                             (typep selected-user '(not potty-trained-team-member))
                             (not (typep inventory 'diaper))
                             (typep wear 'diaper)
                             (list-length-> 2 (filter-items (wear-of selected-user) 'diaper)))
                            (format t "letting ~a go without padding is a really bad idea. Don't do it.~%" (name-of selected-user))
                            (return-from yadfa-bin:change))
                           ((let ((wear (typecase (must-wear*-of (get-zone (position-of (player-of *game*))))
                                          (cons (must-wear*-of (get-zone (position-of (player-of *game*)))))
                                          (symbol (gethash (must-wear*-of *game*) (must-wear*-of (get-zone (position-of (player-of *game*))))))))
                                  (not-wear (typecase (must-not-wear*-of (get-zone (position-of (player-of *game*))))
                                              (cons (must-not-wear*-of (get-zone (position-of (player-of *game*)))))
                                              (symbol (gethash (must-not-wear*-of *game*) (must-not-wear*-of (get-zone (position-of (player-of *game*)))))))))
                              (or (and (not (typep inventory (car wear)))
                                       (typep wear (car wear))
                                       (list-length->= 1 (filter-items (wear-of selected-user) (car wear)))
                                       (not (funcall (coerce (cdr not-wear) 'function) selected-user)))
                                  (and (typep inventory (car not-wear)) (not (funcall (coerce (cdr not-wear) 'function) selected-user)))))
                            (return-from yadfa-bin:change))
                           ((and
                             (iter (for i in (butlast (wear-of selected-user) (- (list-length (wear-of selected-user)) (position wear (wear-of selected-user)) 1)))
                                   (when (and (typep i 'closed-bottoms) (lockedp i))
                                     (format t "~a can't remove ~a ~a to put on ~a ~a as it's locked~%"
                                             (name-of selected-user)
                                             (if (malep selected-user) "his" "her")
                                             (name-of i)
                                             (if (malep selected-user) "his" "her")
                                             (name-of inventory))
                                     (leave t))))
                            (return-from yadfa-bin:change)))
                     (setf a (substitute inventory wear (wear-of selected-user) :count 1)
                           i (iter (for outer in (reverse (subseq a 0 (1+ (position inventory a)))))
                                   (with b = (reverse a))
                                   (when (and (typep outer 'bottoms) (thickness-capacity-of outer) (> (fast-thickness b outer) (thickness-capacity-of outer)))
                                     (leave outer))))
                     (if i
                         (format t
                                 "~a struggles to fit ~a ~a over ~a ~a in a hilarious fashion but fail to do so.~%"
                                 (name-of selected-user)
                                 (if (malep selected-user) "his" "her")
                                 (name-of i)
                                 (if (malep selected-user) "his" "her")
                                 (name-of inventory))
                         (progn (when *battle*
                                  (format t "The ~a you're battling stops and waits for you to put on your ~a because Pouar never prevented this function from being called in battle~%"
                                          (if (list-length-< 1 (enemies-of *battle*)) "enemies" "enemy")
                                          (name-of inventory)))
                                (format t "~a changes out of ~a ~a and into ~a ~a~%"
                                        (name-of selected-user)
                                        (if (malep selected-user) "his" "her")
                                        (name-of wear)
                                        (if (malep selected-user) "his" "her")
                                        (name-of inventory))
                                (substitutef (inventory-of selected-user) wear inventory :count 1)
                                (setf (wear-of selected-user) a)))))
(defunassert yadfa-bin:toss (&rest items)
  (items list)
  "Throw an item in your inventory away. @var{ITEM} is the index of the item in your inventory"
  (let ((value (iter (for i in items)
                     (unless (typep i 'unsigned-byte)
                       (leave i)))))
    (when value
      (error 'type-error :datum value :expected-type 'unsigned-byte)))
  (let ((items (sort (remove-duplicates items) #'<)))
    (setf items (iter (generate i in items)
                      (for j in (inventory-of (player-of *game*)))
                      (for (the fixnum k) upfrom 0)
                      (when (first-iteration-p)
                        (next i))
                      (when (= k i)
                        (collect j)
                        (next i))))
    (unless items
      (format t "Those items aren't valid")
      (return-from yadfa-bin:toss))
    (iter (for i in items)
          (unless (tossablep i)
            (format t "To avoid breaking the game, you can't toss your ~a." (name-of i))
            (return-from yadfa-bin:toss)))
    (iter (for i in items)
          (format t "You send ~a straight to /dev/null~%" (name-of i)))
    (a:deletef (inventory-of (player-of *game*)) items
               :test (lambda (o e)
                       (s:memq e o)))))
(defunassert yadfa-bin:wield (&key user inventory)
  (user (or unsigned-byte null)
        inventory (or unsigned-byte type-specifier))
  "Wield an item. Set @var{INVENTORY} to the index or a type specifier of an item in your inventory to wield that item. Set @var{USER} to the index of an ally to have them to equip it or leave it @code{NIL} for the player."
  (let* ((selected-user (if user
                            (nth user (allies-of *game*))
                            (player-of *game*)))
         (item (typecase inventory
                 (unsigned-byte
                  (nth inventory (inventory-of (player-of *game*))))
                 ((or list (and symbol (not keyword)))
                  (find inventory (inventory-of (player-of *game*))
                        :test #'(lambda (type-specifier obj)
                                  (typep obj type-specifier)))))))
    (cond ((not item)
           (format t "INVENTORY isn't valid~%")
           (return-from yadfa-bin:wield)))
    (when *battle*
      (format t "The ~a you're battling stops and waits for you to equip your ~a because Pouar never prevented this function from being called in battle~%"
              (if (list-length-< 1 (enemies-of *battle*)) "enemies" "enemy")
              (name-of item)))
    (format t "~a equips his ~a ~a~%"
            (name-of selected-user)
            (if (malep selected-user) "his" "her")
            (name-of item))
    (a:deletef (inventory-of (player-of *game*)) item :count 1)
    (when (wield-of selected-user)
      (push (wield-of selected-user) (inventory-of (player-of *game*))))
    (setf (wield-of selected-user) item)))
(defunassert yadfa-bin:unwield (&key user)
  (user (or integer null))
  "Unwield an item. Set @var{USER} to the index of an ally to have them to unequip it or leave it @code{NIL} for the player."
  (let ((selected-user
          (if user
              (nth user (allies-of *game*))
              (player-of *game*))))
    (if (wield-of selected-user)
        (progn (push (wield-of selected-user)
                     (inventory-of (player-of *game*)))
               (setf (wield-of selected-user) nil))
        (format t "~a hasn't equiped a weapon~%" (name-of selected-user)))))
(defunassert yadfa-bin:pokedex (&optional enemy)
  (enemy symbol)
  "Browse enemies in your pokedex, @var{ENEMY} is a quoted symbol that is the same as the class name of the enemy you want to view. Leave it to @code{NIL} to list available entries"
  (if enemy
      (let ((a (if (s:memq enemy (seen-enemies-of *game*))
                   (make-instance enemy)
                   (progn (write-line "That enemy isn't in your pokedex")
                          (return-from yadfa-bin:pokedex)))))
        (format t "Name: ~a~%Species: ~a~%Description: ~a~%" (name-of a) (species-of a) (description-of a)))
      (progn (format t "~30a~30a~%" "ID" "Name")
             (iter (for i in (seen-enemies-of *game*))
                   (let ((a (make-instance i)))
                     (format t "~30a~30a~%" i (name-of a)))))))
(defunassert yadfa-bin:toggle-lock (wear key &optional user)
  (wear unsigned-byte
        key unsigned-byte
        user (or unsigned-byte null))
  "Toggle the lock on one of the clothes a user is wearing. @var{WEAR} is the index of an item a user is wearing, @var{KEY} is the index of a key in your inventory, @var{USER} is a number that is the index of an ally, leave this to @code{NIL} to select the player."
  (let* ((selected-user (if user (nth user (allies-of *game*)) (player-of *game*)))
         (wear-length (list-length (wear-of selected-user)))
         (inventory-length (list-length (inventory-of (player-of *game*))))
         (selected-key (nth key (inventory-of (player-of *game*))))
         (selected-wear (nth wear (wear-of selected-user))))
    (cond ((not selected-user)
           (format t "You only have ~d allies~%" (list-length (allies-of *game*))))
          ((not (>= wear-length wear))
           (format t "~a is only wearing ~d items~%" (name-of selected-user) wear-length))
          ((not (>= inventory-length key))
           (format t "You only have ~d items in your inventory~%" inventory-length))
          ((not (typep selected-key (key-of selected-wear)))
           (write-line "That doesn't go with that"))
          ((lockedp selected-wear)
           (format t "~a's ~a is now unlocked~%" (name-of selected-user) (name-of selected-wear))
           (setf (lockedp selected-wear) nil))
          ((typep selected-wear 'closed-bottoms)
           (write-line "That can't be locked"))
          (t
           (format t "~a's ~a is now locked~%" (name-of selected-user) (name-of selected-wear))
           (setf (lockedp selected-wear) t)))))
