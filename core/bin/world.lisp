;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa"; coding: utf-8-unix; -*-
(in-package :yadfa)
(defunassert yadfa-world:save-game (path)
  (path (or simple-string pathname))
  #.(format nil "This function saves current game to @var{PATH}

~a."
            (xref yadfa-world:load-game :function))
  (ensure-directories-exist (make-pathname :host (pathname-host path) :device (pathname-device path) :directory (pathname-directory path)))
  (a:with-output-to-file (s path :if-exists :supersede :external-format :utf-8)
    (write-string (write-to-string (ms:marshal *game*)) s))
  (typecase path
    (logical-pathname (translate-logical-pathname path))
    (pathname path)
    (simple-string (handler-case (translate-logical-pathname path)
                     (type-error () (parse-namestring path))
                     (file-error () nil)))))
(defunassert yadfa-world:load-game (path)
  (path (or simple-string pathname))
  #.(format nil "This function loads a saved game from @var{PATH}

~a."
            (xref yadfa-world:save-game :function))
  (a:with-input-from-file (stream path)
    (setf *game* (ms:unmarshal (read stream))))
  (typecase path
    (logical-pathname (translate-logical-pathname path))
    (pathname path)
    (simple-string (handler-case (translate-logical-pathname path)
                     (type-error () (parse-namestring path))
                     (file-error () nil)))))
(defunassert yadfa-world:move (&rest directions)
  (directions list)
  #.(format nil "type in the direction as a keyword to move in that direction, valid directions can be found with @code{(lst :directions t)}.
You can also specify multiple directions, for example @code{(move :south :south)} will move 2 zones south. @code{(move :south :west :south)} will move south, then west, then south.

~a."
            (xref yadfa-bin:lst :function))
  (iter (for direction in directions)
        (multiple-value-bind (new-position error) (get-path-end (get-destination direction (position-of (player-of *game*))) (position-of (player-of *game*)) direction)
          (let* ((old-position (position-of (player-of *game*))))
            (unless new-position
              (format t "~a" error)
              (return-from yadfa-world:move))
            (move-to-zone new-position :direction direction :old-position old-position)))))
(defunassert yadfa-world:place-prop (prop indicator)
  (prop (or unsigned-byte type-specifier) indicator symbol)
  (let ((position (position-of (player-of *game*))))
    (handle-user-input ((selected-prop (s:dispatch-case ((prop (or unsigned-byte type-specifier)))
                                                        ((unsigned-byte) (nth prop (inventory-of (player-of *game*))))
                                                        ((type-specifier) (find prop (inventory-of (player-of *game*)) :test (lambda (o e)
                                                                                                                               (typep e o)))))))
                       (*query-io* ((typep selected-prop '(not placable-prop))
                                    (prop)
                                    :prompt-text "Enter a different prop, either as a type specifier or an index of your inventory"
                                    :error-text "Either that prop doesn't exist or it isn't placable")
                                   ((or (not (eq (symbol-package indicator) (find-package :yadfa-user))) (getf (props-of (get-zone position)) indicator))
                                    (indicator)
                                    :prompt-text "Enter the property indicator for the prop you want to place"
                                    :error-text "Either there's already a prop there or you picked a symbol that's not in the YADFA-USER package as the property indicator"))
                       (a:deletef (inventory-of (player-of *game*)) selected-prop :count 1 :test 'eq)
                       (setf (getf (props-of (get-zone position)) indicator) selected-prop)
                       selected-prop)))
(defunassert yadfa-world:take-prop (indicator)
  (indicator symbol)
  (let ((position (position-of (player-of *game*))))
    (handle-user-input ((selected-prop (getf (props-of (get-zone position)) indicator)))
                       (*query-io* ((and #-sbcl (symbolp indicator) (typep selected-prop '(not placable-prop)))
                                    (indicator)
                                    :prompt-text "Enter the property indicator for the prop you want to take"
                                    :error-text "Either there's already a prop there, the prop you picked isn't a YADFA:PLACABLE-PROP or the property indicator you picked is not a symbol"))
                       (remf (props-of (get-zone position)) indicator)
                       (push selected-prop (inventory-of (player-of *game*)))
                       selected-prop)))
(defunassert yadfa-world:interact (prop &rest keys &key list take action describe-action describe &allow-other-keys)
  (action (or keyword null)
          describe-action (or keyword null)
          prop symbol
          describe boolean
          take (or null keyword list))
  #.(format nil "interacts with @var{PROP}. @var{PROP} is a keyword, you can get these with @code{LST} with the @var{PROPS} parameter. setting @var{LIST} to non-NIL will list all the items and actions in the prop. you can take the items with the @var{TAKE} parameter. Setting this to an integer will take the item at that index, while setting it to @code{:ALL} will take all the items, setting it to @code{:BITCOINS} will take just the bitcoins. You can get this index with the @var{LIST} parameter. @var{ACTION} is a keyword referring to an action to perform, can also be found with the @var{LIST} parameter. You can also specify other keys when using @var{ACTION} and this function will pass those keys to that function. set @var{DESCRIBE-ACTION} to the keyword of the action to find out how to use it. Set @var{DESCRIBE} to @code{T} to print the prop's description.

~a."
            (xref yadfa-bin:lst :function))
  (when (typep take 'list) (loop for i in take do (check-type i unsigned-byte)))
  (flet ((format-table (header &rest body)
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
                                                                                  (t (write cell))))))))))
    (when list
      (with-effective-frame
          (format t "Bitcoins: ~a~%~%" (get-bitcoins-from-prop prop (position-of (player-of *game*))))
        (apply #'format-table '("Index" "Name" "Class")
               (iter (for i in (get-items-from-prop prop (position-of (player-of *game*))))
                     (for (the fixnum j) upfrom 0)
                     (collect (list j (name-of i) (type-of i)))))
        (format t "~%~%Actions: ")
        (iter (for (key value) on (actions-of (getf (get-props-from-zone (position-of (player-of *game*))) prop)) by #'cddr)
              (when value
                (format t "~s " key)
                (finally (write-char #\Newline))))))
    (when take
      (cond ((eq take :all)
             (setf (inventory-of (player-of *game*)) (append* (get-items-from-prop prop (position-of (player-of *game*))) (inventory-of (player-of *game*))))
             (setf (get-items-from-prop prop (position-of (player-of *game*))) '())
             (incf (bitcoins-of (player-of *game*)) (get-bitcoins-from-prop prop (position-of (player-of *game*))))
             (setf (get-bitcoins-from-prop prop (position-of (player-of *game*))) 0))
            ((eq take :bitcoins)
             (incf (bitcoins-of (player-of *game*)) (get-bitcoins-from-prop prop (position-of (player-of *game*))))
             (setf (get-bitcoins-from-prop prop (position-of (player-of *game*))) 0))
            (t
             (iter (for i in take)
                   (push (nth i (get-items-from-prop prop (position-of (player-of *game*)))) (inventory-of (player-of *game*))))
             (iter (for i in (sort (copy-tree take) #'>))
                   (setf (get-items-from-prop prop (position-of (player-of *game*))) (remove-nth i (get-items-from-prop prop (position-of (player-of *game*)))))))))
    (when action
      (apply (coerce (action-lambda (getf-action-from-prop (position-of (player-of *game*)) prop action))
                     'function)
             (getf (get-props-from-zone (position-of (player-of *game*))) prop)
             :allow-other-keys t keys))
    (when describe-action
      (format t "Keyword: ~a~%~%Other Parameters: ~w~%~%Documentation: ~a~%~%Describe: ~a~%~%"
              describe-action
              (rest (lambda-list (action-lambda (getf-action-from-prop (position-of (player-of *game*)) prop describe-action))))
              (documentation (getf (actions-of (getf (get-props-from-zone (position-of (player-of *game*))) prop)) describe-action) t)
              (with-output-to-string (s)
                (let ((*standard-output* s))
                  (describe (action-lambda (getf-action-from-prop (position-of (player-of *game*)) prop describe-action)))))))
    (when describe
      (format t "~a~%" (description-of (getf (get-props-from-zone (position-of (player-of *game*))) prop))))))
(defunassert yadfa-world:stats (&optional user)
  (user (or unsigned-byte boolean))
  "Prints the current stats, essentially this game's equivalent of a health and energy bar in battle. Set @var{USER} to the index of an ally to show that ally's stats or set it to @code{T} to show your stats, leave it at @code{NIL} to show everyone's stats"
  (cond ((eq user t)
         (present-stats (player-of *game*)))
        (user
         (present-stats (nth user (allies-of *game*))))
        (t
         (iter (for i in (cons (player-of *game*) (allies-of *game*)))
               (present-stats i)))))
(defunassert yadfa-world:go-potty (&key prop wet mess pull-pants-down user)
  (user (or null real)
        prop (or null keyword)
        wet (or boolean real)
        mess (or boolean real))
  "Go potty. @var{PROP} is a keyword identifying the prop you want to use. If it's a toilet, use the toilet like a big boy. if it's not. Go potty on it like an animal. If you want to wet yourself, leave @var{PROP} as @code{NIL}. @var{WET} is the amount you want to pee in ml. @var{MESS} is the amount in cg, set @var{WET} and/or @var{MESS} to @code{T} to empty yourself completely. set @var{PULL-PANTS-DOWN} to non-NIL to pull your pants down first. @var{USER} is the index value of an ALLY you have. Set this to @code{NIL} if you're referring to yourself"
  (let ((this-prop (getf (get-props-from-zone (position-of (player-of *game*))) prop))
        (selected-user (if user
                           (nth user (allies-of *game*))
                           (player-of *game*))))
    (when (and prop (not this-prop))
      (format t "that PROP doesn't exist in this zone~%")
      (return-from yadfa-world:go-potty))
    (typecase this-prop
      (yadfa-props:toilet
       (potty-on-toilet this-prop
                        :wet (if user t wet)
                        :mess (if user t mess)
                        :pants-down pull-pants-down
                        :user selected-user))
      (t
       (potty-on-self-or-prop this-prop
                              :wet (if user t wet)
                              :mess (if user t mess)
                              :pants-down pull-pants-down
                              :user selected-user)))))
(defunassert yadfa-world:tickle (ally)
  (ally unsigned-byte)
  "Tickle an ally. @var{ALLY} is an integer that is the index of you allies"
  (when (list-length-> ally (allies-of *game*))
    (write-line "That ally doesn't exist")
    (return-from yadfa-world:tickle))
  (let ((selected-ally (nth ally (allies-of *game*))))
    (cond ((getf (attributes-of selected-ally) :not-ticklish)
           (format t "~a isn't ticklish"
                   (name-of selected-ally)))
          ((>= (bladder/contents-of selected-ally) (bladder/potty-dance-limit-of selected-ally))
           (format t "~a: Gah! No! Stop! *falls over and laughs while thrashing about then uncontrollably floods ~aself like an infant*~%~%*~a stops tickling*~%~%~a: Looks like the baby wet ~aself~%~%*~a slowly stands up while still wetting ~aself and grumbles*~%~%"
                   (name-of selected-ally)
                   (if (malep selected-ally) "him" "her")
                   (name-of (player-of *game*))
                   (name-of (player-of *game*))
                   (if (malep selected-ally) "him" "her")
                   (name-of selected-ally)
                   (if (malep selected-ally) "him" "her"))
           (wet :wetter selected-ally))
          ((and (>= (bladder/contents-of selected-ally) (bladder/need-to-potty-limit-of selected-ally)) (= (random 5) 0))
           (format t "~a: Gah! No! Stop! *falls over and laughs while thrashing about for about 30 seconds then uncontrollably floods ~aself like an infant*~%~%*~a stops tickling*~%~%~a: Looks like the baby wet ~aself~%~%*~a slowly stands up while still wetting ~aself and grumbles*~%~%"
                   (name-of selected-ally)
                   (if (malep selected-ally) "him" "her")
                   (name-of (player-of *game*))
                   (name-of (player-of *game*))
                   (if (malep selected-ally) "him" "her")
                   (name-of selected-ally)
                   (if (malep selected-ally) "him" "her"))
           (wet :wetter selected-ally))
          (t
           (format t "~a: Gah! No! Stop! *falls over and laughs while thrashing about for a few minutes until you get bored and stop*~%~%*~a slowly stands up exhausted from the tickling and grumbles*~%~%"
                   (name-of selected-ally)
                   (name-of selected-ally))))))
(defunassert yadfa-world:wash-all-in (&optional prop)
  (prop (or keyword null))
  "washes your dirty diapers and all the clothes you've ruined. @var{PROP} is a keyword identifying the washer you want to put it in. If you're washing it in a body of water, leave @var{PROP} out."
  (cond
    ((and prop (not (typep (getf (get-props-from-zone (position-of (player-of *game*))) prop) 'yadfa-props:washer)))
     (write-line "That's not a washer"))
    ((and (not prop) (not (underwaterp (get-zone (position-of (player-of *game*)))))) (format t "There's no where to wash that~%"))
    ((underwaterp (get-zone (position-of (player-of *game*))))
     (wash (inventory-of (player-of *game*)))
     (write-line "You washed all your soggy and messy clothing. Try not to wet and mess them next time"))
    (t (wash-in-washer (getf (get-props-from-zone (position-of (player-of *game*))) prop)))))
(defunassert yadfa-world:place (prop &rest items)
  (items list
         prop symbol)
  "Store items in a prop. @var{ITEMS} is a list of indexes of the items in your inventory. @var{PROP} is a keyword"
  (let ((value (iter (for i in items)
                     (unless (typep i 'integer)
                       (leave i)))))
    (when value
      (error 'type-error :datum value :expected-type 'integer)))
  (iter (for i in items) (check-type i integer))
  (unless (getf (get-props-from-zone (position-of (player-of *game*))) prop)
    (write-line "That prop doesn't exist")
    (return-from yadfa-world:place))
  (unless (placeablep (getf (get-props-from-zone (position-of (player-of *game*))) prop))
    (write-line "To avoid breaking the game, you can't place that item here.")
    (return-from yadfa-world:place))
  (let ((items (sort (remove-duplicates items) #'<)))
    (setf items (iter (generate i in items)
                      (for j in (player-of *game*))
                      (for (the fixnum k) upfrom 0)
                      (when (first-iteration-p)
                        (next i))
                      (when (= k i)
                        (collect j)
                        (next i))))
    (unless items
      (format t "Those items aren't valid")
      (return-from yadfa-world:place))
    (iter (for i in items)
          (format t "You place your ~a on the ~a~%" (name-of i) (name-of (getf (get-props-from-zone (position-of (player-of *game*))) prop)))
          (push i (get-items-from-prop prop (position-of (player-of *game*)))))
    (a:deletef (inventory-of (player-of *game*)) items
               :test (lambda (o e)
                       (s:memq e o)))))
(defunassert yadfa-world:use-item (item &rest keys &key user action &allow-other-keys)
  (item (or unsigned-byte type-specifier)
        action (or null keyword)
        user (or null unsigned-byte))
  "Uses an item. @var{ITEM} is an index of an item in your inventory. @var{USER} is an index of an ally. Setting this to @code{NIL} will use it on yourself. @var{ACTION} is a keyword when specified will perform a special action with the item, all the other keys specified in this function will be passed to that action. @var{ACTION} doesn't work in battle."
  (declare (ignorable action))
  (handle-user-input ((selected-item (typecase item
                                       (unsigned-byte
                                        (nth item (inventory-of (player-of *game*))))
                                       (type-specifier
                                        (find item (inventory-of (player-of *game*))
                                              :test #'(lambda (type-specifier obj)
                                                        (typep obj type-specifier))))))
                      (allies-length (list-length (allies-of *game*))))
                     (*query-io* ((null selected-item)
                                  (item)
                                  :prompt-text "Enter a different item"
                                  :error-text (format nil "You only have ~a items" (length (inventory-of (player-of *game*)))))
                                 ((and user (< allies-length user))
                                  (user)
                                  :prompt-text "Enter a different user"
                                  :error-text (format nil "You only have ~d allies" allies-length)))
                     (let ((this-user (if user (nth user (allies-of *game*)) (player-of *game*))))
                       (handler-case (progn
                                       (multiple-value-bind (cant-use plist) (apply 'cant-use-p selected-item (player-of *game*) this-user (getf keys :action) keys)
                                         (when cant-use
                                           (destructuring-bind (&key format-control format-arguments &allow-other-keys) plist
                                             (if format-control
                                                 (apply 'format t format-control format-arguments)
                                                 (write-line "You can't do that with that item"))
                                             (fresh-line)
                                             (return-from yadfa-world:use-item))))
                                       (let ((ret (apply #'use-item% selected-item (player-of *game*)
                                                         :target this-user
                                                         keys)))
                                         (incf (time-of *game*))
                                         (process-potty)
                                         (iter (for i in (allies-of *game*))
                                               (process-potty i))
                                         ret))
                         (unusable-item (c)
                           (princ c))))))
(defunassert yadfa-world:reload (ammo-type &optional user)
  (ammo-type (and type-specifier (not null))
             user (or unsigned-byte null))
  (let* ((user (if user
                   (nth user (allies-of *game*))
                   (player-of *game*)))
         (user-name (name-of user))
         (weapon (wield-of user))
         (weapon-ammo-type (ammo-type-of weapon))
         (weapon-capacity (ammo-capacity-of weapon))
         (weapon-name (name-of weapon))
         (reload-count (reload-count-of weapon))
         (player (player-of *game*)))
    (unless (wield-of user)
      (format t "~a isn't carrying a weapon~%" user-name)
      (return-from yadfa-world:reload))
    (unless (and weapon-ammo-type (> weapon-capacity 0))
      (format t "~a's ~a doesn't take ammo~%" user-name weapon-name)
      (return-from yadfa-world:reload))
    (when (list-length-<= weapon-capacity (ammo-of (wield-of user)))
      (format t "~a's ~a is already full~%" user-name weapon-name)
      (return-from yadfa-world:reload))
    (unless (subtypep ammo-type weapon-ammo-type)
      (format t "~a ~a doesn't take that ammo~%" user-name weapon-name)
      (return-from yadfa-world:reload))
    (unless (iter (for i in (inventory-of player))
                  (when (typep i ammo-type)
                    (leave t)))
      (format t "~a doesn't have that ammo~%" user-name)
      (return-from yadfa-world:reload))
    (format t "~a reloaded ~a ~a" user-name (if (malep user) "his" "her") weapon-name)
    (iter (with count = 0)
          (for item in (inventory-of player))
          (when (or (list-length-<= weapon-capacity (ammo-of weapon))
                    (and reload-count (>= count reload-count)))
            (leave t))
          (when (and (typep item ammo-type) (typep item weapon-ammo-type))
            (push item (ammo-of weapon))
            (a:deletef item (inventory-of player) :count 1)))))
(defunassert yadfa-world:add-ally-to-team (ally-index)
  (ally-index unsigned-byte)
  "Adds an ally to your battle team. @var{ALLY-INDEX} is the index of an ally in your list of allies"
  (let ((allies-length (list-length (allies-of *game*))))
    (if (< allies-length ally-index)
        (format t "You only have ~d allies~%" allies-length)
        (let* ((ally (nth ally-index (allies-of *game*)))
               (old (car (team-of *game*)))
               (new (car (pushnew ally (team-of *game*) :test 'eq))))
          (if (eq old new)
              (format t "~a is already on the battle team~%" (name-of ally))
              (format t "~a has joined the battle team~%" (name-of ally)))))))
(defunassert yadfa-world:remove-ally-from-team (team-index)
  (team-index unsigned-byte)
  "Removes an ally to your battle team. @var{TEAM-INDEX} is the index of an ally in your battle team list"
  (let ((team-length (list-length (team-of *game*))))
    (cond
      ((>= team-index team-length)
       (format t "You only have ~d members in your team~%" team-length)
       (return-from yadfa-world:remove-ally-from-team))
      ((eq (nth team-index (team-of *game*)) (player-of *game*))
       (write-line "You can't remove the player from the team")
       (return-from yadfa-world:remove-ally-from-team))
      (t (setf (team-of *game*) (remove-nth team-index (team-of *game*)))))))
(defunassert yadfa-world:swap-team-member (team-index-1 team-index-2)
  (team-index-1 unsigned-byte
                team-index-2 unsigned-byte)
  "swap the positions of 2 battle team members. @var{TEAM-INDEX-1} and @var{TEAM-INDEX-2} are the index numbers of these members in your battle team list"
  (cond ((or (list-length-> team-index-1 (team-of *game*)) (list-length-> team-index-2 (team-of *game*)))
         (format t "You only have ~d members in your team~%" (list-length (team-of *game*)))
         (return-from yadfa-world:swap-team-member))
        ((= team-index-1 team-index-2)
         (write-line "Those refer to the same team member")
         (return-from yadfa-world:swap-team-member))
        (t (rotatef (nth team-index-1 (team-of *game*)) (nth team-index-2 (team-of *game*))))))
(defunassert yadfa-world:fart (&optional user)
    (user (or unsigned-byte type-specifier))
  (handle-user-input ((selected-user (typecase user
                                       (null (player-of *game*))
                                       (unsigned-byte (nth user (allies-of *game*)))
                                       (type-specifier (find user (cons (player-of *game*) (allies-of *game*))
                                                             :test (lambda (o e)
                                                                     (typep e o)))))))
      (*query-io* ((null selected-user)
                   (user)
                   :prompt-text "Enter a different user"
                   :error-text (typecase user
                                 (unsigned-byte (f:fmt nil "You only have " (length (allies-of *game*)) " allies"))
                                 (type-specifier "you don't have that ally"))))
    (multiple-value-bind (result mess) (fart user)
      (fart-result-text selected-user result mess))))
