;;;;this file contains functions the player can enter in the REPL

(in-package :yadfa)
(defun yadfa-bin:get-inventory-of-type (type)
    #+sbcl (declare
               (type type-specifier type))
    (check-type type type-specifier)
    (get-positions-of-type type (inventory-of (player-of *game*))))
(defun yadfa-bin:reload-files (&rest keys &key compiler-verbose &allow-other-keys)
    "Intended for developers. Use this to recompile the game without having to close it. Accepts the same keyword arguments as asdf:load-system and asdf:operate. Set COMPILER-VERBOSE to T to print the compiling messages. setting LOAD-SOURCE t T will avoid creating fasls"
    (let ((*compile-verbose* compiler-verbose) (*compile-print* compiler-verbose))
        (apply #'asdf:load-system :yadfa :allow-other-keys t keys)
        (apply #'load-mods :allow-other-keys t keys)))
(defun yadfa-bin:enable-mod (system)
    "Enable a mod, the modding system is mostly just asdf, SYSTEM is a keyword which is the name of the system you want to enable"
    (declare (ignorable system))
    #+yadfa-mods
    (if (asdf:find-system system nil)
        (progn
            (pushnew (asdf:coerce-name system) *mods* :test #'string=)
            (with-open-file (stream (uiop:xdg-config-home "yadfa/mods.conf")
                                :if-does-not-exist :create
                                :if-exists :supersede
                                :direction :output)
                (write *mods* :stream stream))
            (asdf:load-system system))
        (write-line "That system doesn't exist"))
    #-yadfa-mods (write-line "Mod support is not enabled for this build"))
(defun yadfa-bin:disable-mod (system)
    "Disable a mod, the modding system is mostly just asdf, SYSTEM is a keyword which is the name of the system you want to enable"
    (declare (ignorable system))
    #+yadfa-mods (progn
                     (removef *mods* (asdf:coerce-name system) :test #'string=)
                     (with-open-file (stream (uiop:xdg-config-home "yadfa/mods.conf")
                                         :if-does-not-exist :create
                                         :if-exists :supersede
                                         :direction :output)
                         (write *mods* :stream stream)))
    #-yadfa-mods (write-line "Mod support is not enabled for this build"))
(defun yadfa-world:save-game (path)
    "This function saves current game to PATH"
    #+sbcl (declare (type (or simple-string pathname) path))
    (check-type path (or simple-string pathname))
    (with-open-file (s path :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-string (write-to-string (ms:marshal *game*)) s)))
(defun yadfa-world:load-game (path)
    "This function loads a saved game from PATH"
    #+sbcl (declare (type (or simple-string pathname) path))
    (check-type path (or simple-string pathname))
    (with-open-file (stream path)
        (setf *game* (ms:unmarshal (read stream)))))
(defun yadfa-bin:toggle-onesie (&key wear user)
    "Open or closes your onesie. WEAR is the index of a onesie. Leave NIL for the outermost onesie. USER is the index of an ally. Leave NIL to refer to yourself"
    #+sbcl (declare (type (or unsigned-byte null) user wear))
    (check-type user (or unsigned-byte null))
    (check-type wear (or unsigned-byte null))
    (cond (user
              (cond ((>= user (list-length (allies-of *game*)))
                        (format t "You only have ~d allies~%~%" (list-length (allies-of *game*))))
                  (wear (toggle-onesie%
                            (nth wear (wear-of (nth user (allies-of *game*))))
                            (nthcdr (1+ wear) (wear-of (nth user (allies-of *game*))))
                            (nth user (allies-of *game*))))
                  (t (iter
                         (for i in (wear-of (nth user (allies-of *game*))))
                         (with j = 1)
                         (when (typep i 'onesie) (leave (toggle-onesie% i (nthcdr j (wear-of (nth user (allies-of *game*)))) (nth user (allies-of *game*)))))
                         (incf j) (finally (write-line "You're not wearing a onesie")))))))
    (if wear
        (toggle-onesie%
            (nth wear (wear-of (player-of *game*)))
            (nthcdr (1+ wear) (wear-of (player-of *game*)))
            (player-of *game*))
        (iter
            (for i in (wear-of (player-of *game*)))
            (with j = 1)
            (when (typep i 'onesie) (leave (toggle-onesie% i (nthcdr j (wear-of (player-of *game*))) (player-of *game*))))
            (incf j) (finally (write-line "You're not wearing a onesie")))))
(defun yadfa-world:move (&rest directions)
    "type in the direction as a keyword to move in that direction, valid directions can be found with `(lst :directions t)'. you can also specify multiple directions, for example `(move :south :south)' will move 2 zones south. `(move :south :west :south)' will move south, then west, then south."
    #+sbcl (declare (type list directions))
    (check-type directions list)
    (iter (for direction in directions)
        (let* ((new-position (get-path-end
                                 (get-destination direction (position-of (player-of *game*)))
                                 (position-of (player-of *game*))
                                 direction))
                  (old-position (position-of (player-of *game*))))
            (unless (get-path-end
                        (get-destination direction (position-of (player-of *game*)))
                        (position-of (player-of *game*))
                        direction)
                (format t "~a"
                    (second
                        (multiple-value-list
                            (get-path-end
                                (get-destination direction (position-of (player-of *game*)))
                                (position-of (player-of *game*))
                                direction))))
                (return-from yadfa-world:move))
            (when
                (iter (for i in (cons (player-of *game*) (allies-of *game*)))
                    (when
                        (or
                            (and
                                (< (list-length (wearingp (wear-of i)
                                                    (car
                                                        (must-wear-of
                                                            (get-zone
                                                                new-position)))))
                                    1)
                                (not (funcall
                                         (coerce
                                             (cdr
                                                 (must-wear-of
                                                     (get-zone
                                                         new-position)))
                                             'function)
                                         i)))
                            (and
                                (> (list-length (wearingp (wear-of i)
                                                    (car
                                                        (must-not-wear-of
                                                            (get-zone
                                                                new-position)))))
                                    0)
                                (not (funcall
                                         (coerce
                                             (cdr
                                                 (must-not-wear-of
                                                     (get-zone
                                                         new-position)))
                                             'function)
                                         i))))
                        (leave t)))
                (return-from yadfa-world:move))
            (when (or
                      (and
                          (not (eq (lockedp (get-zone new-position)) :nil))
                          (not (member
                                   (lockedp (get-zone new-position))
                                   (inventory-of (player-of *game*))
                                   :test (lambda (item key)
                                             (typep key item)))))
                      (and
                          (not (eq
                                   (getf (getf (direction-attributes-of
                                                   (get-zone (position-of (player-of *game*))))
                                             direction)
                                       :locked :nil)
                                   :nil))
                          (not (member
                                   (getf (getf (direction-attributes-of
                                                   (get-zone (position-of (player-of *game*))))
                                             direction)
                                       :locked :nil)
                                   (inventory-of (player-of *game*))
                                   :test (lambda (item key)
                                             (typep key item))))))
                (write-line "That zone is locked and you don't have a key")
                (return-from yadfa-world:move))
            (when (or
                      (and
                          (not (eq (lockedp (get-zone new-position)) :nil))
                          (member
                              (lockedp (get-zone new-position))
                              (inventory-of (player-of *game*))
                              :test (lambda (item key)
                                        (typep key item))))
                      (and
                          (not (eq
                                   (getf (getf (direction-attributes-of
                                                   (get-zone (position-of (player-of *game*))))
                                             direction)
                                       :locked :nil)
                                   :nil))
                          (member
                              (getf (getf (direction-attributes-of
                                              (get-zone (position-of (player-of *game*))))
                                        direction)
                                  :locked :nil)
                              (inventory-of (player-of *game*))
                              :test (lambda (item key)
                                        (typep key item)))))
                (format t "You unlock zone ~a~%" new-position))
            (unless (eq (lockedp (get-zone new-position)) :nil)
                (setf (lockedp (get-zone new-position)) :nil))
            (unless (eq (getf-direction (position-of (player-of *game*)) direction :locked) :nil)
                (setf-direction (position-of (player-of *game*)) direction :locked :nil))
            (setf (position-of (player-of *game*))
                new-position)
            (when (underwaterp (get-zone (position-of (player-of *game*)))) (swell-up-all))
            (process-potty)
            (run-equip-effects (player-of *game*))
            (loop for i in (allies-of *game*) do
                (process-potty i)
                (run-equip-effects i))
            (print-enter-text (position-of (player-of *game*)) old-position direction)
            (cond ((continue-battle-of (get-zone (position-of (player-of *game*))))
                      (set-new-battle (getf (continue-battle-of (get-zone (position-of (player-of *game*)))) :enemies)
                          :win-events (getf (continue-battle-of (get-zone (position-of (player-of *game*)))) :win-events)
                          :continuable t
                          :enter-battle-text (getf (continue-battle-of (get-zone (position-of (player-of *game*)))) :enter-battle-text))
                      (return-from yadfa-world:move))
                ((iter (for i in (events-of (get-zone (position-of (player-of *game*)))))
                     (when (trigger-event i)
                         (collect i)))
                    (return-from yadfa-world:move))
                ((enemy-spawn-list-of (get-zone (position-of (player-of *game*))))
                    (iter
                        (for i in (enemy-spawn-list-of (get-zone (position-of (player-of *game*)))))
                        (let ((random (if (getf i :random) (getf i :random) 1)))
                            (when (< (random (getf i :max-random)) random)
                                (set-new-battle (cond
                                                    ((getf i :eval)
                                                        (eval (getf i :eval)))
                                                    ((getf i :lambda)
                                                        (funcall (coerce (getf i :lambda) 'function)))
                                                    (t (getf i :enemies))))
                                (return-from yadfa-world:move)))))))))
(defun yadfa-bin:lst (&key inventory inventory-group props wear user directions moves position map descriptions describe-zone)
    "used to list various objects and properties, INVENTORY takes a type specifier for the items you want to list in your inventory. setting INVENTORY to T will list all the items. INVENTORY-GROUP is similar to INVENTORY, but will group the items by class name. WEAR is similar to INVENTORY but lists clothes you're wearing instead. setting DIRECTIONS to non-NIL will list the directions you can walk.setting MOVES to non-NIL will list the moves you know. setting USER to T will cause MOVES and WEAR to apply to the player, setting it to an integer will cause it to apply it to an ally. Leaving it at NIL will cause it to apply to everyone. setting POSITION to true will print your current position. Setting MAP to a number will print the map with the floor number set to MAP, setting MAP to T will print the map of the current floor you're on. When printing the map in McCLIM, red means there's a warp point, dark green is the zone with the player, blue means there are stairs. These 3 colors will blend with each other to make the final color"
    #+sbcl (declare
               (type (or unsigned-byte boolean) user)
               (type (or boolean integer) map)
               (type type-specifier inventory))
    (check-type user (or unsigned-byte boolean))
    (check-type map (or boolean integer))
    (check-type inventory type-specifier)
    (when (and (typep user 'unsigned-byte) (>= user (list-length (allies-of *game*))))
        (format t "You only have ~d allies~%" (list-length (allies-of *game*)))
        (return-from yadfa-bin:lst))
    (when inventory
        (format t "~7a~30a~6a~8a~6a~8a~%" "Index" "Name" "Wet" "Wetcap" "Mess" "Messcap")
        (let ((j 0)) (iter (for i in (inventory-of (player-of *game*)))
                         (when (typep i inventory)
                             (format t "~7a~30a~6a~8a~6a~8a~%" j
                                 (name-of i)
                                 (if (typep i 'closed-bottoms) (sogginess-of i) nil)
                                 (if (typep i 'closed-bottoms) (sogginess-capacity-of i) nil)
                                 (if (typep i 'closed-bottoms) (messiness-of i) nil)
                                 (if (typep i 'closed-bottoms) (messiness-capacity-of i) nil)))
                         (incf j))
            (format t "~%")))
    (when describe-zone
        (format t "~a~%"
            (get-zone-text (description-of
                               (cond
                                   ((typep describe-zone 'zone)
                                       describe-zone)
                                   ((typep describe-zone 'list)
                                       (get-zone describe-zone))
                                   (t (position-of (player-of *game*))))))))
    (when inventory-group
        (let ((a ()))
            (iter (for i in (inventory-of (player-of *game*)))
                (when (typep i inventory-group)
                    (if (getf a (class-name (class-of i)))
                        (incf (second
                                  (getf a (class-name (class-of i)))))
                        (setf
                            (getf a (class-name (class-of i)))
                            (list
                                (name-of (make-instance (class-name (class-of i))))
                                1)))))
            (format t "~30a~40a~10a" "Class Name" "Name" "Quantity")
            (iter (for (key value) on a by #'cddr)
                (apply #'format t "~30a~40a~10a" key value))))
    (when wear
        (cond
            ((not user)
                (format t "Number of items listed: ~a~%" (iter
                                                             (with j = 0)
                                                             (for i in (wear-of (player-of *game*)))
                                                             (when (typep i wear) (incf j))
                                                             (finally (return j))))
                (format t "~a:~%~%" (name-of (player-of *game*)))
                (format t "~7a~40a~6a~8a~6a~8a~%" "Index" "Name" "Wet" "Wetcap" "Mess" "Messcap")
                (let ((j 0)) (loop for i in (wear-of (player-of *game*)) do
                                 (when (typep i wear)
                                     (format t "~7a~40a~6a~8a~6a~8a~%" j
                                         (name-of i)
                                         (if (typep i 'closed-bottoms) (sogginess-of i) nil)
                                         (if (typep i 'closed-bottoms) (sogginess-capacity-of i) nil)
                                         (if (typep i 'closed-bottoms) (messiness-of i) nil)
                                         (if (typep i 'closed-bottoms) (messiness-capacity-of i) nil)))
                                 (incf j))
                    (format t "~%"))
                (loop for k in (allies-of *game*) do
                    (format t "~a:~%~%" (name-of k))
                    (format t "~7a~40a~6a~8a~6a~8a~%" "Index" "Name" "Wet" "Wetcap" "Mess" "Messcap")
                    (let ((j 0)) (loop for i in (wear-of k) do
                                     (when (typep i wear)
                                         (format t "~7a~40a~6a~8a~6a~8a~%" j
                                             (name-of i)
                                             (if (typep i 'closed-bottoms) (sogginess-of i) nil)
                                             (if (typep i 'closed-bottoms) (sogginess-capacity-of i) nil)
                                             (if (typep i 'closed-bottoms) (messiness-of i) nil)
                                             (if (typep i 'closed-bottoms) (messiness-capacity-of i) nil)))
                                     (incf j))
                        (write-char #\Newline))))
            ((typep user 'integer)
                (when (>= user (list-length (allies-of *game*)))
                    (format t "You only have ~d allies~%~%" (list-length (allies-of *game*)))
                    (return-from yadfa-bin:lst))
                (format t "Number of items listed: ~a~%" (iter
                                                             (with j = 0)
                                                             (for i in (wear-of (nth user (allies-of *game*))))
                                                             (when (typep i wear) (incf j))
                                                             (finally (return j))))
                (format t "~a:~%~%" (name-of (nth user (allies-of *game*))))
                (format t "~7a~40a~6a~8a~6a~8a~%" "Index" "Name" "Wet" "Wetcap" "Mess" "Messcap")
                (let ((j 0)) (loop for i in (wear-of (nth user (allies-of *game*))) do
                                 (when (typep i wear)
                                     (format t "~7a~40a~6a~8a~6a~8a~%" j
                                         (name-of i)
                                         (if (typep i 'closed-bottoms) (sogginess-of i) nil)
                                         (if (typep i 'closed-bottoms) (sogginess-capacity-of i) nil)
                                         (if (typep i 'closed-bottoms) (messiness-of i) nil)
                                         (if (typep i 'closed-bottoms) (messiness-capacity-of i) nil)))
                                 (incf j))
                    (write-char #\Newline)))
            (t
                (format t "Number of items listed: ~a~%" (iter
                                                             (with j = 0)
                                                             (for i in (wear-of (player-of *game*)))
                                                             (when (typep i wear) (incf j))
                                                             (finally (return j))))
                (format t "~a:~%~%" (name-of (player-of *game*)))
                (format t "~7a~40a~6a~8a~6a~8a~%" "Index" "Name" "Wet" "Wetcap" "Mess" "Messcap")
                (let ((j 0)) (loop for i in (wear-of (player-of *game*)) do
                                 (when (typep i wear)
                                     (format t "~7a~40a~6a~8a~6a~8a~%" j
                                         (name-of i)
                                         (if (typep i 'closed-bottoms) (sogginess-of i) nil)
                                         (if (typep i 'closed-bottoms) (sogginess-capacity-of i) nil)
                                         (if (typep i 'closed-bottoms) (messiness-of i) nil)
                                         (if (typep i 'closed-bottoms) (messiness-capacity-of i) nil)))
                                 (incf j))
                    (write-char #\Newline)))))
    (when moves
        (cond
            ((typep user 'number)
                (format t "~a:~%~%" (name-of (nth user (allies-of *game*))))
                (format t "~30a~20a~40a~%" "Symbol" "Name" "Description")
                (iter
                    (for i in (moves-of (nth user (allies-of *game*))))
                    (when i (format t "~30s~20a~40a~%" (class-name (class-of i)) (name-of i) (description-of i)))))
            ((not user)
                (format t "~a:~%~%" (name-of (player-of *game*)))
                (format t "~30a~20a~40a~%" "Symbol" "Name" "Description")
                (iter
                    (for i in (moves-of (player-of *game*)))
                    (when i (format t "~30s~20a~40a~%" (class-name (class-of i)) (name-of i) (description-of i))))
                (write-char #\Newline)
                (loop for k in (allies-of *game*) do
                    (format t "~a:~%~%" (name-of k))
                    (format t "~30a~20a~40a~%" "Symbol" "Name" "Description")
                    (iter
                        (for i in (moves-of k))
                        (when i (format t "~30s~20a~40a~%" (class-name (class-of i)) (name-of i) (description-of i))))
                    (write-char #\Newline)))
            (t
                (format t "~a:~%~%" (name-of (player-of *game*)))
                (format t "~30a~20a~40a~%" "Symbol" "Name" "Description")
                (iter
                    (for i in (moves-of (player-of *game*)))
                    (when i (format t "~30s~20a~40a~%" (class-name (class-of i)) (name-of i) (description-of i)))))))
    (when props (format t "~20a~30a~%" "Keyword" "Object")
        (iter (for (a b) on (get-props-from-zone (position-of (player-of *game*))) by #'cddr)
            (when b (format t ":~20a~30a~%" a (name-of b)))))
    (flet ((z (delta direction)
               (when (and
                         (get-zone
                             (append
                                 (mapcar #'+ (butlast (position-of (player-of *game*))) delta)
                                 (last (position-of (player-of *game*)))))
                         (not
                             (hiddenp
                                 (get-zone
                                     (append
                                         (mapcar #'+ (butlast (position-of (player-of *game*))) delta)
                                         (last (position-of (player-of *game*))))))))
                   (format t
                       "~s ~a~%"
                       direction
                       (name-of
                           (get-zone
                               (append
                                   (mapcar #'+ (butlast (position-of (player-of *game*))) delta)
                                   (last (position-of (player-of *game*))))))))))
        (when directions
            (z (list 1 0 0) :east)
            (z (list -1 0 0) :west)
            (z (list 0 -1 0) :north)
            (z (list 0 1 0) :south)
            (z (list 0 0 1) :up)
            (z (list 0 0 -1) :down)
            (when (warp-points-of (get-zone (position-of (player-of *game*))))
                (iter
                    (for (a b) on (warp-points-of (get-zone (position-of (player-of *game*)))) by #'cddr)
                    (when (and (get-zone b) (not (hiddenp (get-zone b)))) (format t "~s ~a~%" a (name-of (get-zone b))))))))
    (when position
        (format t "Your current position is ~s~%" (position-of (player-of *game*))))
    (when map
        (cond ((eq map t)
                  (print-map t))
            (t (print-map (list
                              (first (position-of (player-of *game*)))
                              (second (position-of (player-of *game*)))
                              map
                              (fourth (position-of (player-of *game*))))))))
    (when descriptions
        (cond
            ((eq user t)
                (format t "Name: ~a~%Species: ~a~%Description: ~a~%~%"
                    (name-of (player-of *game*))
                    (species-of (player-of *game*))
                    (description-of (player-of *game*))))
            ((typep user 'unsigned-byte)
                (format t "Name: ~a~%Species: ~a~%Description: ~a~%~%"
                    (name-of (nth user (allies-of *game*)))
                    (species-of (nth user (allies-of *game*)))
                    (description-of (nth user (allies-of *game*)))))
            (t
                (format t "Name: ~a~%Species: ~a~%Description: ~a~%~%"
                    (name-of (player-of *game*))
                    (species-of (player-of *game*))
                    (description-of (player-of *game*)))
                (iter (for i in (allies-of *game*))
                    (format t "Name: ~a~%Species: ~a~%Description: ~a~%~%"
                        (name-of i)
                        (species-of i)
                        (description-of i)))))))
(defun yadfa-bin:get-stats (&key inventory wear prop item attack ally wield)
    "lists stats about various items in various places. INVENTORY is the index of an item in your inventory. WEAR is the index of what you or your ally is wearing. PROP is a keyword that refers to the prop you're selecting. ITEM is the index of an item that a prop has and is used to print information about that prop. ATTACK is a keyword reffering to the move you or your ally has when showing that move. ALLY is the index of an ally on your team when selecting INVENTORY or MOVE, don't set ALLY if you want to select yourself."
    #+sbcl (declare
               (type (or null unsigned-byte) ally wear inventory)
               (type (or null keyword) prop))
    (check-type ally (or null unsigned-byte))
    (check-type wear (or null unsigned-byte))
    (check-type inventory (or null unsigned-byte))
    (check-type prop (or null keyword))
    (when (and ally (>= ally (list-length (allies-of *game*))))
        (write-line "That ally doesn't exist")
        (return-from yadfa-bin:get-stats))
    (let ((selected-user (if ally (nth ally (allies-of *game*)) (player-of *game*))))
        (when wield
            (let ((i (wield-of selected-user)))
                (format t
                    "Name: ~a~%Description:~%~a~%Resale Value: ~f~%"
                    (name-of i)
                    (description-of i)
                    (/ (value-of i) 2))
                (when (typep i 'closed-bottoms)
                    (iter (for (a b) on (wet-text-of i))
                        (when (>= (sogginess-of i) a)
                            (leave (format t "~%~a~%" b))))
                    (iter (for (a b) on (mess-text-of i))
                        (when (>= (messiness-of i) a)
                            (leave (format t "~%~a~%" b))))
                    (format t "Sogginess: ~a~%Sogginess Capacity: ~a~%Messiness: ~a~%Messiness Capacity: ~a~%"
                        (sogginess-of i)
                        (sogginess-capacity-of i)
                        (messiness-of i)
                        (messiness-capacity-of i)))
                (when (ammo-type-of i)
                    (format t "Ammo Type: ~s" (ammo-type-of i)))
                (when (special-actions-of i)
                    (iter (for (a b) on i by #'cddr)
                        (format t "Keyword: ~a~%Other Parameters: ~w~%Documentation: ~a~%~%Describe: ~a~%~%"
                            a
                            (cddr (lambda-list (action-lambda b)))
                            (action-documentation b)
                            (with-output-to-string (s)
                                (let ((*standard-output* s))
                                    (describe (action-lambda b)))))))))
        (when inventory
            (let ((i (nth inventory (inventory-of selected-user))))
                (format t
                    "Name: ~a~%Description:~%~a~%Resale Value: ~f~%"
                    (name-of i)
                    (description-of i)
                    (/ (value-of i) 2))
                (when (typep i 'closed-bottoms)
                    (iter (for (a b) on (wet-text-of i))
                        (when (>= (sogginess-of i) a)
                            (leave (format t "~%~a~%" b))))
                    (iter (for (a b) on (mess-text-of i))
                        (when (>= (messiness-of i) a)
                            (leave (format t "~%~a~%" b))))
                    (format t "Sogginess: ~a~%Sogginess Capacity: ~a~%Messiness: ~a~%Messiness Capacity: ~a~%"
                        (sogginess-of i)
                        (sogginess-capacity-of i)
                        (messiness-of i)
                        (messiness-capacity-of i)))
                (when (ammo-type-of i)
                    (format t "Ammo Type: ~s" (ammo-type-of i)))
                (when (special-actions-of i)
                    (iter (for (a b) on i by #'cddr)
                        (format t "Keyword: ~a~%~%Documentation: ~a~%~%Describe: ~a~%~%"
                            a
                            (action-documentation b)
                            (with-output-to-string (s)
                                (let ((*standard-output* s))
                                    (describe (action-lambda b)))))))))
        (when wear
            (let* ((i (nth wear (wear-of selected-user)))
                      (k (nthcdr (1+ wear) (wear-of selected-user)))
                      (l nil))
                (format t "Name: ~a~%Description:~% ~a~%"
                    (name-of i)
                    (format nil "~a"
                        (with-output-to-string (s)
                            (format s "~a~%" (description-of i))
                            (cond
                                ((typep i 'bottoms)
                                    (setf l
                                        (when k
                                            (iter
                                                (for (a b) on (bulge-text-of i) by #'cddr)
                                                (when (>= (total-thickness k) a) (leave b)))))
                                    (when l (format s " ~a~%" l))))
                            (when (typep i 'closed-bottoms)
                                (setf l (iter
                                            (for (a b) on (wear-wet-text-of i))
                                            (when (>= (sogginess-of i) a)
                                                (leave b))))
                                (when l (format s " ~a~%" l))
                                (setf l (iter
                                            (for (a b) on (wear-mess-text-of i))
                                            (when (>= (messiness-of i) a)
                                                (leave b))))
                                (when l (format s " ~a~%" l))))))
                (when (typep i 'closed-bottoms)
                    (format t "Sogginess: ~f~%Sogginess Capacity: ~f~%Messiness: ~f~%Messiness Capacity: ~f~%"
                        (sogginess-of i)
                        (sogginess-capacity-of i)
                        (messiness-of i)
                        (messiness-capacity-of i)))
                (when (special-actions-of i)
                    (iter (for (a b) on i by #'cddr)
                        (format t "Keyword: ~a~%~%Documentation: ~a~%~%Describe: ~a~%~%"
                            a
                            (action-documentation b)
                            (with-output-to-string (s)
                                (let ((*standard-output* s))
                                    (describe (action-lambda b)))))))))
        (when attack
            (format t "Name:~a~%Description~a~%Energy Cost: ~f~%~%"
                (name-of (get-move attack selected-user))
                (description-of (get-move attack selected-user))
                (energy-cost-of (get-move attack selected-user))))
        (when prop
            (let ((j (getf (get-props-from-zone (position-of (player-of *game*))) prop)))
                (when item
                    (let ((i (nth item (items-of j))))
                        (format t "Name: ~a~%Description:~%~a~%" (name-of i) (description-of i))
                        (when (typep i 'closed-bottoms)
                            (iter (for (a b) on (wet-text-of i))
                                (when (>= (sogginess-of i) a)
                                    (leave (format t "~%~a~%" b))))
                            (iter (for (a b) on (mess-text-of i))
                                (when (>= (messiness-of i) a)
                                    (leave (format t "~%~a~%" b))))
                            (format t "Sogginess: ~a~%Sogginess Capacity: ~a~%Messiness: ~a~%Messiness Capacity: ~a~%"
                                (sogginess-of i)
                                (sogginess-capacity-of i)
                                (messiness-of i)
                                (messiness-capacity-of i)))))))))
(defun yadfa-world:interact (prop &rest keys &key list take action describe-action describe &allow-other-keys)
    "interacts with PROP. PROP is a keyword, you can get these with `lst' with the PROPS parameter. setting LIST to non-NIL will list all the items and actions in the prop. you can take the items with the TAKE parameter. Setting this to an integer will take the item at that index, while setting it to :ALL will take all the items, setting it to :BITCOINS will take just the bitcoins. You can get this index with the LIST parameter. ACTION is a keyword referring to an action to perform, can also be found with the LIST parameter. You can also specify other keys when using ACTION and this function will pass those keys to that function. set DESCRIBE-ACTION to the keyword of the action to find out how to use it. Set DESCRIBE to T to print the prop's description."
    #+sbcl (declare
               (type (or keyword null) action describe-action)
               (type symbol prop)
               (type list describe)
               (type (or null keyword list) take))
    (check-type action (or keyword null))
    (check-type describe-action (or keyword null))
    (check-type prop symbol)
    (check-type describe list)
    (check-type take (or null keyword list))
    (when (typep take 'list) (loop for i in take do (check-type i unsigned-byte)))
    (when list
        (format t "Bitcoins: ~a~%~%" (get-bitcoins-from-prop prop (position-of (player-of *game*))))
        (format t "~7a~30a~%" "Index" "Object")
        (let ((j (list-length (items-of (getf (get-props-from-zone (position-of (player-of *game*))) prop)))))
            (loop for i from 0 to (- j 1) do (format t "~7a~30a~%" i (name-of (nth i (get-items-from-prop prop (position-of (player-of *game*))))))))
        (format t "~%~%Actions: ")
        (iter (for (key value) on (actions-of (getf (get-props-from-zone (position-of (player-of *game*))) prop)) by #'cddr)
            (when value
                (format t "~s "
                    key)
                (finally (write-char #\Newline)))))
    (when take
        (cond
            ((eq take :all)
                (setf (inventory-of (player-of *game*)) (append (get-items-from-prop prop (position-of (player-of *game*))) (inventory-of (player-of *game*))))
                (setf (get-items-from-prop prop (position-of (player-of *game*))) '())
                (incf (bitcoins-of (player-of *game*)) (get-bitcoins-from-prop prop (position-of (player-of *game*))))
                (setf (get-bitcoins-from-prop prop (position-of (player-of *game*))) 0))
            ((eq take :bitcoins)
                (incf (bitcoins-of (player-of *game*)) (get-bitcoins-from-prop prop (position-of (player-of *game*))))
                (setf (get-bitcoins-from-prop prop (position-of (player-of *game*))) 0))
            (t
                (loop for i in take do
                    (push (nth i (get-items-from-prop prop (position-of (player-of *game*)))) (inventory-of (player-of *game*))))
                (loop for i in (sort (copy-tree take) #'>) do
                    (setf (get-items-from-prop prop (position-of (player-of *game*))) (remove-nth i (get-items-from-prop prop (position-of (player-of *game*)))))))))
    (when action (apply (coerce (action-lambda (getf-action-from-prop (position-of (player-of *game*)) prop action)) 'function) (getf (get-props-from-zone (position-of (player-of *game*))) prop) :allow-other-keys t keys))
    (when describe-action
        (format t "Keyword: ~a~%~%Other Parameters: ~w~%~%Documentation: ~a~%~%Describe: ~a~%~%"
            describe-action
            (rest (lambda-list (action-lambda (getf-action-from-prop (position-of (player-of *game*)) prop describe-action))))
            (action-documentation (getf (actions-of (getf (get-props-from-zone (position-of (player-of *game*))) prop)) describe-action))
            (with-output-to-string (s)
                (let ((*standard-output* s))
                    (describe (action-lambda (getf-action-from-prop (position-of (player-of *game*)) prop describe-action)))))))
    (when describe
        (format t "~a~%" (description-of (getf (get-props-from-zone (position-of (player-of *game*))) prop)))))
(defun yadfa-bin:wear (&key (inventory 0) (wear 0) user)
    "Wear an item in your inventory. WEAR is the index you want to place this item. Smaller index refers to outer clothing. INVENTORY is an index in your inventory of the item you want to wear. You can also give it a type specifier which will pick the first item in your inventory of that type. USER is an index of an ally. Leave this at NIL to refer to yourself."
    #+sbcl (declare
               (type (or null unsigned-byte) user)
               (type unsigned-byte wear)
               (type (or type-specifier unsigned-byte) wear inventory))
    (check-type user (or null unsigned-byte))
    (check-type wear unsigned-byte)
    (check-type inventory (or type-specifier unsigned-byte))
    (let* ((selected-user
               (if
                   user
                   (nth user (allies-of *game*))
                   (player-of *game*)))
              (item (cond
                        ((typep inventory 'unsigned-byte)
                            (nth inventory (inventory-of (player-of *game*))))
                        ((typep inventory 'type-specifier)
                            (find inventory (inventory-of (player-of *game*))
                                :test #'(lambda (type-specifier obj)
                                            (typep obj type-specifier))))))
              (i nil)
              (a ()))
        (cond ((not item)
                  (format t "INVENTORY isn't a valid item ~%")
                  (return-from yadfa-bin:wear))
            ((> wear (list-length (wear-of selected-user)))
                (format t
                    "`:WEAR ~d' doesn't refer to a valid position as it can't go past the items you're current wearing which is currently ~d"
                    wear
                    (list-length (wear-of selected-user)))
                (return-from yadfa-bin:wear))
            ((not (typep item 'clothing))
                (format t "That ~a isn't something you can wear~%" (name-of item))
                (return-from yadfa-bin:wear))
            ((and
                 (typep item
                     (car
                         (must-not-wear*-of
                             (get-zone (position-of (player-of *game*))))))
                 (not
                     (funcall
                         (coerce
                             (cdr (must-not-wear*-of (get-zone (position-of (player-of *game*)))))
                             'function)
                         selected-user)))
                (format t "~a isn't allowed to wear those here.~%" (name-of selected-user))
                (return-from yadfa-bin:wear))
            ((and
                 (> wear 0)
                 (iter
                     (for i in (butlast (wear-of selected-user) (- (list-length (wear-of selected-user)) wear)))
                     (when (and
                               (typep i 'closed-bottoms)
                               (not (eq (lockedp i) :nil)))
                         (format t "~a can't remove ~a ~a to put on ~a ~a as it's locked~%"
                             (name-of selected-user)
                             (if (malep selected-user) "his" "her")
                             (name-of i)
                             (if (malep selected-user) "his" "her")
                             (name-of item))
                         (leave t))))
                (return-from yadfa-bin:wear)))
        (setf
            a (insert (wear-of selected-user) item wear)
            i (iter
                  (for (inner outer) on (reverse a))
                  (when (and
                            (typep outer 'bottoms)
                            (not (eq (thickness-capacity-of outer) t))
                            (>
                                (total-thickness (member inner a))
                                (thickness-capacity-of outer)))
                      (leave (first (thickest-sort (member inner a)))))))
        (if i
            (format t
                "~a struggles to fit ~a ~a over ~a ~a in a hilarious fashion but fail to do so.~%"
                (name-of selected-user)
                (if (malep selected-user) "his" "her")
                (name-of item)
                (if (malep selected-user) "his" "her")
                (name-of i))
            (progn
                (when *battle* (format t
                                   "The ~a you're battling stops and waits for you to put on your ~a because Pouar never prevented this function from being called in battle~%"
                                   (if (> (list-length (enemies-of *battle*)) 1) "enemies" "enemy")
                                   (name-of item)))
                (format t "~a puts on ~a ~a~%"
                    (name-of selected-user)
                    (if (malep selected-user) "his" "her")
                    (name-of item))
                (removef (inventory-of (player-of *game*)) item :count 1)
                (setf (wear-of selected-user) a)))))
(defun yadfa-bin:unwear (&key (inventory 0) (wear 0) user)
    "Unwear an item you're wearing. Inventory is the index you want to place this item. WEAR is the index of the item you're wearing that you want to remove. You can also set WEAR to a type specifier for the outer most clothing of that type. USER is a integer referring to the index of an ally. Leave at NIL to refer to yourself"
    #+sbcl (declare
               (type (or unsigned-byte null) user)
               (type (or (and symbol (not keyword)) list unsigned-byte) wear)
               (type unsigned-byte inventory))
    (check-type user (or unsigned-byte null))
    (check-type inventory unsigned-byte)
    (check-type wear (or type-specifier unsigned-byte))
    (let* ((selected-user
               (if
                   user
                   (nth user (allies-of *game*))
                   (player-of *game*)))
              (item (cond
                        ((typep wear 'unsigned-byte)
                            (nth wear (wear-of (player-of *game*))))
                        ((typep wear 'type-specifier)
                            (find wear (wear-of (player-of *game*))
                                :test #'(lambda (type-specifier obj)
                                            (typep obj type-specifier)))))))
        (cond ((not item)
                  (format t "WEAR isn't a valid item ~%")
                  (return-from yadfa-bin:unwear))
            ((> inventory (list-length (inventory-of (player-of *game*))))
                (format t
                    "`:INVENTORY ~d' doesn't refer to a valid position as it can't go past the items you currently have in your inventory which is currently ~d~%"
                    inventory
                    (list-length (inventory-of (player-of *game*))))
                (return-from yadfa-bin:unwear))
            ((and
                 (not (eq (player-of *game*) selected-user))
                 (typep item 'tabbed-briefs)
                 (or (eq (potty-training-of user) :none) (eq (potty-training-of user) :rebel))
                 (< (wearingp (wear-of selected-user) 'tabbed-briefs) 2))
                (format t "Letting ~a go without padding is a really bad idea. Don't do it.~%"
                    (name-of selected-user))
                (return-from yadfa-bin:unwear))
            ((and
                 (typep item
                     (car
                         (must-wear*-of
                             (get-zone
                                 (position-of
                                     (player-of *game*))))))
                 (<= (list-length
                         (wearingp
                             (wear-of selected-user)
                             (car
                                 (must-wear*-of
                                     (get-zone
                                         (position-of
                                             (player-of *game*)))))))
                     1)
                 (not (funcall
                          (coerce
                              (cdr
                                  (must-wear*-of
                                      (get-zone
                                          (position-of
                                              (player-of *game*)))))
                              'function)
                          selected-user)))
                (return-from yadfa-bin:unwear))
            ((iter
                 (for i in (butlast
                               (wear-of selected-user)
                               (-
                                   (list-length (wear-of selected-user))
                                   (position item (wear-of selected-user))
                                   1)))
                 (when (and
                           (typep i 'closed-bottoms)
                           (not (eq (lockedp i) :nil)))
                     (format t "~a can't remove ~a ~a to take off ~a ~a as it's locked~%"
                         (name-of selected-user)
                         (if (malep selected-user) "his" "her")
                         (name-of i)
                         (if (malep selected-user) "his" "her")
                         (name-of item))
                     (leave t)))
                (return-from yadfa-bin:unwear)))
        (when *battle* (format t
                           "The ~a you're battling stops and waits for you to take off your ~a because Pouar never prevented this function from being called in battle~%"
                           (if (> (list-length (enemies-of *battle*)) 1) "enemies" "enemy")
                           (name-of item)))
        (format t "~a takes off ~a ~a~%" (name-of selected-user) (if (malep selected-user) "his" "her") (name-of item))
        (removef (wear-of (player-of *game*)) item :count 1)
        (insertf (inventory-of (player-of *game*)) item inventory)))
(defun yadfa-bin:change (&key (inventory 0) (wear 0) user)
    "Change one of the clothes you're wearing with one in your inventory. WEAR is the index of the clothing you want to replace. Smaller index refers to outer clothing. INVENTORY is an index in your inventory of the item you want to replace it with. You can also give INVENTORY and WEAR a quoted symbol which can act as a type specifier which will pick the first item in your inventory of that type. USER is an index of an ally. Leave this at NIL to refer to yourself."
    #+sbcl (declare
               (type (or null unsigned-byte) user)
               (type (or type-specifier unsigned-byte) inventory wear))
    (check-type user (or null unsigned-byte))
    (check-type inventory (or type-specifier unsigned-byte))
    (check-type wear (or type-specifier unsigned-byte))
    (let* ((selected-user
               (if
                   user
                   (nth user (allies-of *game*))
                   (player-of *game*)))
              (inventory (cond
                             ((typep inventory 'unsigned-byte)
                                 (nth inventory (inventory-of (player-of *game*))))
                             ((typep inventory 'type-specifier)
                                 (find inventory (inventory-of (player-of *game*))
                                     :test #'(lambda (type-specifier obj)
                                                 (typep obj type-specifier))))))
              (wear (cond
                        ((typep wear 'unsigned-byte)
                            (nth wear (wear-of (player-of *game*))))
                        ((typep wear 'type-specifier)
                            (find wear (wear-of (player-of *game*))
                                :test #'(lambda (type-specifier obj)
                                            (typep obj type-specifier))))))
              (i nil)
              (a nil))
        (cond ((not inventory)
                  (write-line "INVENTORY isn't valid")
                  (return-from yadfa-bin:change))
            ((not wear)
                (write-line "WEAR isn't valid")
                (return-from yadfa-bin:change))
            ((< (list-length (wear-of selected-user)) 1)
                (format t "~a isn't wearing any clothes to change~%" (name-of selected-user)))
            ((not (typep inventory 'clothing))
                (format t "That ~a isn't something you can wear~%" (name-of inventory))
                (return-from yadfa-bin:change))
            ((and
                 (not (eq (player-of *game*) selected-user))
                 (or (eq (potty-training-of user) :none) (eq (potty-training-of user) :rebel))
                 (typep inventory 'pullon)
                 (typep wear 'tabbed-briefs)
                 (< (list-length (wearingp (wear-of selected-user) 'tabbed-briefs)) 2))
                (format t "Does ~a look ready for pullups to you?~%" (name-of selected-user))
                (return-from yadfa-bin:change))
            ((and
                 (not (eq (player-of *game*) selected-user))
                 (or (eq (potty-training-of user) :none) (eq (potty-training-of user) :rebel))
                 (not (typep inventory 'tabbed-briefs))
                 (typep wear 'tabbed-briefs)
                 (< (list-length (wearingp (wear-of selected-user) 'tabbed-briefs)) 2))
                (format t "letting ~a go without padding is a really bad idea. Don't do it.~%" (name-of selected-user))
                (return-from yadfa-bin:change))
            ((or
                 (and
                     (not (typep inventory
                              (car
                                  (must-wear*-of
                                      (get-zone
                                          (position-of
                                              (player-of *game*)))))))
                     (typep wear
                         (car
                             (must-wear*-of
                                 (get-zone
                                     (position-of
                                         (player-of *game*))))))
                     (<= (list-length
                             (wearingp
                                 (wear-of selected-user)
                                 (car
                                     (must-wear*-of
                                         (get-zone
                                             (position-of
                                                 (player-of *game*)))))))
                         1)
                     (not (funcall
                              (coerce
                                  (cdr
                                      (must-wear*-of
                                          (get-zone
                                              (position-of
                                                  (player-of *game*)))))
                                  'function)
                              selected-user)))
                 (and
                     (typep inventory
                         (car
                             (must-not-wear*-of
                                 (get-zone
                                     (position-of
                                         (player-of *game*))))))
                     (not (funcall
                              (coerce
                                  (cdr
                                      (must-not-wear*-of
                                          (get-zone
                                              (position-of
                                                  (player-of *game*)))))
                                  'function)
                              selected-user))))
                (return-from yadfa-bin:change))
            ((and
                 (iter
                     (for i in (butlast
                                   (wear-of selected-user)
                                   (-
                                       (list-length (wear-of selected-user))
                                       (position wear (wear-of selected-user))
                                       1)))
                     (when (and
                               (typep i 'closed-bottoms)
                               (not (eq (lockedp i) :nil)))
                         (format t "~a can't remove ~a ~a to put on ~a ~a as it's locked~%"
                             (name-of selected-user)
                             (if (malep selected-user) "his" "her")
                             (name-of i)
                             (if (malep selected-user) "his" "her")
                             (name-of inventory))
                         (leave t))))
                (return-from yadfa-bin:change)))
        (setf
            a (substitute inventory wear (wear-of selected-user) :count 1)
            i (iter
                  (for (inner outer) on (reverse a))
                  (when (and
                            (typep outer 'bottoms)
                            (not (eq (thickness-capacity-of outer) t))
                            (>
                                (total-thickness (member inner a))
                                (thickness-capacity-of outer)))
                      (leave outer))))
        (if i
            (format t
                "~a struggles to fit ~a ~a over ~a ~a in a hilarious fashion but fail to do so.~%"
                (name-of selected-user)
                (if (malep selected-user) "his" "her")
                (name-of i)
                (if (malep selected-user) "his" "her")
                (name-of inventory))
            (progn
                (when *battle* (format t
                                   "The ~a you're battling stops and waits for you to put on your ~a because Pouar never prevented this function from being called in battle~%"
                                   (if (> (list-length (enemies-of *battle*)) 1) "enemies" "enemy")
                                   (name-of inventory)))
                (format t "~a changes out of ~a ~a and into ~a ~a~%"
                    (name-of selected-user)
                    (if (malep selected-user) "his" "her")
                    (name-of wear)
                    (if (malep selected-user) "his" "her")
                    (name-of inventory))
                (substitutef (inventory-of selected-user) wear inventory :count 1)
                (setf (wear-of selected-user) a)))))
(defun yadfa-battle:fight (attack &key target friendly-target)
    "Use a move on an enemy. ATTACK* is either a keyword which is the indicator to select an attack that you know, or T for default. USER is the index of a member in your team that you want to fight. TARGET is the index of the enemy you're attacking. FRIENDLY-TARGET is a member on your team you're using the move on instead. Only specify either a FRIENDLY-TARGET or TARGET. Setting both might make the game's code unhappy"
    #+sbcl (declare
               (type (or null unsigned-byte) target)
               (type (or symbol boolean) attack))
    (check-type target (or null unsigned-byte))
    (check-type attack (or symbol boolean))
    (process-battle :attack attack :target target :friendly-target friendly-target))
(defun yadfa-battle:stats (&key user enemy)
    "Prints the current stats in battle, essentially this game's equivelant of a health and energy bar in battle. USER is the index of the member in your team, ENEMY is the index of the enemy in battle. Set both to NIL to show the stats for everyone."
    #+sbcl (declare
               (type (or unsigned-byte null) user enemy))
    (check-type user (or unsigned-byte null))
    (check-type enemy (or unsigned-byte null))
    (cond (user
              (present-stats (nth user (team-of *game*))))
        (enemy
            (present-stats (nth enemy (enemies-of *battle*))))
        (t
            (format t "Your team:~%~%")
            (iter (for i in (team-of *game*))
                (present-stats i))
            (format t "Their team:~%~%")
            (iter (for i in (enemies-of *battle*))
                (present-stats i)))))
(defun yadfa-world:stats (&optional user)
    "Prints the current stats, essentially this game's equivelant of a health and energy bar in battle. Set USER to the index of an ally to show that ally's stats or set it to T to show your stats, leave it at NIL to show everyone's stats"
    #+sbcl (declare (type (or unsigned-byte boolean) user))
    (check-type user (or unsigned-byte boolean))
    (cond ((eq user t)
              (present-stats (player-of *game*)))
        (user
            (present-stats (nth user (allies-of *game*))))
        (t
            (iter (for i in (cons (player-of *game*) (allies-of *game*)))
                (present-stats i)))))
(defun yadfa-world:go-potty (&key prop wet mess pull-pants-down user)
    "Go potty. PROP is a keyword identifying the prop you want to use. If it's a toilet, use the toilet like a big boy. if it's not. Go potty on it like an animal. If you want to wet yourself, leave PROP as NIL. WET is the amount you want to pee in ml. MESS is the amount in cg, set WET and/or MESS to T to empty yourself completely. set PULL-PANTS-DOWN to non-NIL to pull your pants down first. USER is the index value of an ALLY you have. Set this to NIL if you're referring to yourself"
    #+sbcl (declare
               (type (or null number) user)
               (type (or null keyword) prop)
               (type (or boolean number) wet mess))
    (check-type user (or null number))
    (check-type prop (or null keyword))
    (check-type wet (or boolean number))
    (check-type mess (or boolean number))
    (let ((this-prop (getf (get-props-from-zone (position-of (player-of *game*))) prop))
             (selected-user (if user (nth user (allies-of *game*)) (player-of *game*))))
        (when (and prop (not this-prop))
            (format t "that PROP doesn't exist in this zone~%")
            (return-from yadfa-world:go-potty))
        (cond ((typep this-prop 'toilet)
                  (potty-on-toilet
                      this-prop
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
(defun yadfa-world:tickle (ally)
    "Tickle an ally. ALLY is an integer that is the index of you allies"
    #+sbcl (declare (type unsigned-byte ally))
    (check-type ally unsigned-byte)
    (when (>= ally (list-length (allies-of *game*)))
        (write-line "That ally doesn't exist")
        (return-from yadfa-world:tickle))
    (let ((selected-ally (nth ally (allies-of *game*))))
        (cond
            ((>= (bladder/contents-of selected-ally) (bladder/potty-dance-limit-of selected-ally))
                (format t
                    "~a: Gah! No! Stop! *falls over and laughs while thrashing about then uncontrollably floods ~aself like an infant*~%~%*~a stops tickling*~%~%~a: Looks like the baby wet ~aself~%~%*~a slowly stands up while still wetting ~aself and grumbles*~%~%"
                    (name-of selected-ally)
                    (if (malep selected-ally) "him" "her")
                    (name-of (player-of *game*))
                    (name-of (player-of *game*))
                    (if (malep selected-ally) "him" "her")
                    (name-of selected-ally)
                    (if (malep selected-ally) "him" "her"))
                (wet :wetter selected-ally))
            ((and (>= (bladder/contents-of selected-ally) (bladder/need-to-potty-limit-of selected-ally)) (= (random 5) 0))
                (format t
                    "~a: Gah! No! Stop! *falls over and laughs while thrashing about for about 30 seconds then uncontrollably floods ~aself like an infant*~%~%*~a stops tickling*~%~%~a: Looks like the baby wet ~aself~%~%*~a slowly stands up while still wetting ~aself and grumbles*~%~%"
                    (name-of selected-ally)
                    (if (malep selected-ally) "him" "her")
                    (name-of (player-of *game*))
                    (name-of (player-of *game*))
                    (if (malep selected-ally) "him" "her")
                    (name-of selected-ally)
                    (if (malep selected-ally) "him" "her"))
                (wet :wetter selected-ally))
            (t
                (format t
                    "~a: Gah! No! Stop! *falls over and laughs while thrashing about for a few minutes until you get bored and stop*~%~%*~a slowly stands up exhausted from the tickling and grumbles*~%~%"
                    (name-of selected-ally)
                    (name-of selected-ally))))))
(defun yadfa-world:wash-all-in (&optional prop)
    "washes your dirty diapers and all the clothes you've ruined. PROP is a keyword identifying the washer you want to put it in. If you're washing it in a body of water, leave PROP out."
    #+sbcl (declare (type (or keyword null) prop))
    (check-type prop (or keyword null))
    (cond
        ((and prop (not (typep (getf (get-props-from-zone (position-of (player-of *game*))) prop) 'washer)))
            (write-line "That's not a washer"))
        ((and (not prop) (not (underwaterp (get-zone (position-of (player-of *game*)))))) (format t "There's no where to wash that~%"))
        ((underwaterp (get-zone (position-of (player-of *game*))))
            (wash (inventory-of (player-of *game*)))
            (write-line "You washed all your soggy and messy clothing. Try not to wet and mess them next time"))
        (t (wash-in-washer (getf (get-props-from-zone (position-of (player-of *game*))) prop)))))
(defun yadfa-bin:toss (&rest items)
    "Throw an item in your inventory away. ITEM is the index of the item in your inventory"
    #+sbcl (declare (type list items))
    (check-type items list)
    (iter (for i in items) (check-type i unsigned-byte))
    (let ((items (sort (remove-duplicates items) #'<)))
        (setf items (iter
                        (generate i in items)
                        (for j in (inventory-of (player-of *game*)))
                        (for k upfrom 0)
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
                (format t "To avoid breaking the game, you can't toss your ~a."
                    (name-of i))
                (return-from yadfa-bin:toss)))
        (iter (for i in items)
            (format t "You send ~a straight to /dev/null~%" (name-of i)))
        (removef (inventory-of (player-of *game*)) items
            :test (lambda (o e)
                      (member e o)))))
(defun yadfa-world:place (prop &rest items)
    "Store items in a prop. ITEMS is a list of indexes of the items in your inventory. PROP is a keyword"
    #+sbcl (declare
               (type list items)
               (type symbol prop))
    (check-type items list)
    (check-type prop symbol)
    (iter (for i in items) (check-type i integer))
    (unless (getf (get-props-from-zone (position-of (player-of *game*))) prop)
        (write-line "That prop doesn't exist")
        (return-from yadfa-world:place))
    (unless (placeablep (getf (get-props-from-zone (position-of (player-of *game*))) prop))
        (write-line "To avoid breaking the game, you can't place that item here.")
        (return-from yadfa-world:place))
    (let ((items (sort (remove-duplicates items) #'<)))
        (setf items (iter
                        (generate i in items)
                        (for j in (player-of *game*))
                        (for k upfrom 0)
                        (when (first-iteration-p)
                            (next i))
                        (when (= k i)
                            (collect j)
                            (next i))))
        (unless items
            (format t "Those items aren't valid")
            (return-from yadfa-world:place))
        (iter (for i in items)
            (format t "You place your ~a on the ~a~%"
                (name-of i)
                (name-of (getf (get-props-from-zone (position-of (player-of *game*))) prop)))
            (push
                i
                (get-items-from-prop prop (position-of (player-of *game*)))))
        (removef (inventory-of (player-of *game*)) items
            :test (lambda (o e)
                      (member e o)))))
(defun yadfa-battle:run ()
    "Run away from a battle like a coward"
    (cond
        ((continue-battle-of (get-zone (position-of (player-of *game*))))
            (write-line "Can't run from this battle")
            (return-from yadfa-battle:run))
        ((and (>=
                  (bladder/contents-of (player-of *game*))
                  (bladder/need-to-potty-limit-of (player-of *game*)))
             (>=
                 (bowels/contents-of (player-of *game*))
                 (bowels/need-to-potty-limit-of (player-of *game*))))
            (format t
                "~a wet and messed ~aself in fear and ran away like a coward~%"
                (name-of (player-of *game*))
                (if (malep (player-of *game*))
                    "him"
                    "her"))
            (wet)
            (mess))
        ((>=
             (bladder/contents-of (player-of *game*))
             (bladder/need-to-potty-limit-of (player-of *game*)))
            (format t
                "~a wet ~aself in fear and ran away like a coward~%"
                (name-of (player-of *game*))
                (if (malep (player-of *game*))
                    "him"
                    "her"))
            (wet))
        ((>=
             (bowels/contents-of (player-of *game*))
             (bowels/need-to-potty-limit-of (player-of *game*)))
            (format t
                "~a messed ~aself in fear and ran away like a coward~%"
                (name-of (player-of *game*))
                (if (malep (player-of *game*))
                    "him"
                    "her"))
            (mess))
        (t
            (format t "~a ran away like a coward~%"
                (name-of (player-of *game*)))))
    (setf *battle* nil)
    (unuse-package :yadfa-battle :yadfa-user)
    (use-package :yadfa-world :yadfa-user))
(defun yadfa-world:use-item (item &rest keys &key user action &allow-other-keys)
    "Uses an item. Item is an index of an item in your inventory. USER is an index of an ally. Setting this to NIL will use it on yourself. ACTION is a keyword when specified will perform a special action with the item, all the other keys specified in this function will be passed to that action. ACTION doesn't work in battle."
    #+sbcl (declare
               (type (or unsigned-byte (and symbol (not keyword)) list) item)
               (type (or null keyword) action))
    (check-type item (or unsigned-byte (and symbol (not keyword)) list))
    (check-type action (or null keyword))
    (let ((selected-item (cond
                             ((typep item 'unsigned-byte)
                                 (nth item (inventory-of (player-of *game*))))
                             ((typep item '(or symbol list))
                                 (find item (inventory-of (player-of *game*))
                                     :test #'(lambda (type-specifier obj)
                                                 (typep obj type-specifier))))))
             (ret nil))
        (unless selected-item
            (format t "You don't have that item~%")
            (return-from yadfa-world:use-item))
        (when (and user (>= user (list-length (allies-of *game*))))
            (format t "You only have ~d allies~%" (list-length (allies-of *game*)))
            (return-from yadfa-world:use-item))
        (let ((this-user (if user
                             (nth user (allies-of *game*))
                             (player-of *game*))))
            (setf ret (apply #'use-item%
                          selected-item
                          (player-of *game*)
                          :target this-user
                          keys))
            (process-potty)
            (loop for i in (allies-of *game*) do (process-potty i))
            ret)))
(defun yadfa-battle:use-item (item &key target enemy-target)
    "Uses an item. Item is an index of an item in your inventory. TARGET is an index of your team. Setting this to 0 will use it on yourself. ENEMY-TARGET is an index of an enemy in battle if you're using it on an enemy in battle. Only specify either a TARGET or ENEMY-TARGET. Setting both might make the game's code unhappy"
    #+sbcl (declare
               (type (or unsigned-byte (and symbol (not keyword)) list) item)
               (type (or null unsigned-byte) target enemy-target))
    (check-type item (or unsigned-byte (and symbol (not keyword)) list))
    (check-type target (or null unsigned-byte))
    (check-type enemy-target (or null unsigned-byte))
    (let ((selected-item (cond
                             ((typep item 'unsigned-byte)
                                 (nth item (inventory-of (player-of *game*))))
                             ((typep item '(or symbol list))
                                 (find item (inventory-of (player-of *game*))
                                     :test #'(lambda (type-specifier obj)
                                                 (typep obj type-specifier)))))))
        (cond
            ((not item)
                (format t "You don't have that item~%")
                (return-from yadfa-battle:use-item))
            ((and target (>= target (list-length (team-of *game*))))
                (format t "You only have ~d team members~%" (list-length (team-of *game*)))
                (return-from yadfa-battle:use-item))
            ((and enemy-target (>= enemy-target (list-length (enemies-of *battle*))))
                (format t "You only have ~d allies~%" (list-length (enemies-of *battle*)))
                (return-from yadfa-battle:use-item))
            ((and target enemy-target)
                (format t "Only specify -TARGET or ENEMY-TARGET. Not both.")))
        (process-battle
            :item selected-item
            :friendly-target (cond
                                 (target target)
                                 (enemy-target nil)
                                 (t
                                     (position
                                         (first (turn-queue-of *battle*))
                                         (team-of *game*))))
            :target (cond
                        (enemy-target enemy-target)
                        (t nil)))))
(defun yadfa-battle:reload (ammo-type)
    #+sbcl (declare
               (type (and (or list (and symbol (not keyword))) (not null)) ammo-type))
    (check-type ammo-type (and (or list (and symbol (not keyword))) (not null)))
    (unless
        (wield-of (first (turn-queue-of *battle*)))
        (format t "~a isn't carrying a weapon~%"
            (name-of (first (turn-queue-of *battle*))))
        (return-from yadfa-battle:reload))
    (unless (and
                (ammo-type-of (wield-of (first (turn-queue-of *battle*))))
                (> (ammo-capacity-of (wield-of (first (turn-queue-of *battle*)))) 0))
        (format t "~a's ~a doesn't take ammo~%"
            (name-of (first (turn-queue-of *battle*)))
            (name-of (wield-of (first (turn-queue-of *battle*)))))
        (return-from yadfa-battle:reload))
    (when
        (>=
            (list-length (ammo-of (wield-of (first (turn-queue-of *battle*)))))
            (ammo-capacity-of (wield-of (first (turn-queue-of *battle*)))))
        (format t "~a's ~a is already full~%"
            (name-of (first (turn-queue-of *battle*)))
            (name-of (wield-of (first (turn-queue-of *battle*)))))
        (return-from yadfa-battle:reload))
    (unless
        (iter
            (for i in (inventory-of (player-of *game*)))
            (when (typep i ammo-type)
                (leave t)))
        (format t "~a doesn't have that ammo~%"
            (name-of (first (turn-queue-of *battle*))))
        (return-from yadfa-battle:reload))
    (unless
        (iter
            (for i in (inventory-of (player-of *game*)))
            (when (typep i (ammo-type-of
                               (wield-of
                                   (first (turn-queue-of *battle*)))))
                (leave t)))
        (format t "~a ~a doesn't take that ammo~%"
            (name-of (first (turn-queue-of *battle*)))
            (name-of (wield-of (first (turn-queue-of *battle*)))))
        (return-from yadfa-battle:reload))
    (process-battle :reload ammo-type))
(defun yadfa-world:reload (ammo-type &optional user)
    #+sbcl (declare
               (type (and (or list (and symbol (not keyword))) (not null)) ammo-type)
               (type (or unsigned-byte null) user))
    (check-type ammo-type (and (or list (and symbol (not keyword))) (not null)))
    (check-type user (or unsigned-byte null))
    (let ((user (if user
                    (nth user (allies-of *game*))
                    (player-of *game*))))
        (unless
            (wield-of user)
            (format t "~a isn't carrying a weapon~%"
                (name-of user))
            (return-from yadfa-world:reload))
        (unless (and
                    (ammo-type-of (wield-of user))
                    (> (ammo-capacity-of (wield-of user)) 0))
            (format t "~a's ~a doesn't take ammo~%"
                (name-of user)
                (name-of (wield-of user)))
            (return-from yadfa-world:reload))
        (when
            (>=
                (list-length (ammo-of (wield-of user)))
                (ammo-capacity-of (wield-of user)))
            (format t "~a's ~a is already full~%"
                (name-of user)
                (name-of (wield-of user)))
            (return-from yadfa-world:reload))
        (unless
            (iter
                (for i in (inventory-of (player-of *game*)))
                (when (typep i ammo-type)
                    (leave t)))
            (format t "~a doesn't have that ammo~%"
                (name-of user))
            (return-from yadfa-world:reload))
        (unless
            (iter
                (for i in (inventory-of (player-of *game*)))
                (when (typep i (ammo-type-of
                                   (wield-of
                                       user)))
                    (leave t)))
            (format t "~a ~a doesn't take that ammo~%"
                (name-of user)
                (name-of (wield-of user)))
            (return-from yadfa-world:reload))
        (format t "~a reloaded ~a ~a"
            (name-of user)
            (if (malep user)
                "his"
                "her")
            (name-of (wield-of user)))
        (iter
            (with count = 0)
            (for item in (inventory-of (player-of *game*)))
            (when (or
                      (>=
                          (list-length (ammo-of (wield-of user)))
                          (ammo-capacity-of (wield-of user)))
                      (and
                          (reload-count-of (wield-of user))
                          (>=
                              count
                              (reload-count-of (wield-of user)))))
                (leave t))
            (when (and
                      (typep item ammo-type)
                      (typep item (ammo-type-of (wield-of user))))
                (push item (ammo-of (wield-of user)))
                (removef item (inventory-of (player-of *game*)) :count 1)))))
(defun yadfa-bin:wield (&key user inventory)
    "Wield an item. Set INVENTORY to the index or a type specifier of an item in your inventory to wield that item. Set USER to the index of an ally to have them to equip it or leave it NIL for the player."
    #+sbcl (declare
               (type (or unsigned-byte null) user)
               (type (or (and symbol (not keyword)) list class null unsigned-byte) inventory))
    (check-type user (or unsigned-byte null))
    (check-type inventory (or (and symbol (not keyword)) list class null unsigned-byte))
    (let* ((selected-user
               (if
                   user
                   (nth user (allies-of *game*))
                   (player-of *game*)))
              (item (cond
                        ((typep inventory 'unsigned-byte)
                            (nth inventory (inventory-of (player-of *game*))))
                        ((typep inventory '(or list (and symbol (not keyword))))
                            (find inventory (inventory-of (player-of *game*))
                                :test #'(lambda (type-specifier obj)
                                            (typep obj type-specifier)))))))
        (cond ((not item)
                  (format t "INVENTORY isn't valid~%")
                  (return-from yadfa-bin:wield)))
        (when *battle* (format t
                           "The ~a you're battling stops and waits for you to equip your ~a because Pouar never prevented this function from being called in battle~%"
                           (if (> (list-length (enemies-of *battle*)) 1) "enemies" "enemy")
                           (name-of item)))
        (format t "~a equips his ~a ~a~%"
            (name-of selected-user)
            (if (malep selected-user) "his" "her")
            (name-of item))
        (removef (inventory-of (player-of *game*)) item :count 1)
        (when (wield-of selected-user)
            (push (wield-of selected-user) (inventory-of (player-of *game*))))
        (setf (wield-of selected-user) item)))
(defun yadfa-bin:unwield (&key user)
    "Unield an item. Set USER to the index of an ally to have them to unequip it or leave it NIL for the player."
    #+sbcl (declare (type (or integer null) user))
    (check-type user (or integer null))
    (let ((selected-user
              (if
                  user
                  (nth user (allies-of *game*))
                  (player-of *game*))))
        (if (wield-of selected-user)
            (progn
                (push (wield-of selected-user)
                    (inventory-of (player-of *game*)))
                (setf (wield-of selected-user) nil))
            (format t "~a hasn't equiped a weapon~%" (name-of selected-user)))))
(defun yadfa-bin:pokedex (&optional enemy)
    "Browse enemies in your pokedex, ENEMY is a quoted symbol that is the same as the class name of the enemy you want to view. Leave it to NIL to list available entries"
    (if enemy
        (let ((a (if (member enemy (seen-enemies-of *game*))
                     (make-instance enemy)
                     (progn
                         (write-line "That enemy isn't in your pokedex")
                         (return-from yadfa-bin:pokedex)))))
            (format t "Name: ~a~%Species: ~a~%Description: ~a~%" (name-of a) (species-of a) (description-of a)))
        (progn
            (format t "~30a~30a~%" "ID" "Name")
            (iter (for i in (seen-enemies-of *game*))
                (let ((a (make-instance i)))
                    (format t "~30a~30a~%" i (name-of a)))))))
(defun yadfa-world:add-ally-to-team (ally-index)
    "Adds an ally to your battle team. ALLY-INDEX is the index of an ally in your list of allies"
    #+sbcl (declare (type unsigned-byte ally-index))
    (check-type ally-index unsigned-byte)
    (if (>= ally-index (list-length (allies-of *game*)))
        (format t "You only have ~d allies~%" (list-length (allies-of *game*)))
        (pushnew
            (nth ally-index (allies-of *game*))
            (team-of *game*)
            :test (lambda (ally team-member)
                      (let ((result (eq ally team-member)))
                          (if result
                              (format t "~a is already on the battle team~%" (name-of ally))
                              (format t "~a has joined the battle team~%" (name-of ally)))
                          result)))))
(defun yadfa-world:remove-ally-from-team (team-index)
    "Removes an ally to your battle team. TEAM-INDEX is the index of an ally in your battle team list"
    #+sbcl (declare (type unsigned-byte team-index))
    (check-type team-index unsigned-byte)
    (cond
        ((>= team-index (list-length (team-of *game*)))
            (format t "You only have ~d members in your team~%" (list-length (team-of *game*)))
            (return-from yadfa-world:remove-ally-from-team))
        ((eq (nth team-index (team-of *game*)) (player-of *game*))
            (write-line "You can't remove the player from the team")
            (return-from yadfa-world:remove-ally-from-team))
        (t (setf (team-of *game*) (remove-nth team-index (team-of *game*))))))
(defun yadfa-world:swap-team-member (team-index-1 team-index-2)
    "swap the positions of 2 battle team members. TEAM-INDEX-1 and TEAM-INDEX-2 are the index numbers of these members in your battle team list"
    #+sbcl (declare (type unsigned-byte team-index-1 team-index-2))
    (check-type team-index-1 unsigned-byte)
    (check-type team-index-2 unsigned-byte)
    (cond
        ((or (>=
                 team-index-1
                 (list-length (team-of *game*)))
             (>=
                 team-index-2
                 (list-length (team-of *game*))))
            (format t "You only have ~d members in your team~%" (list-length (team-of *game*)))
            (return-from yadfa-world:swap-team-member))
        ((= team-index-1 team-index-2)
            (write-line "Those refer to the same team member")
            (return-from yadfa-world:swap-team-member))
        (t (rotatef (nth team-index-1 (team-of *game*)) (nth team-index-2 (team-of *game*))))))
(defun yadfa-bin:toggle-lock (wear key &optional user)
    "Toggle the lock on one of the clothes a user is wearing. WEAR is the index of an item a user is wearing, KEY is the index of a key in your inventory, USER is a number that is the index of an ally, leave this to NIL to select the player."
    #+sbcl (declare
               (type unsigned-byte wear key)
               (type (or unsigned-byte null) user))
    (check-type wear unsigned-byte)
    (check-type key unsigned-byte)
    (check-type user (or unsigned-byte null))
    (let ((selected-user (if user (nth user (allies-of *game*)) (player-of *game*))))
        (cond
            ((not selected-user)
                (format t "You only have ~d allies~%" (list-length (allies-of *game*))))
            ((not (< wear (list-length (wear-of selected-user))))
                (format t "~a is only wearing ~d items~%"
                    (name-of selected-user)
                    (list-length (wear-of selected-user))))
            ((not (< key (list-length (inventory-of (player-of *game*)))))
                (format t "You only have ~d items in your inventory~%"
                    (list-length (inventory-of (player-of *game*)))))
            ((not (typep (nth key (inventory-of (player-of *game*))) (key-of (nth wear (wear-of selected-user)))))
                (write-line "That doesn't go with that"))
            ((not (eq (lockedp (nth wear (wear-of selected-user))) :nil))
                (format t "~a's ~a is now unlocked~%"
                    (name-of selected-user)
                    (name-of (nth wear (wear-of selected-user))))
                (setf (lockedp (nth wear (wear-of selected-user))) :nil))
            ((typep (nth wear (wear-of selected-user)) 'closed-bottoms)
                (write-line "That can't be locked"))
            (t
                (format t "~a's ~a is now locked~%"
                    (name-of selected-user)
                    (name-of (nth wear (wear-of selected-user))))
                (setf (lockedp (nth wear (wear-of selected-user))) t)))))
