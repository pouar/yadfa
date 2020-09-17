;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa"; coding: utf-8-unix; -*-
(in-package :yadfa)
;;; list-length-< and list-length-<= are based off of sequence-of-length-p from Alexandria
(defun list-length-<= (length list)
  (declare (type list list)
           (type integer length))
  (let ((n (1- length)))
    (and (or (minusp n) (nthcdr n list)) t)))
(defun list-length-< (length list)
  (declare (type list list)
           (type integer length))
  (list-length-<= (1+ length) list))
(defun list-length-> (length list)
  (declare (type list list)
           (type integer length))
  (not (list-length-<= length list)))
(defun list-length->= (length list)
  (declare (type list list)
           (type integer length))
  (not (list-length-< length list)))
(defun switch-user-packages (&aux (clim:*application-frame* (clim:find-application-frame 'yadfa-clim::yadfa-listener)))
  (declare (special clim:*application-frame*))
  (if *battle*
      (progn
        (unuse-package *world-packages* :yadfa-user)
        (use-package *battle-packages* :yadfa-user)
        (conditional-commands:change-entity-enabledness 'yadfa-clim::com-enable-battle))
      (progn
        (unuse-package *battle-packages* :yadfa-user)
        (use-package *world-packages* :yadfa-user)
        (conditional-commands:change-entity-enabledness 'yadfa-clim::com-enable-world)))
  (use-package *command-packages* :yadfa-user)
  t)

(defunassert get-event (event-id)
  (event-id symbol)
  (gethash event-id *events*))
(defunassert (setf get-event) (new-value event-id)
  (event-id symbol)
  (setf (gethash event-id *events*) new-value))
(defun get-zone (position)
  (declare (type list position))
  (gethash position (slot-value *game* 'zones)))
(defun (setf get-zone) (new-value position)
  (declare (type list position)
           (type zone new-value))
  (setf (position-of new-value) position
        (gethash position (slot-value *game* 'zones)) new-value))
(s:eval-always
 (defun set-logical-pathnames ()
   (setf (logical-pathname-translations "YADFA")
         `(("yadfa:data;**;*.*.*" ,(uiop:merge-pathnames*
                                    (make-pathname
                                     :directory '(:relative "YADFA" :wild-inferiors)
                                     :name :wild
                                     :type :wild
                                     :version :wild
                                     :case :common)
                                    (uiop:xdg-data-home)))
           ("yadfa:config;**;*.*.*" ,(uiop:merge-pathnames*
                                      (make-pathname
                                       :directory '(:relative "YADFA" :wild-inferiors)
                                       :name :wild
                                       :type :wild
                                       :version :wild
                                       :case :common)
                                      (uiop:xdg-config-home)))
           ("yadfa:home;**;*.*.*" ,(uiop:merge-pathnames*
                                    (make-pathname
                                     :directory '(:relative :wild-inferiors)
                                     :type :wild
                                     :name :wild
                                     :version :wild
                                     :case :common)
                                    (if uiop:*image-dumped-p*
                                        (make-pathname
                                         :device (pathname-device (truename (uiop:argv0)))
                                         :directory (pathname-directory (truename (uiop:argv0))))
                                        (asdf:component-pathname (asdf:find-system "yadfa")))))))
   (illogical-pathnames:define-illogical-host :yadfa.data (uiop:merge-pathnames*
                                                           (make-pathname
                                                            :directory '(:relative "YADFA")
                                                            :case :common)
                                                           (uiop:xdg-data-home)))
   (illogical-pathnames:define-illogical-host :yadfa.config (uiop:merge-pathnames*
                                                             (make-pathname
                                                              :directory '(:relative "YADFA")
                                                              :case :common)
                                                             (uiop:xdg-config-home)))
   (illogical-pathnames:define-illogical-host :yadfa.home (if uiop:*image-dumped-p*
                                                              (make-pathname
                                                               :device (pathname-device (truename (uiop:argv0)))
                                                               :directory (pathname-directory (truename (uiop:argv0))))
                                                              (asdf:system-source-directory "yadfa"))))
 (set-logical-pathnames))
(defun process-potty-dance-check (character attack)
  (and (or
        (>= (bladder/contents-of character) (bladder/potty-dance-limit-of character))
        (>= (bowels/contents-of character) (bowels/potty-dance-limit-of character)))
       (< (car (sort (let ((a ())
                           (x (- (bladder/maximum-limit-of character) (bladder/contents-of character)))
                           (y (- (bladder/maximum-limit-of character) (bladder/potty-dance-limit-of character))))
                       (when (>= (bladder/contents-of character) (bladder/potty-dance-limit-of character))
                         (push (random (expt (u:$ x / y * (expt 5 1.3l0)) (coerce (/ 1 (+ 1 3/10)) 'long-float))) a))
                       (setf x (- (bowels/maximum-limit-of character) (bowels/contents-of character))
                             y (- (bowels/maximum-limit-of character) (bowels/potty-dance-limit-of character)))
                       (when (>= (bowels/contents-of character) (bowels/potty-dance-limit-of character))
                         (push (random (expt (u:$ x / y * (expt 5 2)) 0.5l0)) a))
                       a)
                     '<))
          1)
       (or (eq attack t) (not (typep (get-move attack character) '(or mess-move-mixin wet-move-mixin))))))
(defunassert get-positions-of-type (type list)
  (type type-specifier
        list list)
  (iter (for i in list)
        (for (the fixnum j) upfrom 0)
        (when (typep i type)
          (collect j))))
(defunassert finished-events (events)
  (events (or list symbol))
  (iter (for (the (or list symbol) event) in (a:ensure-list events))
        #-(or sbcl ccl)
        (check-type event (or list symbol))
        (unless (gethash (a:ensure-list event) (finished-events-of *game*))
          (leave))
        (finally (return t))))
(defunassert unfinished-events (events)
  (events (or list symbol))
  (iter (for (the (or list symbol) event) in (a:ensure-list events))
        #-(or sbcl ccl)
        (check-type event (or list symbol))
        (when (gethash (a:ensure-list event) (finished-events-of *game*))
          (leave))
        (finally (return t))))
(defunassert finish-events (events)
  (events (or list symbol))
  (iter (for (the symbol event) in (a:ensure-list events))
        #-(or sbcl ccl)
        (check-type event symbol)
        (remhash event (current-events-of *game*))
        (setf (gethash `(,event) (finished-events-of *game*)) t)))
(defunassert get-diaper-expansion (item)
  (item closed-bottoms)
  (+ (* 10 (/ (+ (sogginess-of item) (messiness-of item))
              (- (* 72 36) (* (/ (* 72 5/7) 2) 18/2 pi))))
     (thickness-of item)))
(defun initialize-mod-registry ()
  (clrhash *mod-registry*)
  (labels ((preferred-mod (old new)
             (cond ((not old)
                    new)
                   ((list-length-> (list-length (pathname-directory old))
                                   (pathname-directory new))
                    new)
                   ((list-length->
                     (list-length (pathname-directory new))
                     (pathname-directory old))
                    old)
                   ((string< (the simple-string (namestring old)) (the simple-string (namestring new)))
                    old)
                   (t new))))
    (iter (for i in (uiop:directory*
                     #P((:common :yadfa.data) ("MODS" :**) (:* "ASD") :newest)))
          (setf (gethash (pathname-name i) *mod-registry*)
                (preferred-mod (gethash (pathname-name i) *mod-registry*)
                               i)))))
(defun clear-pattern-cache ()
  (clrhash *pattern-cache*))
(defunassert find-mod (system)
  (system (or symbol simple-string))
  (gethash (asdf:primary-system-name system) *mod-registry*))
(defun clear-configuration-hook ()
  (set-logical-pathnames)
  (clear-pattern-cache)
  (initialize-mod-registry))
(defun load-mods (&rest keys &key compiler-verbose &allow-other-keys)
  (unless
      (and (find "texi" (uiop:command-line-arguments) :test #'string=) (asdf:component-loaded-p "yadfa/docs"))
    (let* ((file #P"yadfa:config;mods.conf")
           (mods '()))
      (ensure-directories-exist #P"yadfa:config;")
      (handler-case (a:with-input-from-file (stream file)
                      (setf mods (read stream)))
        (file-error ()
          (write-line "The configuration file containing the list of enabled mods seems missing, creating a new one")
          (a:with-output-to-file (stream file
                                         :if-exists :supersede
                                         :external-format :utf-8)
            (write *mods* :stream stream)))
        (error ()
          (write-line "The configuration file containing the list of enabled mods seems broken, ignoring")))
      (if (and
           (typep mods 'list)
           (iter (for i in mods)
                 (unless (typep i '(or string symbol asdf/component:component))
                   (leave nil))
                 (finally (return t))))
          (setf *mods* mods)
          (write-line "The configuration file containing the list of enabled mods isn't valid, ignoring")))
    (let ((*compile-verbose* compiler-verbose)
          (*compile-print* compiler-verbose))
      (iter (for i in *mods*)
            (when (asdf:find-system i nil)
              (apply #'asdf:load-system i :allow-other-keys t keys))))))
(defun (setf getf-direction) (new-value position direction attribute)
  (setf (getf (getf (direction-attributes-of (get-zone position)) direction) attribute) new-value))
(defun getf-direction (position direction attribute)
  (getf (getf (direction-attributes-of (get-zone position)) direction) attribute))
(defun remf-direction (position direction attribute)
  (remf (getf (direction-attributes-of (get-zone position)) direction) attribute)
  (unless (getf (direction-attributes-of (get-zone position)) direction)
    (remf (direction-attributes-of (get-zone position)) direction)))
(defun set-status-condition (status-condition user &key duration test key
                             &aux (status-conditions (iter (for i in (getf (status-conditions-of *battle*) user))
                                                           (when (eq (type-of i) status-condition)
                                                             (collect i))))
                               (i (if (or (eq (accumulative-of (make-instance status-condition)) t)
                                          (list-length-> (accumulative-of (make-instance status-condition)) status-conditions))
                                      (make-instance status-condition)
                                      (car (s:dsu-sort status-conditions (lambda (a b)
                                                                           (cond ((eq b t)
                                                                                  nil)
                                                                                 ((eq a t)
                                                                                  t)
                                                                                 (t (< a b))))
                                                       :key #'duration-of))))
                               (duration (or duration (duration-of (make-instance status-condition)))))
  (pushnew i (getf (status-conditions-of *battle*) user) :test (or test #'eql) :key (or key #'identity))
  (when (and (not (eq (duration-of i) t)) (< (duration-of i) duration))
    (setf (duration-of i) duration))
  t)
(defunassert trigger-event (event-ids)
  (event-ids (or symbol list))
  (iter (for (the symbol event-id) in (a:ensure-list event-ids))
        #-(or sbcl ccl)
        (check-type event-id symbol)
        (when (and
               (funcall (coerce (slot-value (get-event event-id) 'predicate) 'function)
                        (get-event event-id))
               (or (and (slot-value (get-event event-id) 'repeatable) (not (gethash event-id (current-events-of *game*))))
                   (not (gethash event-id (finished-events-of *game*))))
               (finished-events (slot-value (get-event event-id) 'finished-depends)))
          (let* ((mission (slot-value (get-event event-id) 'mission))
                 (accept (when mission
                           (funcall (coerce (slot-value (get-event event-id) 'mission) 'function)))))
            (when mission
              (setf (gethash event-id (current-events-of *game*)) t))
            (setf (gethash `(,event-id
                             ,@(when (and mission (s:memq accept '(:accepted :declined)))
                                 `(,accept)))
                           (finished-events-of *game*))
                  t)
            (apply (coerce (slot-value (get-event event-id) 'lambda) 'function)
                   `(,event-id ,@(when mission `(,accept)))))
          (collect event-id))))
(defunassert event-attributes (event-id)
  (event-id symbol)
  (gethash event-id (slot-value *game* 'event-attributes%)))
(defunassert (setf event-attributes) (instance event-id)
  (event-id symbol)
  (setf (gethash event-id (slot-value *game* 'event-attributes%)) instance))
(defun set-new-battle (enemies &rest keys &key team-npcs win-events enter-battle-text continuable)
  (when continuable
    (setf
     (continue-battle-of (get-zone (position-of (player-of *game*))))
     `(:enemies ,enemies
       :win-events ,win-events
       :enter-battle-text ,enter-battle-text
       :team-npcs ,team-npcs)))
  (setf *battle*
        (apply #'make-instance 'battle
               :enemies (iter (for (the list j) in enemies)
                              (collect (apply #'make-instance (car j) (eval (cdr j)))))
               :team-npcs (iter (for (the list j) in team-npcs)
                                (collect (apply #'make-instance (car j) (eval (cdr j)))))
               keys))
  (format t "~a~%" (enter-battle-text-of *battle*))
  (iter (for (the symbol j) in (iter (for i in (enemies-of *battle*))
                                     (unless (s:memq (class-name (class-of i)) (seen-enemies-of *game*))
                                       (format t "~a was added to your pokedex~%" (name-of i))
                                       (push (class-name (class-of i)) (seen-enemies-of *game*))
                                       (collect (class-name (class-of i))))))
        (yadfa-bin:pokedex j))
  (switch-user-packages)
  (process-battle :attack t :no-team-attack t))
(defunassert run-equip-effects (user)
  (user base-character)
  (iter (for i in (wear-of user))
        (wear-script i user))
  (when (wield-of user)
    (wield-script (wield-of user) user)))
(defunassert get-warp-point (direction position)
  (direction symbol position list)
  (getf (warp-points-of (get-zone position))
        (typecase direction
          ((member :north :south :east :west :up :down)
           direction)
          (keyword
           (iter (for (k v) on (warp-points-of (get-zone position)) by 'cddr)
                 (when (and (string= k direction) v)
                   (leave k))))
          (symbol direction))))
(defunassert get-destination (direction position)
  (direction symbol position list)
  (macrolet ((a (pos x y z)
               (a:with-gensyms ((posx "POSX") (posy "POSY") (posz "POSZ") (posm "POSM") (b "B"))
                 `(let ((,b (destructuring-bind (,posx ,posy ,posz ,posm) ,pos
                              (declare (type integer ,posx ,posy ,posz)
                                       (type symbol ,posm))
                              (s:append1 (mapcar #'+ (list ,posx ,posy ,posz) '(,x ,y ,z)) ,posm))))
                    (when (get-zone ,b)
                      ,b)))))
    (case direction
      (:north (a position 0 -1 0))
      (:south (a position 0 1 0))
      (:east (a position 1 0 0))
      (:west (a position -1 0 0))
      (:up (a position 0 0 1))
      (:down (a position 0 0 -1))
      (otherwise (get-warp-point direction position)))))
(defunassert get-path-end (destination &optional position direction
                                       &aux (player (player-of *game*)) (allies (allies-of *game*)) (wield (wield-of player))
                                       (wear (wear-of player)) (inventory (inventory-of player)))
  (direction symbol position list destination list)
  (unless (get-zone destination)
    (return-from get-path-end (values nil (format nil "Pick a direction the game knows about~%"))))
  (when (or (hiddenp (get-zone destination)) (and position direction (getf-direction position direction :hidden)))
    (return-from get-path-end (values nil (format nil "Pick a direction the game knows about~%"))))
  (when (and direction (s:memq direction '(:up :down)) (not (s:memq direction (stairs-of (get-zone (or position (position-of player)))))))
    (return-from get-path-end (values nil (format nil "There are no stairs there~%"))))
  (when (or (and (lockedp (get-zone destination))
                 (not (member-if (lambda (a)
                                   (typep a (key-of (get-zone destination))))
                                 (append inventory
                                         (cons wield wear)
                                         (let ((a ()))
                                           (iter (for i in allies)
                                                 (push (wield-of i) a)
                                                 (iter (for j in (wear-of i))
                                                       (push j a)))
                                           a)))))
            (and position direction
                 (getf-direction position direction :locked)
                 (not (member-if (lambda (a)
                                   (typep a (getf-direction position direction :key)))
                                 (append inventory
                                         (cons wield wear)
                                         (let ((a ()))
                                           (iter (for i in allies)
                                                 (push (wield-of i) a)
                                                 (iter (for j in (wear-of i))
                                                       (push j a)))
                                           a))))))
    (return-from get-path-end (values nil (format nil "zone ~a is locked~%" destination))))
  destination)
(defunassert print-map-pattern-cache (path designs)
  (path pathname designs list)
  (or (gethash `(:map-pattern ,path ,designs) *pattern-cache*)
      (setf (gethash `(:map-pattern ,path ,designs) *pattern-cache*)
            (clim:make-pattern-from-bitmap-file
             (uiop:merge-pathnames*
              path
              #P"yadfa:home;pixmaps;map-patterns;")
             :format :xpm
             :designs designs))))
(defun travelablep (position direction)
  (declare (type list position)
           (type symbol direction))
  (and (get-zone (get-destination direction position))
       (get-zone position)
       (not (getf-direction position direction :hidden))
       (not (hiddenp (get-zone (get-destination direction position))))
       (or (and (s:memq direction '(:up :down)) (s:memq direction (stairs-of (get-zone position))))
           (and (not (s:memq direction '(:up :down)))))
       t))
(defunassert print-map (position &aux (player (player-of *game*)) (player-position (position-of player)) (player-zone (get-zone player-position)))
  (player player player-position list player-zone (or null zone))
  (labels ((a (position)
             (let ((b 0)
                   (array
                     #1A(#P"nsew.xpm"
                           #P"nsw.xpm"
                           #P"nse.xpm"
                           #P"ns.xpm"
                           #P"new.xpm"
                           #P"nw.xpm"
                           #P"ne.xpm"
                           #P"n.xpm"
                           #P"sew.xpm"
                           #P"sw.xpm"
                           #P"se.xpm"
                           #P"s.xpm"
                           #P"ew.xpm"
                           #P"w.xpm"
                           #P"e.xpm"
                           #P"dot.xpm")))
               (iter (for direction in '(:east :west :south :north))
                     (for (the fixnum byte-position) upfrom 0)
                     (unless (travelablep position direction)
                       (setf (ldb (byte 1 byte-position) b) 1)))
               (aref array b))))
    (updating-present-with-effective-frame (*query-io* :unique-id `(map% ,position)
                                                       :id-test #'equal
                                                       :cache-value (sxhash (list player-position
                                                                                  (iter (for i in '(:north :south :east :west :up :down))
                                                                                        (collect (travelablep player-position i)))
                                                                                  (and player-zone
                                                                                       (warp-points-of player-zone)))))
                                           (let ((pattern (print-map-pattern-cache #P"blank.xpm"
                                                                                   (list clim:+background-ink+ clim:+foreground-ink+))))
                                             (multiple-value-bind (start-x start-y) (if c:*application-frame*
                                                                                        (clim:stream-cursor-position *standard-output*)
                                                                                        (values 0 0))
                                               (declare (type real start-x start-y))
                                               (clim:updating-output (t)
                                                                     ;; position needs to be bound inside of clim:updating-output and not outside
                                                                     ;; for the presentation to notice when the floor the player is on changes
                                                                     (let* ((player-position (position-of (player-of *game*)))
                                                                            (position (if (eq position t)
                                                                                          player-position
                                                                                          position)))
                                                                       (declare (type list position player-position))
                                                                       (destructuring-bind (posx posy posz posm) position
                                                                         (declare (type integer posx posy posz)
                                                                                  (type symbol posm))
                                                                         (iter (for (the integer y)
                                                                                    from (- posy 15)
                                                                                    to (+ posy 15))
                                                                               (for y-pos
                                                                                    from start-y
                                                                                    to (+ start-y (* 30 (the (unsigned-byte 32) (clim:pattern-height pattern))))
                                                                                    by (the (unsigned-byte 32) (clim:pattern-height pattern)))
                                                                               (iter (for (the integer x)
                                                                                          from (- posx 15)
                                                                                          to (+ posx 15))
                                                                                     (for x-pos
                                                                                          from start-x
                                                                                          to (+ start-x (* 30 (the (unsigned-byte 32) (clim:pattern-width pattern))))
                                                                                          by (the (unsigned-byte 32) (clim:pattern-width pattern)))
                                                                                     (let* ((current-position `(,x ,y ,posz ,posm))
                                                                                            (current-zone (get-zone current-position))
                                                                                            (char (cons (if (or (and current-zone (hiddenp current-zone)) (not current-zone))
                                                                                                            #P"blank.xpm"
                                                                                                            (a current-position))
                                                                                                        (clim:make-rgb-color (if (and current-zone (warp-points-of current-zone)) 1 0)
                                                                                                                             (if (equal current-position player-position) 0.7l0 0)
                                                                                                                             (if (or (travelablep current-position :up) (travelablep current-position :down)) 1 0)))))
                                                                                       (setf pattern (print-map-pattern-cache (car char) (list clim:+background-ink+ (cdr char))))
                                                                                       (when (get-zone current-position)
                                                                                         (clim:with-output-as-presentation
                                                                                             (*standard-output* (get-zone current-position) 'zone)
                                                                                           (clim:draw-pattern* *standard-output* pattern x-pos y-pos)))))))))
                                               (when c:*application-frame*
                                                 (clim:stream-set-cursor-position *standard-output* start-x (+ start-y (* 31 (the (unsigned-byte 32) (clim:pattern-height pattern)))))))))))
(defunassert get-zone-text (text)
  (text (or string coerced-function))
  (typecase text
    (string
     text)
    (t
     (the (values string &optional) (funcall (coerce text 'function))))))
(defun print-enter-text (position &optional old-position old-direction)
  (let ((old-direction (when (and old-direction old-position)
                         (typecase old-direction
                           ((member :north :south :east :west :up :down)
                            old-direction)
                           (keyword
                            (iter (for (k v) on (warp-points-of (get-zone old-position)) by 'cddr)
                                  (when (and (string= k old-direction) v)
                                    (leave k))))
                           (symbol old-direction)))))
    (format t "~a~%" (get-zone-text (if (and old-position old-direction (getf-direction old-position old-direction :exit-text))
                                        (getf-direction old-position old-direction :exit-text)
                                        (enter-text-of (get-zone position))))))
  (flet ((z (delta direction stairs)
           (let* ((current-position (destructuring-bind (x y z m) position
                                      (declare (type integer x y z)
                                               (type symbol m))
                                      `(,@(mapcar #'+ (list x y z) delta) ,m)))
                  (current-zone (get-zone current-position))
                  (stairs (and current-zone (s:memq direction stairs))))
             (when (and current-zone
                        (not (hiddenp current-zone))
                        (or (and (s:memq direction '(:up :down)) stairs)
                            (not (s:memq direction '(:up :down)))))
               (format t "To ~s is ~a. " direction (name-of current-zone))))))
    (let ((stairs (stairs-of (get-zone position))))
      (z '(-1 0 0) :west stairs)
      (z '(1 0 0) :east stairs)
      (z '(0 1 0) :south stairs)
      (z '(0 -1 0) :north stairs)
      (z '(0 0 1) :up stairs)
      (z '(0 0 -1) :down stairs)))
  (iter (for (a b) on (warp-points-of (get-zone position)) by #'cddr)
        (when (and (get-zone b) (not (hiddenp (get-zone b))))
          (format t "To ~s is ~a. " a (name-of (get-zone b)))))
  (format t "~%"))
(defun get-inventory-list ()
  (iter (for i in (inventory-of (player-of *game*))) (collect (symbol-name (type-of i)))))
(defunassert filter-items (items type)
  (items list type type-specifier)
  "This function will return all items in the list @var{ITEMS} that is of type @var{TYPE}"
  (iter (for item in items)
        (when (typep item type)
          (collect item))))
(defunassert swell-up% (user)
  (user base-character)
  (iter (for i in (filter-items (wear-of user) 'closed-bottoms))
        (if (waterproofp i)
            (finish)
            (progn
              (setf (sogginess-of i) (sogginess-capacity-of i))
              (collect i)))))
(defunassert swell-up (user &aux (swollen-clothes (swell-up% user)) (name (name-of user)))
  (user base-character)
  (cond
    ((filter-items swollen-clothes 'diaper)
     (format t "~a's diapers swells up humorously~%~%" name))
    ((filter-items swollen-clothes 'pullup)
     (format t "~a's pullups swells up humorously~%~%" name))
    ((filter-items swollen-clothes 'stuffer)
     (format t "~a's incontinence pad swells up~%~%" name)))
  (pop-from-expansion user))
(defun swell-up-all ()
  (swell-up (player-of *game*))
  (iter (for i in (allies-of *game*)) (swell-up i)))
(defunassert total-thickness (clothing)
  (clothing list)
  (iter (for i in (filter-items clothing 'closed-bottoms))
        (with j = 0)
        (incf j (get-diaper-expansion i))
        (finally (return j))))
(defun fast-thickness (list item)
  #+sbcl (declare (type list list)
                  (type clothing item))
  (s:nlet execute (list item (count 0))
          (if (or (eq (car list) item) (endp list))
              count
              (execute (cdr list) item (if (typep (car list) 'closed-bottoms)
                                           (+ count (get-diaper-expansion (car list)))
                                           count)))))
(defunassert pop-from-expansion (user &optional wet/mess &aux (reverse-wear (nreverse (wear-of user))) (last (car reverse-wear)) (return ()))
  (user base-character)
  (macrolet ((pushclothing (i wet/mess return)
               `(progn
                  (when (and (getf (car ,wet/mess) :wet-amount)
                             (> (getf (car ,wet/mess) :wet-amount) 0))
                    (pushnew ,i (getf (car ,wet/mess) :popped)))
                  (when (and (getf (cdr ,wet/mess) :mess-amount)
                             (> (getf (cdr ,wet/mess) :mess-amount) 0))
                    (pushnew ,i (getf (cdr ,wet/mess) :popped)))
                  (pushnew ,i ,return))))
    (iter
     (for item in reverse-wear)
     (let* ((thickness-capacity (if (typep item 'bottoms) (thickness-capacity-of item)))
            (thickness-capacity-threshold (if (typep item 'bottoms) (thickness-capacity-threshold-of item)))
            (total-thickness (if (and (typep item 'bottoms)
                                      thickness-capacity
                                      thickness-capacity-threshold)
                                 (fast-thickness reverse-wear item))))
       (declare (type (or null (real 0)) thickness-capacity thickness-capacity-threshold total-thickness))
       (when
           (and (not (eq item last))
                total-thickness
                thickness-capacity
                thickness-capacity-threshold
                (> total-thickness (+ thickness-capacity thickness-capacity-threshold)))
         (typecase item
           (onesie/closed
            (toggle-onesie% item)
            (if (lockedp item)
                (progn (format t "~a's ~a pops open from the expansion destroying the lock in the process~%~%"
                               (name-of user)
                               (name-of item))
                       (setf (lockedp item) nil))
                (format t "~a's ~a pops open from the expansion~%~%"
                        (name-of user)
                        (name-of item)))
            (pushclothing (the item item) wet/mess return))
           ((or incontinence-product snap-bottoms)
            (push item (inventory-of (if (typep user 'team-member)
                                         (player-of *game*)
                                         user)))
            (a:deletef (the list reverse-wear) item :count 1)
            (format t "~a's ~a comes off from the expansion~%~%"
                    (name-of user)
                    (name-of item))
            (pushclothing (the item item) wet/mess return))
           ((and bottoms (not incontinence-product))
            (a:deletef (the list reverse-wear) item :count 1)
            (format t "~a's ~a tears from the expansion and is destroyed~%~%"
                    (name-of user)
                    (name-of item))
            (pushclothing (the item item) wet/mess return))))))
    (setf (wear-of user) (nreverse reverse-wear))
    (cond ((or (getf (car wet/mess) :popped) (getf (cdr wet/mess) :popped))
           (values wet/mess :wet/mess))
          (return (values return :return))
          (t (values nil nil)))))
(defunassert thickest-sort (clothing)
  (clothing list)
  (s:dsu-sort (iter (for i in clothing)
                    (when (typep i 'closed-bottoms)
                      (collect i)))
              '>
              :key 'get-diaper-expansion))
(defunassert thickest (clothing &optional n &aux (a (iter (for i in clothing)
                                                          (when (typep i 'closed-bottoms)
                                                            (collect i)))))
  (clothing list n (or null unsigned-byte))
  (if n
      (the (values list &optional)
           (s:bestn n a '> :key 'get-diaper-expansion :memo t))
      (iter (for i in a)
            (finding i maximizing (get-diaper-expansion i)))))
(defun move-to-zone (new-position &key ignore-lock direction old-position)
  (when (iter (for i in (cons (player-of *game*) (allies-of *game*)))
              (let ((wear (typecase (must-wear-of (get-zone new-position))
                            (cons (must-wear-of (get-zone new-position)))
                            (symbol (gethash (must-wear-of *game*) (must-wear-of (get-zone new-position))))))
                    (not-wear (typecase (must-not-wear-of (get-zone new-position))
                                (cons (must-not-wear-of (get-zone new-position)))
                                (symbol (gethash (must-not-wear-of *game*) (must-not-wear-of (get-zone new-position)))))))
                (when (or (and (list-length-> 1 (filter-items (wear-of i) (car wear)))
                               (not (funcall (coerce (cdr wear) 'function) i)))
                          (and (list-length-< 0 (filter-items (wear-of i) (car not-wear)))
                               (not (funcall (coerce (cdr not-wear) 'function) i))))
                  (leave t))))
    (return-from move-to-zone))
  (when (and (not ignore-lock)
             (or (and (lockedp (get-zone new-position))
                      (not (member (key-of (get-zone new-position)) (inventory-of (player-of *game*))
                                   :test (lambda (item key)
                                           (typep key item)))))
                 (and (getf (getf (direction-attributes-of (get-zone (position-of (player-of *game*)))) direction) :locked)
                      (not (member (getf (getf (direction-attributes-of (get-zone (position-of (player-of *game*)))) direction) :key)
                                   (inventory-of (player-of *game*))
                                   :test (lambda (item key)
                                           (typep key item)))))))
    (write-line "That zone is locked and you don't have a key")
    (return-from move-to-zone))
  (incf (time-of *game*))
  (when (or
         (and (lockedp (get-zone new-position))
              (or ignore-lock (member (key-of (get-zone new-position)) (inventory-of (player-of *game*))
                                      :test (lambda (item key)
                                              (typep key item)))))
         (and (getf (getf (direction-attributes-of (get-zone (position-of (player-of *game*)))) direction) :locked)
              (member (getf (getf (direction-attributes-of (get-zone (position-of (player-of *game*)))) direction) :key)
                      (inventory-of (player-of *game*))
                      :test (lambda (item key)
                              (typep key item)))))
    (format t "You unlock zone ~a~%" new-position)
    (setf (lockedp (get-zone new-position)) nil)
    (remf (getf (direction-attributes-of (get-zone (position-of (player-of *game*)))) direction) :locked))
  (setf (position-of (player-of *game*)) new-position)
  (when (underwaterp (get-zone (position-of (player-of *game*)))) (swell-up-all))
  (process-potty)
  (run-equip-effects (player-of *game*))
  (iter (for i in (allies-of *game*))
        (process-potty i)
        (run-equip-effects i))
  (print-enter-text (position-of (player-of *game*)) old-position direction)
  (cond ((continue-battle-of (get-zone (position-of (player-of *game*))))
         (set-new-battle (getf (continue-battle-of (get-zone (position-of (player-of *game*)))) :enemies)
                         :team-npcs (getf (continue-battle-of (get-zone (position-of (player-of *game*)))) :team-npcs)
                         :win-events (getf (continue-battle-of (get-zone (position-of (player-of *game*)))) :win-events)
                         :continuable t
                         :enter-battle-text (getf (continue-battle-of (get-zone (position-of (player-of *game*)))) :enter-battle-text))
         (return-from move-to-zone))
        ((trigger-event (events-of (get-zone (position-of (player-of *game*)))))
         (return-from move-to-zone))
        ((resolve-enemy-spawn-list (get-zone (position-of (player-of *game*))))
         (let ((enemy-spawn-list (iter (for i in (resolve-enemy-spawn-list (get-zone (position-of (player-of *game*)))))
                                       (when (< (random 1.0l0) (getf i :chance))
                                         (leave (cond ((getf i :eval)
                                                       (eval (getf i :eval)))
                                                      ((getf i :lambda)
                                                       (funcall (coerce (getf i :lambda) 'function)))
                                                      (t (getf i :enemies)))))))
               (team-npc-spawn-list (iter (for i in (resolve-team-npc-spawn-list (get-zone (position-of (player-of *game*)))))
                                          (when (< (random 1.0l0) (getf i :chance))
                                            (leave (cond ((getf i :eval)
                                                          (eval (getf i :eval)))
                                                         ((getf i :lambda)
                                                          (funcall (coerce (getf i :lambda) 'function)))
                                                         (t (getf i :enemies))))))))
           (when enemy-spawn-list
             (set-new-battle enemy-spawn-list :team-npcs team-npc-spawn-list))))))
(defun move-to-secret-underground ()
  (when *battle*
    (write-line "To avoid breaking the game due to a few assumptions made in this function, please don't run this in a battle~%")
    (return-from move-to-secret-underground))
  (multiple-value-bind (destination error) (get-path-end '(0 0 0 yadfa-zones:secret-underground))
    (unless destination
      (format t "~a" error)
      (return-from move-to-secret-underground))
    (move-to-zone '(0 0 0 yadfa-zones:secret-underground) :ignore-lock t)))
(defun move-to-pocket-map (item)
  (when *battle*
    (write-line "To avoid breaking the game due to a few assumptions made in this function, please don't run this in a battle~%")
    (return-from move-to-pocket-map))
  (unless (get-zone '(0 0 0 pocket-map))
    (make-pocket-zone (0 0 0)
                      :name "Pocket Map Entrance"
                      :description "Welcome to the Pocket Map. It's like the secret bases in Pokémon, except you customize it by scripting, and you can take it with you."
                      :enter-text "You're at the start of the Pocket Map. Use the Pocket Map machine again at anytime to exit."))
  (let ((old-position (position-of (player-of *game*))))
    (move-to-zone (if (eq (fourth (position-of (player-of *game*))) :pocket-map)
                      (getf (attributes-of item) :pocket-map-position)
                      '(0 0 0 :pocket-map))
                  :ignore-lock t)
    (unless (eq (fourth old-position) :pocket-map)
      (setf (getf (attributes-of item) :pocket-map-position) old-position))))
(defunassert wet (&key (wet-amount t) force-fill-amount pants-down accident force-wet-amount (wetter (player-of *game*)) (clothes nil clothes-supplied-p)
                       &aux (return-value ()) (affected-clothes ()) (random (random 4)) (amount nil)
                       (clothes (if clothes-supplied-p clothes (wear-of wetter))))
  (force-fill-amount (or null real)
                     force-wet-amount (or boolean real)
                     wet-amount (or boolean real)
                     wetter base-character)
  #.(format nil "this function is mostly for mods, doesn't print text or diaper expansion, that's handled by other functions. @var{WETTER} is the instance of @code{BASE-CHARACTER} doing the flooding. @var{WET-AMOUNT} is the amount @var{WETTER} floods but won't flood if he/she can't go, passing @code{T} to @var{WET-AMOUNT} means to use @code{(BLADDER/CONTENTS-OF WETTER)}, @var{FORCE-WET-AMOUNT} causes @var{WETTER} to wet regardless. @var{FORCE-FILL-AMOUNT} will set @code{(BLADDER/CONTENTS-OF WETTER)} to that amount first. @var{PANTS-DOWN} is @code{T} if @var{WETTER} pulls his/her pants down first. @var{ACCIDENT} is @code{T} if the wetting isn't intentional and @var{WETTER} may or may not be able to stop the flow. if @var{CLOTHES} is passed, it will be the one @var{WETTER} floods, otherwise it will be @code{(wear-of @var{WETTER})}

~a."
            (xref mess :function))
  (when force-fill-amount
    (setf (bladder/contents-of wetter) force-fill-amount))
  (cond (force-wet-amount
         (setf amount (cond ((eq force-wet-amount t)
                             (bladder/contents-of wetter))
                            ((> force-wet-amount (bladder/contents-of wetter))
                             (bladder/contents-of wetter))
                            (t
                             force-wet-amount))))
        ((< (bladder/contents-of wetter) (bladder/need-to-potty-limit-of wetter))
         (return-from wet `(:old-bladder-contents ,(bladder/contents-of wetter)
                            :new-bladder-contents ,(bladder/contents-of wetter)
                            :affected-clothes ()
                            :leak-amount 0
                            :wet-amount 0)))
        (accident
         (setf amount
               (a:switch (random :test '=)
                         (3 (* 4 (bladder/fill-rate-of wetter)))
                         (2 (bladder/need-to-potty-limit-of wetter))
                         (t (bladder/contents-of wetter)))))
        (t (setf amount (cond ((eq wet-amount t)
                               (bladder/contents-of wetter))
                              ((> wet-amount (bladder/contents-of wetter))
                               (bladder/contents-of wetter))
                              (t
                               wet-amount)))))
  (setf (getf return-value :accident)
        (if accident
            (a:switch (random :test '=)
                      (3 :dribble)
                      (2 :some)
                      (t :all))))
  (setf (getf return-value :old-bladder-contents) (bladder/contents-of wetter))
  (let* ((amount-left amount))
    (cond ((or pants-down (not (filter-items clothes 'closed-bottoms)))
           (decf (bladder/contents-of wetter) amount)
           (setf amount-left 0))
          (t
           (decf (bladder/contents-of wetter) amount)
           (iter (while (> amount-left 0))
                 (for i in (reverse clothes))
                 (when (typep i 'closed-bottoms)
                   (cond ((> amount-left (- (sogginess-capacity-of i) (sogginess-of i)))
                          (if (leakproofp i)
                              (setf amount-left 0)
                              (decf amount-left (- (sogginess-capacity-of i) (sogginess-of i))))
                          (setf (sogginess-of i) (sogginess-capacity-of i))
                          (push i affected-clothes)
                          )
                         ((> amount-left 0)
                          (incf (sogginess-of i) amount-left)
                          (setf amount-left 0)
                          (push i affected-clothes)))))))
    (setf (getf return-value :new-bladder-contents) (bladder/contents-of wetter))
    (setf (getf return-value :affected-clothes) affected-clothes)
    (setf (getf return-value :leak-amount) amount-left)
    (setf (getf return-value :wet-amount) amount))
  return-value)
(defunassert mess (&key (mess-amount t) force-fill-amount pants-down accident force-mess-amount (messer (player-of *game*)) (clothes nil clothes-supplied-p)
                        &aux (return-value ()) (affected-clothes ()) (amount nil) (clothes (if clothes-supplied-p clothes (wear-of messer))))
  (force-fill-amount (or null real)
                     force-mess-amount (or boolean real)
                     mess-amount (or boolean real)
                     messer base-character)
  #.(format nil "this function is mostly for mods, doesn't print text or diaper expansion, that's handled by other functions. @var{MESSER} is the instance of @code{BASE-CHARACTER} doing the messing. @var{MESS-AMOUNT} is the amount @var{MESSER} messes but won't mess if he/she can't go, passing @code{T} to @var{MESS-AMOUNT} means to use @code{(BOWELS/CONTENTS-OF MESSER)}, @var{FORCE-MESS-AMOUNT} causes @var{MESSER} to mess regardless. @var{FORCE-FILL-AMOUNT} will set @code{(BOWELS/CONTENTS-OF MESSER)} to that amount first. @var{PANTS-DOWN} is @code{T} if @var{MESSER} pulls his/her pants down first. @var{ACCIDENT} is @code{T} if the messing isn't intentional. If @var{CLOTHES} is passed, it will be the one @var{MESSER} messes, otherwise it will be @code{(wear-of @var{MESSER})}


~a."
            (xref wet :function))
  (when force-fill-amount
    (setf (bowels/contents-of messer) force-fill-amount))
  (cond (force-mess-amount
         (setf amount (cond ((eq force-mess-amount t)
                             (bowels/contents-of messer))
                            ((> force-mess-amount (bowels/contents-of messer))
                             (bowels/contents-of messer))
                            (t
                             force-mess-amount))))
        ((< (bowels/contents-of messer) (bowels/need-to-potty-limit-of messer))
         (return-from mess `(:old-bowels-contents ,(bowels/contents-of messer)
                             :new-bowels-contents ,(bowels/contents-of messer)
                             :affected-clothes ()
                             :leak-amount 0
                             :mess-amount 0)))
        (accident
         (setf amount (bowels/contents-of messer)))
        (t (setf amount (cond ((eq mess-amount t)
                               (bowels/contents-of messer))
                              ((> mess-amount (bowels/contents-of messer))
                               (bowels/contents-of messer))
                              (t
                               mess-amount)))))
  (setf (getf return-value :old-bowels-contents) (bowels/contents-of messer))
  (let* ((amount-left amount))
    (cond ((or pants-down (not (filter-items clothes 'closed-bottoms)))
           (decf (bowels/contents-of messer) amount)
           (setf amount-left 0))
          (t
           (decf (bowels/contents-of messer) amount)
           (iter (while (> amount-left 0))
                 (for i in (reverse clothes))
                 (when (typep i 'closed-bottoms)
                   (cond ((> amount-left (- (messiness-capacity-of i) (messiness-of i)))
                          (if (leakproofp i)
                              (setf amount-left 0)
                              (decf amount-left (- (messiness-capacity-of i) (messiness-of i))))
                          (setf (messiness-of i) (messiness-capacity-of i))
                          (push i affected-clothes))
                         ((> amount-left 0)
                          (incf (messiness-of i) amount-left)
                          (setf amount-left 0)
                          (push i affected-clothes)))))))
    (setf (getf return-value :new-bowels-contents) (bowels/contents-of messer))
    (setf (getf return-value :affected-clothes) affected-clothes)
    (setf (getf return-value :leak-amount) amount-left)
    (setf (getf return-value :mess-amount) amount))
  (setf (fart-count-of messer) 0)
  return-value)
(defunassert potty-on-toilet (prop &key wet mess pants-down (user (player-of *game*)))
  (prop yadfa-props:toilet
        wet (or boolean real)
        mess (or boolean real))
  (when (notany #'identity (list wet mess))
    (setf wet t
          mess t))
  (cond
    ((typep user '(not potty-trained-team-member))
     (write-line "Yeah, that's not going to happen")
     (return-from potty-on-toilet))
    ((not (funcall (coerce (can-potty-p (get-zone (position-of (player-of *game*)))) 'function)
                   prop
                   :wet wet
                   :mess mess
                   :pants-down pants-down
                   :user user))
     (return-from potty-on-toilet))
    ((and pants-down (iter (for i in (filter-items (wear-of user) 'closed-bottoms))
                           (when (lockedp i)
                             (format t "~a struggles to remove ~a ~a, realizes ~a can't, then starts panicking while doing a potty dance.~%"
                                     (name-of user)
                                     (if (malep user) "his" "her")
                                     (name-of i)
                                     (if (malep user) "he" "she"))
                             (leave t))))
     (return-from potty-on-toilet)))
  (let* ((mess-return-value (when mess
                              (mess :mess-amount mess :pants-down pants-down :messer user)))
         (wet-return-value (when wet
                             (wet :wet-amount wet :pants-down pants-down :wetter user))))
    (when (and (or (not wet-return-value) (and wet-return-value (= (getf wet-return-value :wet-amount) 0)))
               (or (not mess-return-value) (and mess-return-value (= (getf mess-return-value :mess-amount) 0))))
      (format t "~a doesn't have to go~%" (name-of user))
      (return-from potty-on-toilet))
    (if (or pants-down (not (filter-items (wear-of user) 'closed-bottoms)))
        (format t "~a used the ~a like a big ~a"
                (name-of user)
                (name-of prop)
                (if (malep user) "boy" "girl"))
        (let* ((names ())
               (out ()))
          (push (if (and wet-return-value (> (getf wet-return-value :wet-amount) 0)) "soggy butt" "mushy butt") names)
          (push (format nil "~a ~a"
                        (if (and wet-return-value (> (getf wet-return-value :wet-amount) 0)) "soggy" "mushy")
                        (if (malep user) "boy" "girl"))
                names)
          (when (and wet-return-value (> (getf wet-return-value :wet-amount) 0))
            (push (format nil "piddle ~a" (if (malep user) "prince" "princess")) names))
          (push (format nil "Looks like you missed a step ~a" (a:random-elt names)) out)
          (push (format nil "Aww, looks like the little ~a forgot to take ~a ~a first"
                        (let ((a names))
                          (push (format nil "baby ~a" (if (malep user) "boy" "girl")) a)
                          (a:random-elt a))
                        (if (malep user) "his" "her")
                        (cond ((filter-items (wear-of user) 'diaper)
                               "diapers")
                              ((filter-items (wear-of user) 'pullup)
                               "pullups")
                              (t "panties")))
                out)
          (format t "~a~%" (a:random-elt out))))))
(defunassert potty-on-self-or-prop (prop &key wet mess pants-down (user (player-of *game*)))
  (wet (or boolean real)
       mess (or boolean real))
  (when (notany #'identity (list wet mess))
    (setf wet t
          mess t))
  (cond ((and (typep user '(not potty-trained-team-member))
              pants-down)
         (write-line "Yeah, that's not going to happen")
         (return-from potty-on-self-or-prop))
        ((funcall (coerce (no-wetting/messing-of (get-zone (position-of (player-of *game*)))) 'function) user)
         (return-from potty-on-self-or-prop))
        ((not (funcall (coerce (can-potty-p (get-zone (position-of (player-of *game*)))) 'function)
                       prop
                       :wet wet
                       :mess mess
                       :pants-down pants-down
                       :user user))
         (return-from potty-on-self-or-prop))
        ((and pants-down (iter (for i in (filter-items (wear-of user) 'closed-bottoms))
                               (when (lockedp i)
                                 (format t "~a struggles to remove ~a ~a, realizes ~a can't, then starts panicking while doing a potty dance.~%"
                                         (name-of user)
                                         (if (malep user) "his" "her")
                                         (name-of i)
                                         (if (malep user) "he" "she"))
                                 (leave t))))
         (return-from potty-on-self-or-prop)))
  (let*
      ((mess-return-value (when mess
                            (mess :mess-amount mess :pants-down pants-down :messer user)))
       (wet-return-value (when wet
                           (wet :wet-amount wet :pants-down pants-down :wetter user)))
       (clothes (cond ((filter-items (wear-of user) 'diaper)
                       '("diapers" "pamps" "huggies" "pampers" "padding"))
                      ((filter-items (wear-of user) 'pullup)
                       '("pullups" "padding"))
                      ((filter-items (wear-of user) '(and undies bottoms (not incontinence-product)))
                       '("undies" "panties"))
                      (t '("pants")))))
    (when (and
           (or (not wet-return-value) (and wet-return-value (= (getf wet-return-value :wet-amount) 0)))
           (or (not mess-return-value) (and mess-return-value (= (getf mess-return-value :mess-amount) 0))))
      (format t "~a doesn't have to go~%" (name-of user))
      (return-from potty-on-self-or-prop))
    (let ((wet-list ())
          (mess-list ())
          (both-list ())
          (wet-leak-list ())
          (mess-leak-list ())
          (both-leak-list ()))
      (flet ((format-lists ()
               (cond ((and wet-return-value mess-return-value
                           (> (getf wet-return-value :wet-amount) 0)
                           (> (getf mess-return-value :mess-amount) 0)
                           both-list)
                      (format t "~a~%" (a:random-elt both-list)))
                     ((and mess-return-value (> (getf mess-return-value :mess-amount) 0) mess-list)
                      (format t "~a~%" (a:random-elt mess-list)))
                     ((and wet-return-value (> (getf wet-return-value :wet-amount) 0) wet-list)
                      (format t "~a~%" (a:random-elt wet-list))))
               (setf wet-list () mess-list () both-list()))
             (format-leak-lists ()
               (cond ((and
                       wet-return-value
                       mess-return-value
                       (> (getf wet-return-value :leak-amount) 0)
                       (> (getf mess-return-value :leak-amount) 0)
                       both-leak-list)
                      (format t "~a~%" (a:random-elt both-leak-list)))
                     ((and mess-return-value (> (getf mess-return-value :leak-amount) 0) mess-leak-list)
                      (format t "~a~%" (a:random-elt mess-leak-list)))
                     ((and wet-return-value (> (getf wet-return-value :leak-amount) 0) wet-leak-list)
                      (format t "~a~%" (a:random-elt wet-leak-list))))
               (setf wet-leak-list ()
                     mess-leak-list ()
                     both-leak-list())))
        (let* ((male (malep user))
               (hisher (if male "his" "her"))
               (name (name-of user)))
          (cond
            ;; player pulls his pants down then potty
            ((and pants-down (filter-items (wear-of user) 'closed-bottoms))
             (do-push (format nil "~a pulled down ~a ~a and went potty on the ~a"
                              name
                              hisher
                              (a:random-elt clothes)
                              (if prop
                                  (name-of prop)
                                  "floor"))
               both-list wet-list mess-list)
             (do-push (format nil "~a pulls down ~a ~a and marks ~a territory"
                              name
                              hisher
                              (a:random-elt clothes)
                              hisher)
               both-list wet-list mess-list)
             (push (format nil "~a pulled down ~a ~a and peed on the ~a"
                           name
                           hisher
                           (a:random-elt clothes)
                           (if prop
                               (name-of prop)
                               "floor"))
                   wet-list)
             (push (format nil "~a pulled down ~a ~a and squats down and mess"
                           name
                           hisher
                           (a:random-elt clothes))
                   mess-list)
             (do-push (format nil "Bad ~a! No going potty on the ~a!"
                              (species-of user)
                              (if prop
                                  (name-of prop)
                                  "floor"))
               wet-list mess-list both-list)
             (format-lists))
            ;; If the player specifies to pull his pants down without any on, assume he's intentionally going on the floor or prop
            (pants-down
             (push (format nil "~a goes potty on the ~a like an animal"
                           name
                           (if prop
                               (name-of prop)
                               "floor"))
                   both-list)
             (push (format nil "~a pees on the ~a like an animal"
                           name
                           (if prop
                               (name-of prop)
                               "floor"))
                   wet-list)
             (push (format nil "~a squats down and messes on the ~a like an animal"
                           name
                           (if prop
                               (name-of prop)
                               "floor"))
                   mess-list)
             (push (format nil "~a lifts ~a leg and pees on the ~a, then squats down on all fours and mess"
                           name
                           hisher
                           (if prop
                               (name-of prop)
                               "floor"))
                   both-list)
             (push (format nil
                           "~a lifts ~a leg and pees on the ~a"
                           name
                           hisher
                           (if prop
                               (name-of prop)
                               "floor"))
                   wet-list)
             (push (format nil "~a squat down on all fours and messes like an animal" (name-of user)) mess-list)
             (do-push (format nil "Bad ~a! No going potty on the ~a!"
                              (species-of user)
                              (if prop
                                  (name-of prop)
                                  "floor"))
               wet-list mess-list both-list)
             (format-lists))
            ;; otherwise assume the player is just standing there and lets go, possibly forgetting that he's not wearing padding
            ((not (filter-items (wear-of user) 'closed-bottoms))
             (if prop
                 (progn
                   (push (format nil "~a lifts ~a leg and pees on the ~a"
                                 name
                                 hisher
                                 (name-of prop))
                         wet-list)
                   (push (format nil "~a squats down on all fours and mess like an animal"
                                 name)
                         mess-list)
                   (push (format nil "~a lifts ~a leg and pees on the ~a, then squats down on all fours and mess"
                                 name
                                 hisher
                                 (name-of prop))
                         both-list)
                   (do-push (format nil "Bad ~a! No going potty on the ~a!"
                                    (species-of user)
                                    (name-of prop))
                     wet-list mess-list both-list))
                 (do-push (format nil "~a realized ~a made a horrible mistake. ~a weren't wearing any padding!!!"
                                  name
                                  (if male "he" "she")
                                  (if male "He" "She"))
                   both-list wet-list mess-list))
             (format-lists))
            ;; player is using his pants like a toilet
            (t
             (cond ((and (not prop) wet-return-value (< (getf wet-return-value :wet-amount) 30))
                    (push (format nil "~a lets a little out to relieve the pressure"
                                  name)
                          wet-list)
                    (push (format nil "Bad idea as ~a just made a puddle on the floor"
                                  (if (malep user) "he" "she"))
                          wet-leak-list)
                    (format-lists)
                    (format-leak-lists))
                   ((filter-items (wear-of user) 'diaper)
                    (when prop
                      (push (format nil "~a lifts ~a leg near the ~a and floods ~a pamps"
                                    name
                                    hisher
                                    (name-of prop)
                                    hisher)
                            wet-list)
                      (push (format nil "~a lifts ~a leg near the ~a and floods ~a pamps. Looks like the little ~a isn't house-trained"
                                    name
                                    hisher
                                    (name-of prop)
                                    hisher
                                    (species-of user))
                            wet-list)
                      (push (format nil "You lift your leg near the ~a and flood your pamps, then squat down on all fours and mess"
                                    (name-of prop))
                            both-list)
                      (push (format nil "~a squats down on all fours~a like an animal and messes ~a diapers"
                                    name
                                    (if (s:memq (car (tail-of user)) '(:medium :large))
                                        (format nil " with ~a tail raised"
                                                hisher)
                                        "")
                                    hisher)
                            mess-list))
                    (do-push (format nil "~a goes potty in ~a diapers like a toddler"
                                     name
                                     hisher)
                      wet-list mess-list both-list)
                    (when (>= (getf wet-return-value :wet-amount) (bladder/potty-desperate-limit-of user))
                      (do-push (format nil "after doing a potty dance like a 5 year old, ~a floods ~a diapers with a huge sigh of relief"
                                       name
                                       hisher)
                        wet-list))
                    (when (filter-items (wear-of user) '(and diaper ab-clothing))
                      (do-push (format nil "Aww, is the baby using ~a diapers?"
                                       hisher)
                        wet-list mess-list both-list))
                    (push (format nil "~a pauses and floods ~a diapers"
                                  name
                                  hisher)
                          wet-list)
                    (push (format nil "~a squats down~a and fills ~a diapers"
                                  name
                                  (if (s:memq (car (tail-of user)) '(:medium :large))
                                      (format nil " with ~a tail raised"
                                              hisher)
                                      "")
                                  hisher)
                          mess-list)
                    (push (format nil "heh, the baby blorted ~a diapers" hisher) mess-list)
                    (push (format nil "~a diapers sprung a leak" name) wet-leak-list)
                    (do-push (format nil
                                     "~a's diapers leak all over, there goes the carpet" name)
                      wet-leak-list mess-leak-list both-leak-list)
                    (push (format nil "Blowout!!!") mess-leak-list)
                    (push (format nil "Heh, baby made a puddle") wet-leak-list)
                    (push (format nil "~a piddles ~a pamps" name hisher) wet-list))
                   ((filter-items (wear-of user) 'pullup)
                    (when prop
                      (push (format nil "~a lifts ~a leg near the ~a and floods ~a pullups"
                                    name
                                    hisher
                                    (name-of prop)
                                    hisher)
                            wet-list)
                      (push (format nil "~a lifts ~a leg near the ~a and floods ~a pullups, then squats down on all fours and messes"
                                    name
                                    hisher
                                    (name-of prop)
                                    hisher)
                            both-list)
                      (when (s:memq (car (tail-of user)) '(:medium :large))
                        (push (format nil "~a squats down on all fours with ~a tail raised like an animal and messes ~a pullups"
                                      name
                                      hisher
                                      hisher)
                              mess-list)))
                    (do-push (format nil
                                     "~a's pullups leak all over, there goes the carpet" name)
                      wet-leak-list mess-leak-list both-leak-list)
                    (when (filter-items (wear-of user) '(and pullup ab-clothing))
                      (do-push
                          (format nil "Bad ~a! You know you're supposed to use the toilet like a big kid"
                                  (if male "boy" "girl"))
                        wet-list mess-list both-list)
                      (when (>= (getf wet-return-value :wet-amount) (bladder/potty-desperate-limit-of user))
                        (do-push (format nil "after doing a potty dance like a 5 year old, ~a floods ~a pullups with a huge sigh of relief and just hopes no one will notice"
                                         name
                                         hisher)
                          wet-list))
                      (do-push (format nil "Bad ~a! Look at the mess you made leaking everywhere like that! Do we have to put you back in diapers?!"
                                       (if (malep user) "boy" "girl"))
                        wet-leak-list mess-leak-list both-leak-list))
                    (push (format nil "~a squats down and messes ~a pullups like a toddler"
                                  name
                                  hisher)
                          mess-list)
                    (do-push (format nil "~a goes potty in ~a pullups like a toddler"
                                     name
                                     hisher)
                      mess-list wet-list both-list)
                    (push (format nil "~a pauses and floods ~a pullups"
                                  name
                                  hisher)
                          wet-list)
                    (push (format nil "~a floods ~a pullups like a toddler"
                                  name
                                  hisher)
                          wet-list)
                    (push (format nil "~a mess falls out of ~a pullups and on the floor"
                                  name
                                  hisher)
                          mess-leak-list)
                    (push (format nil "~a's pullups sprung a leak"
                                  name)
                          wet-leak-list)
                    (push (format nil "~a makes a puddle"
                                  name)
                          wet-leak-list)
                    (when (eq user (player-of *game*))
                      (push (format nil "You made a puddle on the floor. You sure you're ready for pullups?")
                            wet-leak-list)))
                   ((filter-items (wear-of user) 'stuffer)
                    (push (format nil "~a floods ~aself like a toddler"
                                  name
                                  hisher)
                          wet-list)
                    (push (format nil "~a squats down and mess ~aself like a toddler"
                                  name
                                  hisher)
                          mess-list)
                    (do-push (format nil "~a goes potty in ~a pants like a toddler"
                                     name
                                     hisher)
                      wet-list mess-list both-list)
                    (push (format nil "A puddle forms on the floor. Maybe ~a should start wearing diapers"
                                  (if (eq user (player-of *game*)) "you" name))
                          wet-leak-list)
                    (do-push (format nil "Heh, baby made a mess on the floor")
                      wet-leak-list mess-leak-list both-leak-list))
                   (t
                    (do-push (format nil "~a realized ~a made a horrible mistake. ~a's not wearing any padding!!!"
                                     name
                                     (if (malep user) "he" "she")
                                     (if (malep user) "He" "She"))
                      wet-leak-list mess-leak-list both-leak-list)
                    (push (format nil "~a flood ~aself like a toddler"
                                  (name-of user)
                                  (if (malep user) "him" "her"))
                          wet-list)
                    (push (format nil "~a squats down and messes ~aself like a toddler"
                                  (name-of user)
                                  (if (malep user) "him" "her"))
                          mess-list)
                    (do-push (format nil "~a goes potty in ~a pants like a toddler"
                                     (name-of user)
                                     hisher)
                      wet-list mess-list both-list)
                    (push (format nil "A puddle forms on the floor. Maybe ~a should start wearing diapers"
                                  (if (eq user (player-of *game*)) "you" name))
                          wet-leak-list)
                    (do-push (format nil "Heh, baby made a mess on the floor")
                      wet-leak-list mess-leak-list both-leak-list)))
             (format-lists)
             (format-leak-lists)
             (multiple-value-bind (value key)
                 (pop-from-expansion user (cons wet-return-value mess-return-value))
               (when (eq key :wet/mess)
                 (setf wet-return-value (car value)
                       mess-return-value (cdr value))))
             (funcall (coerce (potty-trigger-of (get-zone (position-of (player-of *game*)))) 'function)
                      (cons wet-return-value mess-return-value) user))))))))
(defunassert process-potty (&optional (user (player-of *game*)))
  (user (or player ally))
  (let ((time-difference (- (time-of *game*) (last-process-potty-time-of user))))
    (fill-bladder user :times time-difference)
    (fill-bowels user :times time-difference))
  (setf (last-process-potty-time-of user) (time-of *game*))
  (let ((had-accident (if (typep user 'potty-trained-team-member)
                          (cons (when (>= (bladder/contents-of user) (bladder/maximum-limit-of user))
                                  (wet :accident t :wetter user))
                                (when (>= (bowels/contents-of user) (bowels/maximum-limit-of user))
                                  (mess :accident t :messer user)))
                          (cons (when (>= (bladder/contents-of user) (bladder/need-to-potty-limit-of user))
                                  (wet :wetter user))
                                (when (>= (bowels/contents-of user) (bowels/need-to-potty-limit-of user))
                                  (mess :messer user))))))
    (iter (for i in '(:wet :mess))
          (output-process-potty-text user
                                     (get-babyish-padding user)
                                     i
                                     (get-process-potty-action-type user
                                                                    i
                                                                    had-accident)
                                     had-accident))
    (multiple-value-bind
          (value key)
        (pop-from-expansion user had-accident)
      (when (eq key :wet/mess)
        (setf had-accident value)))
    (funcall (coerce (potty-trigger-of (get-zone (position-of (player-of *game*))))
                     'function)
             had-accident user)
    had-accident))
(defun coerce-element-types (element-types)
  (iter (for element-type in (a:ensure-list element-types))
    (collect (coerce-element-type element-type))))
(defun get-props-from-zone (position)
  (props-of (get-zone position)))
(defun get-items-from-prop (prop position)
  (items-of (getf (get-props-from-zone position) prop)))
(defun get-bitcoins-from-prop (prop position)
  (bitcoins-of (getf (get-props-from-zone position) prop)))
(defun (setf get-items-from-prop) (new-value prop position)
  (setf (items-of (getf (get-props-from-zone position) prop)) new-value))
(defun (setf get-bitcoins-from-prop) (new-value prop position)
  (setf (bitcoins-of (getf (get-props-from-zone position) prop)) new-value))
(defun (setf get-props-from-zone) (new-value position)
  (setf (props-of (eval (get-zone position))) new-value))
(defun pushnewmove (move* user)
  (pushnew (make-instance move*) (moves-of user)
           :test (lambda (a b)
                   (eq (class-name (class-of a)) (class-name (class-of b))))))
(defun get-move (move* user)
  (find move* (moves-of user)
        :test (lambda (a b)
                (if (typep a 'keyword)
                    (string= a (class-name (class-of b)))
                    (eq a (class-name (class-of b)))))))
(defunassert calculate-diaper-usage (user)
  (user base-character)
  (iter
   (with sogginess = 0)
   (with sogginess-capacity = 0)
   (with messiness = 0)
   (with messiness-capacity = 0)
   (for i in (wear-of user))
   (when (typep i 'closed-bottoms)
     (incf sogginess (sogginess-of i))
     (incf sogginess-capacity (sogginess-capacity-of i))
     (incf messiness (messiness-of i))
     (incf messiness-capacity (messiness-capacity-of i)))
   (finally (return `(:sogginess ,sogginess :sogginess-capacity ,sogginess-capacity
                      :messiness ,messiness :messiness-capacity ,messiness-capacity)))))
(defunassert calculate-diaper-usage* (clothes)
  (clothes list)
  (iter
   (with sogginess = 0)
   (with sogginess-capacity = 0)
   (with messiness = 0)
   (with messiness-capacity = 0)
   (for i in clothes)
   (when (typep i 'closed-bottoms)
     (incf sogginess (sogginess-of i))
     (incf sogginess-capacity (sogginess-capacity-of i))
     (incf messiness (messiness-of i))
     (incf messiness-capacity (messiness-capacity-of i)))
   (finally (return `(:sogginess ,sogginess :sogginess-capacity ,sogginess-capacity
                      :messiness ,messiness :messiness-capacity ,messiness-capacity)))))
(defunassert calculate-level-to-exp (level)
  (level real)
  (floor (/ (* 4 (expt level 3)) 5)))
(defunassert calculate-exp-yield (target)
  (target enemy)
  (u:$ (exp-yield-of target) * (level-of target) / 7))
(defunassert calculate-wear-stats (user)
  (user base-character)
  (iter
   (with j = (list :health 0 :attack 0 :defense 0 :energy 0 :speed 0))
   (for i in (wear-of user))
   (iter
    (for (a b) on (wear-stats-of i) by #'cddr)
    (incf (getf j a) b))
   (finally (return j))))
(defunassert calculate-wield-stats (user)
  (user base-character)
  (iter
   (with j = (list :health 0 :attack 0 :defense 0 :energy 0 :speed 0))
   (for (a b) on (if (wield-of user) (wield-stats-of (wield-of user)) ()) by #'cddr)
   (incf (getf j a) b)
   (finally (return j))))
(defunassert calculate-stat-delta (user)
  (user base-character)
  (iter
   (with j = (list :health 0 :attack 0 :defense 0 :energy 0 :speed 0))
   (for i in (when *battle* (getf (status-conditions-of *battle*) user)))
   (iter
    (for (a b) on (stat-delta-of i) by #'cddr)
    (incf (getf j a) b))
   (finally (return j))))
(defunassert calculate-stat-multiplier (user)
  (user base-character)
  (iter
   (with j = (list :health 1 :attack 1 :defense 1 :energy 1 :speed 1))
   (for i in (when *battle* (getf (status-conditions-of *battle*) user)))
   (iter
    (for (a b) on (stat-multiplier-of i) by #'cddr)
    (declare (ignorable b))
    (setf (getf j a) (* (getf j a))))
   (finally (return j))))
(defunassert calculate-stat (user stat-key)
  (user base-character)
  (round (if (or (eq stat-key :health) (eq stat-key :energy))
             (u:$ (u:$ (u:$ (u:$ (u:$ (getf (base-stats-of user) stat-key) +
                                      (getf (iv-stats-of user) stat-key) +
                                      (getf (calculate-wear-stats user) stat-key) +
                                      (getf (calculate-wield-stats user) stat-key) +
                                      (getf (calculate-stat-delta user) stat-key))
                                 * (getf (calculate-stat-multiplier user) stat-key)
                                 * 2)
                            * (level-of user))
                       / 100)
                  + (level-of user) + 10)
             (u:$ (u:$ (u:$ (u:$ (u:$ (getf (base-stats-of user) stat-key) +
                                      (getf (iv-stats-of user) stat-key) +
                                      (getf (calculate-wear-stats user) stat-key) +
                                      (getf (calculate-wield-stats user) stat-key) +
                                      (getf (calculate-stat-delta user) stat-key))
                                 * (getf (calculate-stat-multiplier user) stat-key)
                                 * 2)
                            * (level-of user))
                       / 100)
                  + 5))))
(defun present-stats (user)
  (updating-present-with-effective-frame (*query-io* :unique-id `(stats% ,user) :id-test #'equal)
                                         (clim:updating-output (*query-io*)
                                                               (clim:present user (type-of user) :view yadfa-clim:+stat-view+))))
(defun describe-item (item &optional wear)
  (format t
          "Name: ~a~%Resale Value: ~f~%Description:~%~a~%"
          (name-of item)
          (/ (value-of item) 2)
          (description-of item))
  (if wear
      (describe-diaper-wear-usage item)
      (describe-diaper-inventory-usage item))
  (describe-diaper-usage item)
  (when (typep item 'weapon)
    (format t "Ammo Type: ~s" (ammo-type-of item)))
  (when (special-actions-of item)
    (iter (for (a b) on (special-actions-of item) by #'cddr)
          (format t "Keyword: ~a~%Other Parameters: ~w~%Documentation: ~a~%~%Describe: ~a~%~%"
                  a
                  (cddr (lambda-list (action-lambda b)))
                  (documentation b t)
                  (with-output-to-string (s)
                    (let ((*standard-output* s))
                      (describe (action-lambda b)))))))
  t)
(defun finish-battle (&optional lose &aux (player (player-of *game*)) (male (malep player)) (name (name-of player))
                                       (position (position-of player)) (enemies (enemies-of *battle*)) (team (team-of *game*)))
  (if lose
      (progn (format t "~a was defeated~%" name)
             (setf (position-of player) (warp-on-death-point-of player))
             (format t
                     "~a blacked out and flooded and messed ~aself~%~a wakes up and looks at ~a GPS to find out ~a's at ~a at ~a~%"
                     name
                     (if male "him" "her")
                     name
                     (if male "his" "her")
                     (if male "he" "she")
                     (name-of (get-zone position))
                     position)
             (iter (for user in (cons player (allies-of *game*)))
                   (setf (health-of user) (calculate-stat user :health))
                   (setf (energy-of user) (calculate-stat user :energy)))
             (let ((exp-gained (/ (iter (for enemy in enemies)
                                        (with j = 0)
                                        (incf j (calculate-exp-yield enemy))
                                        (finally (return j)))
                                  2)))
               (iter (for team-member in team)
                     (incf (exp-of team-member) exp-gained)
                     (let ((old-level (level-of team-member)))
                       (iter (while (>= (exp-of team-member) (calculate-level-to-exp (+ (level-of team-member) 1))))
                             (incf (level-of team-member)))
                       (when (> (level-of team-member) old-level)
                         (format t "~a level-uped to ~d~%" (name-of team-member) (level-of team-member))
                         (iter (for level from (1+ old-level) to (level-of team-member))
                               (iter (for learned-move in (learned-moves-of team-member))
                                     (when (= (car learned-move) level)
                                       (unless (get-move (cdr learned-move) team-member)
                                         (pushnewmove (cdr learned-move) team-member)
                                         (format t "~a learned ~a~%" (name-of team-member) (name-of (get-move (cdr learned-move) team-member))))))))))
               (setf *battle* nil))
             (iter (for team-member in team)
                   (wet :force-fill-amount (bladder/maximum-limit-of team-member))
                   (mess :force-fill-amount (bowels/maximum-limit-of team-member))))
      (progn (format t "~a won the battle~%~%" name)
             (let ((items-looted (iter (for enemy in enemies)
                                       (with j = ())
                                       (setf j (append* j (inventory-of enemy) (wear-of enemy)))
                                       (setf (inventory-of enemy) nil
                                             (wear-of enemy) nil)
                                       (finally (return j))))
                   (bitcoins-looted (iter (for enemy in enemies)
                                          (with j = 0)
                                          (incf j (if (bitcoins-per-level-of enemy) (* (bitcoins-per-level-of enemy) (level-of enemy)) (bitcoins-of enemy)))
                                          (finally (return j))))
                   (exp-gained (iter (for enemy in enemies)
                                     (with j = 0)
                                     (incf j (calculate-exp-yield enemy))
                                     (finally (return j))))
                   (win-events (win-events-of *battle*)))
               (iter (for team-member in team)
                     (incf (exp-of team-member) exp-gained)
                     (let ((old-level (level-of team-member)))
                       (iter (while (>= (exp-of team-member) (calculate-level-to-exp (+ (level-of team-member) 1))))
                             (incf (level-of team-member)))
                       (when (> (level-of team-member) old-level)
                         (format t "~a level-uped to ~d~%" (name-of team-member) (level-of team-member))
                         (iter (for level from (1+ old-level) to (level-of team-member))
                               (iter (for learned-move in (learned-moves-of team-member))
                                     (when (= (car learned-move) level)
                                       (unless (get-move (cdr learned-move) team-member)
                                         (pushnewmove (cdr learned-move) team-member)
                                         (format t "~a learned ~a~%" (name-of team-member) (name-of (get-move (cdr learned-move) team-member))))))))))
               (cond ((and items-looted (> bitcoins-looted 0))
                      (format t "~a loots ~d bitcoins and ~d ~a from the enemies~%"
                              name
                              bitcoins-looted
                              (list-length items-looted)
                              (if (= (list-length items-looted) 1)
                                  "item"
                                  "items")))
                     (items-looted
                      (format t "~a loots ~d ~a from the enemy~%"
                              name
                              (list-length items-looted)
                              (if (= (list-length items-looted) 1)
                                  "item"
                                  "items")))
                     ((> bitcoins-looted 0)
                      (format t "~a loots ~d bitcoins from the enemy~%" name bitcoins-looted)))
               (incf (bitcoins-of player) bitcoins-looted)
               (a:nconcf (inventory-of player) items-looted)
               (setf *battle* nil)
               (setf (continue-battle-of (get-zone position)) nil)
               (trigger-event win-events))))
  (switch-user-packages))
(defun wash (clothing)
  (declare (type list clothing))
  (iter (for i in (filter-items clothing 'closed-bottoms))
        (when (not (disposablep i))
          (setf (sogginess-of i) 0 (messiness-of i) 0))))
(defun go-to-sleep% (user)
  (incf (time-of *game*) 60)
  (let ((time-difference (- (time-of *game*) (last-process-potty-time-of user))))
    (fill-bladder user :times time-difference)
    (fill-bowels user :times time-difference))
  (setf (health-of user) (calculate-stat user :health)
        (last-process-potty-time-of user) (time-of *game*)
        (energy-of user) (calculate-stat user :energy))
  (cons (wet :wetter user) (mess :messer user)))
(defun go-to-sleep ()
  (iter (for i in (cons (player-of *game*) (allies-of *game*)))
        (let* ((return-value (go-to-sleep% i))
               (out ())
               (male (malep i))
               (hisher (if male "his" "her"))
               (name (name-of i))
               (cheshe (if male "He" "She")))
          (multiple-value-bind (value key)
              (pop-from-expansion i return-value)
            (when (eq key :wet/mess)
              (setf return-value value)))
          (format t "~a wakes up " (name-of i))
          (when (> (getf (car return-value) :wet-amount) 0)
            (cond ((filter-items (wear-of i) 'diaper)
                   (if (> (getf (car return-value) :leak-amount) 0)
                       (progn (push (format nil "feeling all cold and soggy. ~a checks ~a diaper and to ~a embarrassment finds out it's leaking profusely. Seems ~a wet the bed.~%"
                                            cheshe
                                            hisher
                                            hisher
                                            name)
                                    out)
                              (format t "~a" (a:random-elt out))
                              (setf out ()))
                       (progn (push (format nil "and hears a squish . ~a looks down at ~a diaper, notices that it's soggy and then folds ~a ears back and blushes. Looks like ~a wet the bed~%"
                                            cheshe
                                            hisher
                                            hisher
                                            name)
                                    out)
                              (push (format nil "and looks down and pokes ~a diaper, then gets all blushy when it squishes. Seems ~a wet the bed~%"
                                            hisher
                                            name)
                                    out)
                              (format t "~a" (a:random-elt out))
                              (setf out ()))))
                  ((filter-items (wear-of i) 'pullup)
                   (if (> (getf (car return-value) :leak-amount) 0)
                       (progn (push (format nil "feeling all cold and soggy. ~a checks ~a pullups and to ~a embarrassment finds out it's leaking profusely. Seems ~a wet the bed.~%"
                                            cheshe
                                            hisher
                                            hisher
                                            name)
                                    out)
                              (format t "~a" (a:random-elt out))
                              (setf out ()))
                       (progn (push (format nil "and hears a squish. ~a looks down at ~a pullups, notices that ~a and then folds ~a ears back and blushes. Looks like ~a wet the bed~%"
                                            cheshe
                                            (if (filter-items (wear-of i) '(ab-clothing pullup))
                                                "the little pictures have faded"
                                                "it's soggy")
                                            hisher
                                            hisher
                                            name)
                                    out)
                              (format t "~a" (a:random-elt out))
                              (setf out ()))))
                  ((filter-items (wear-of i) 'stuffer)
                   (if (> (getf (car return-value) :leak-amount) 0)
                       (progn (push (format nil "feeling all cold and soggy. ~a notices ~a PJs, the padding under ~a PJs, and bed are soaked. Seems ~a wet the bed~%"
                                            cheshe
                                            hisher
                                            hisher
                                            name)
                                    out)
                              (format t "~a" (a:random-elt out))
                              (setf out ()))
                       (progn (push (format nil "and hears a squish from under ~a PJs. ~a checks the incontinence pad under them and notices that they're soaked and then folds ~a ears back and blushes. Looks like ~a wet the bed~%"
                                            hisher
                                            cheshe
                                            hisher
                                            name)
                                    out)
                              (format t "~a" (a:random-elt out))
                              (setf out ()))))
                  ((filter-items (wear-of i) 'closed-bottoms)
                   (push (format nil "feeling all cold and soggy. ~a notices ~a PJs and bed are soaked then folds ~a ears back and blushes. Seems ~a wet the bed~%"
                                 cheshe
                                 hisher
                                 hisher
                                 name)
                         out)
                   (format t "~a" (a:random-elt out))
                   (setf out ()))
                  (t
                   (push (format nil "feeling all cold and soggy. ~a notices the bed is soaked then folds ~a ears back and blushes. Seems ~a wet the bed~%"
                                 cheshe
                                 hisher
                                 name)
                         out)
                   (format t "~a" (a:random-elt out))
                   (setf out ()))))
          (when (and (> (getf (cdr return-value) :mess-amount) 0) (> (getf (car return-value) :wet-amount) 0))
            (format t "~a is also " (name-of i)))
          (when (> (getf (cdr return-value) :mess-amount) 0)
            (cond
              ((filter-items (wear-of i) 'diaper)
               (if (> (getf (cdr return-value) :leak-amount) 0)
                   (progn
                     (push (format nil
                                   "feeling all mushy. ~a notices to ~a embarrassment that ~a diaper is leaking poo all over the bed. Seems ~a messed the bed~%"
                                   cheshe
                                   hisher
                                   hisher
                                   name)
                           out)
                     (format t "~a" (a:random-elt out))
                     (setf out ()))
                   (progn
                     (push (format nil
                                   "feeling all mushy. ~a notices to ~a embarrassment that ~a diaper is filled with poo. Seems ~a messed the bed~%"
                                   cheshe
                                   hisher
                                   hisher
                                   name)
                           out)
                     (format t "~a" (a:random-elt out))
                     (setf out ()))))
              ((filter-items (wear-of i) 'pullup)
               (if (> (getf (cdr return-value) :leak-amount) 0)
                   (progn
                     (push (format nil
                                   "feeling all mushy. ~a notices to ~a embarrassment that ~a pullups is leaking poo all over the bed. Seems ~a messed the bed~%"
                                   cheshe
                                   hisher
                                   hisher
                                   name)
                           out)
                     (format t "~a" (a:random-elt out))
                     (setf out ()))
                   (progn
                     (push (format nil
                                   "feeling all mushy. ~a notices to ~a embarrassment that ~a pullup is filled with poo. Seems ~a messed the bed~%"
                                   cheshe
                                   hisher
                                   hisher
                                   name)
                           out)
                     (format t "~a" (a:random-elt out))
                     (setf out ()))))
              ((filter-items (wear-of i) 'stuffer)
               (if (> (getf (cdr return-value) :leak-amount) 0)
                   (progn
                     (push (format nil
                                   "feeling all mushy. ~a notices to ~a embarrassment that ~a incontinence pad is leaking poo all over the bed and PJs. Seems ~a messed the bed~%"
                                   cheshe
                                   hisher
                                   hisher
                                   name)
                           out)
                     (format t "~a" (a:random-elt out))
                     (setf out ()))
                   (progn
                     (push (format nil
                                   "feeling all mushy. ~a notices to ~a embarrassment that ~a incontinence pad is filled with poo. Seems ~a messed the bed~%"
                                   cheshe
                                   hisher
                                   hisher
                                   name)
                           out)
                     (format t "~a" (a:random-elt out))
                     (setf out ()))))
              ((filter-items (wear-of i) 'closed-bottoms)
               (push (format nil
                             "feeling all mushy. ~a notices to ~a embarrassment that ~a PJs have poo in them and is getting on the bed. Seems ~a messed the bed~%"
                             cheshe
                             hisher
                             hisher
                             name)
                     out)
               (format t "~a" (a:random-elt out))
               (setf out ()))
              (t
               (push (format nil
                             "feeling all mushy. ~a notices to ~a embarrassment that ~a bed has poo on it. Seems ~a messed the bed~%"
                             cheshe
                             hisher
                             hisher
                             name)
                     out)
               (format t "~a" (a:random-elt out))
               (setf out ()))))))
  t)
(defunassert shopfun (items-for-sale &key items-to-buy items-to-sell user format-items)
  (user (or base-character null)
        items-to-buy (or list boolean)
        items-to-sell (or list boolean)
        items-for-sale list)
  (when items-to-buy
    (if (eq items-to-buy t)
        (let (item quantity)
          (accept-with-effective-frame (clim:accepting-values (*query-io* :resynchronize-every-pass t)
                                                              (fresh-line *query-io*)
                                                              (setf item (clim:accept `(clim:member-alist ,(iter (for i in items-for-sale)
                                                                                                                 (collect (list (name-of (apply 'make-instance (car i) (eval (cdr i))))
                                                                                                                                i)))) :prompt "Item"
                                                                                                                                      :view (make-instance 'clim:radio-box-view
                                                                                                                                                           :orientation :vertical)
                                                                                                                                      :stream *query-io*))
                                                              (fresh-line *query-io*)
                                                              (setf quantity (clim:accept 'string :prompt "Quantity"
                                                                                                  :view clim:+text-field-view+ :stream *query-io*))))
          (when (and quantity item (handler-case (if (typep (parse-integer quantity) '(integer 1 *))
                                                     t
                                                     (progn (format t "Quantity must be an '(integer 1 *)~%")
                                                            nil))
                                     (parse-error () (format t "does ~w look like an integer to you?~%" quantity)
                                       nil)))
            (setf quantity (parse-integer quantity))
            (let ((temp (apply 'make-instance (car item) (eval (cdr item)))))
              (cond ((> (* (value-of temp) quantity) (bitcoins-of user))
                     (format t "You don't have enough bitcoins to buy ~a~%"
                             (if (= quantity 1)
                                 (format nil "that ~a" (name-of item))
                                 (format nil "~d ~a"
                                         quantity
                                         (if (plural-name-of temp)
                                             (plural-name-of temp)
                                             (format nil "~as" (name-of temp)))))))
                    (t (dotimes (j quantity)
                         (push (apply #'make-instance
                                      (car item)
                                      (eval (cdr item)))
                               (inventory-of user)))
                       (decf (bitcoins-of user) (* (value-of temp) quantity))
                       (format t "You buy ~d ~a for ~f bitcoins~%"
                               quantity
                               (or (plural-name-of temp) (format nil "~as" (name-of temp)))
                               (* (value-of temp) quantity)))))))
        (iter (for i in items-to-buy)
              (let ((item (when (list-length-<= (car i) items-for-sale)
                            (apply #'make-instance
                                   (car (nth (car i) items-for-sale))
                                   (eval (cdr (nth (car i) items-for-sale)))))))
                (cond ((not item)
                       (format t "item ~d doesn't exist~%" (car i)))
                      ((> (* (value-of item) (cdr i)) (bitcoins-of user))
                       (format t "You don't have enough bitcoins to buy ~a~%"
                               (if (= (cdr i) 1)
                                   (format nil "that ~a" (name-of item))
                                   (format nil "~d ~a"
                                           (cdr i)
                                           (if (plural-name-of item)
                                               (plural-name-of item)
                                               (format nil "~as" (name-of item)))))))
                      (t (dotimes (j (cdr i))
                           (push (apply #'make-instance
                                        (car (nth (car i) items-for-sale))
                                        (eval (cdr (nth (car i) items-for-sale))))
                                 (inventory-of user)))
                         (decf (bitcoins-of user) (* (value-of item) (cdr i)))
                         (format t "You buy ~d ~a for ~f bitcoins~%"
                                 (cdr i)
                                 (or (plural-name-of item) (format nil "~as" (name-of item)))
                                 (* (value-of item) (cdr i)))))))))
  (when items-to-sell
    (if (eq items-to-sell t)
        (let (items)
          (accept-with-effective-frame (clim:accepting-values (*query-io* :resynchronize-every-pass t)
                                                              (setf items (clim:accept `(clim:subset-alist ,(iter (for item in (remove-duplicates (inventory-of user)))
                                                                                                                  (collect (cons (name-of item)
                                                                                                                                 item)))) :prompt "Items"
                                                                                                                                          :view clim:+check-box-view+ :stream *query-io*))))
          (iter (for i in items)
                (format t "You sell your ~a for ~f bitcoins~%"
                        (name-of i)
                        (/ (value-of i) 2))
                (incf (bitcoins-of user) (/ (value-of i) 2)))
          (a:deletef (the list (inventory-of user)) items :test (lambda (o e)
                                                                  (s:memq e o))))
        (let ((items (sort (remove-duplicates items-to-sell) #'<)))
          (setf items (iter (generate i in items)
                            (for j in (inventory-of user))
                            (for (the fixnum k) upfrom 0)
                            (when (first-iteration-p)
                              (next i))
                            (when (= k i)
                              (collect j)
                              (next i))))
          (unless items
            (format t "Those items aren't valid")
            (return-from shopfun))
          (iter (for i in items)
                (when (not (sellablep i))
                  (format t "That item isn't sellable~%~%")
                  (return-from shopfun)))
          (iter (for i in items)
                (format t "You sell your ~a for ~f bitcoins~%"
                        (name-of (nth i (inventory-of user)))
                        (/ (value-of (nth i (inventory-of user))) 2))
                (incf (bitcoins-of user) (/ (value-of i) 2)))
          (a:deletef (the list (inventory-of user)) items
                     :test (lambda (o e)
                             (s:memq e o))))))
  (when format-items
    (format t "~10a~40a~10@a~%" "Index" "Item" "Price")
    (iter (for i in items-for-sale)
          (for (the fixnum j) upfrom 0)
          (let ((item (apply #'make-instance (car i) (eval (cdr i)))))
            (format t "~10a~40a~10@a~%" j (name-of item) (value-of item))))))

(defun getf-action-from-prop (position prop action)
  (getf (actions-of (getf (get-props-from-zone position) prop)) action))
(defun (setf getf-action-from-prop) (new-value position prop action)
  (setf (getf (actions-of (getf (get-props-from-zone position) prop)) action) new-value))
(defunassert wash-in-washer (washer)
  (washer (or yadfa-props:washer null))
  "washes your dirty diapers and all the clothes you've ruined. WASHER is an instance of a washer you want to put your clothes in."
  (declare (ignorable washer))
  (wash (inventory-of (player-of *game*)))
  (write-line "You washed all your soggy and messy clothing. Try not to wet and mess them next time"))
(defun process-battle (&key attack item reload no-team-attack selected-target)
  "function used to iterate through the battle. @var{ATTACK} is the key of an attack or @code{T} to use the default attack. Pass @code{NIL} if the team member isn't attacking. @var{ITEM} is the item the team member is using, pass @code{NIL} if the team member isn't using an item. @var{RELOAD} is the type specifier of the ammo the team member is using to reload his/her weapon. Pass @code{NIL} if the team member isn't reloading a weapon. set @var{NO-TEAM-ATTACK} to T if the team member isn't attacking first. @var{SELECTED-TARGET} is the target the team member is attacking. set this to @code{NIL} if the team member isn't attacking someone."
  (declare (type (or enemy team-member null) selected-target)
           (type (or symbol boolean) attack)
           (type type-specifier reload)
           (type (or item null) item))
  (fresh-line)
  (when (and (not attack) (not item))
    (write-line "You need to either specify an attack or an item to use")
    (return-from process-battle))
  (let* ((ret nil)
         (team-attacked no-team-attack))
    (flet ((check-if-done ()
             (s:run-hooks '*cheat-hooks*)
             (iter (for i in (append (enemies-of *battle*) (team-of *game*)))
                   (if (<= (health-of i) 0)
                       (progn (setf (health-of i) 0)
                              (unless (s:memq i (fainted-of *battle*))
                                (format t "~a has fainted~%~%" (name-of i))
                                (pushnew i (fainted-of *battle*)))
                              (a:deletef (turn-queue-of *battle*) i))
                       (a:deletef (fainted-of *battle*) i :count 1))
                   (when (> (health-of i) (calculate-stat i :health))
                     (setf (health-of i) (calculate-stat i :health)))
                   (when (> (energy-of i) (calculate-stat i :energy))
                     (setf (energy-of i) (calculate-stat i :energy))))
             (unless (iter (for i in (team-of *game*)) (when (> (health-of i) 0) (leave t)))
               (finish-battle t)
               (return-from process-battle t))
             (unless (iter (for i in (enemies-of *battle*))
                           (when (> (health-of i) 0)
                             (leave t)))
               (finish-battle)
               (return-from process-battle t))))
      (check-if-done)
      (unless (or (eq attack t) (get-move attack (first (turn-queue-of *battle*))))
        (format t "~a doesn't know ~a~%" (name-of (first (turn-queue-of *battle*))) attack)
        (return-from process-battle))
      (when item
        (multiple-value-bind (cant-use plist) (cant-use-p item (car (turn-queue-of *battle*)) selected-target nil)
          (when cant-use
            (destructuring-bind (&key format-control format-arguments &allow-other-keys) plist
              (if format-control
                  (apply 'format t format-control format-arguments)
                  (write-line "You can't do that with that item"))
              (return-from process-battle)))))
      (when (and (not (eq attack t)) (< (energy-of (first (turn-queue-of *battle*))) (energy-cost-of (get-move attack (first (turn-queue-of *battle*))))))
        (format t "~a doesn't have enough energy to use ~a~%"
                (name-of (first (turn-queue-of *battle*))) (name-of (get-move attack (first (turn-queue-of *battle*)))))
        (return-from process-battle))
      (iter (until (and team-attacked (typep (first (turn-queue-of *battle*)) 'team-member)))
            (check-if-done)
            (let* ((current-character (pop (turn-queue-of *battle*)))
                   (new-ret (process-battle-turn current-character attack item reload selected-target)))
              (iter (for i in (append (team-of *game*) (team-npcs-of *battle*) (enemies-of *battle*)))
                    (pop-from-expansion i))
              (when (typep current-character '(not npc))
                (setf team-attacked t
                      ret new-ret)))
            (check-if-done)
            (unless (turn-queue-of *battle*)
              (incf (time-of *game*))
              (setf (turn-queue-of *battle*)
                    (s:dsu-sort (iter (for i in (append (enemies-of *battle*) (team-npcs-of *battle*) (team-of *game*)))
                                      (when (> (health-of i) 0)
                                        (collect i)))
                                '>
                                :key (lambda (a) (calculate-stat a :speed))))))
      (format t "~a is next in battle~%" (name-of (first (turn-queue-of *battle*))))
      ret)))
(defunassert ally-join (ally)
  (ally ally)
  (format t "~a Joins the team~%" (name-of ally))
  (when (> (bitcoins-of ally) 0)
    (format t "~a gets ~f bitcoins from ~a~%" (name-of (player-of *game*)) (bitcoins-of ally) (name-of ally)))
  (when (inventory-of ally)
    (format t "~a gets some loot from ~a~%" (name-of (player-of *game*)) (name-of ally))
    (pushnew ally (allies-of *game*)))
  (incf (bitcoins-of (player-of *game*)) (bitcoins-of ally))
  (appendf* (inventory-of (player-of *game*)) (inventory-of ally))
  (setf (inventory-of ally) ()
        (bitcoins-of ally) 0)
  t)
(defun use-item% (item user &rest keys &key target action &allow-other-keys
                  &aux (effective-action (getf (special-actions-of item) action)) (script (when effective-action
                                                                                            (action-lambda effective-action))))
  (unless (apply 'cant-use-p item user target action keys)
    (cond ((and action effective-action)
           (error 'item-action-missing :action action :item item))
          ((and (not action)
                (not (compute-applicable-methods #'use-script (list item user target))))
           (error 'item-use-script-missing-error :format-control "~s has no ~s method defined" :format-arguments `(,item use-script))))
    (let ((ret (if script
                   (apply (coerce script 'function) item target keys)
                   (use-script item user target))))
      (when (consumablep item)
        (a:deletef (the list (inventory-of user)) item))
      (when (> (health-of target) (calculate-stat target :health))
        (setf (health-of target) (calculate-stat target :health)))
      (when (> (energy-of target) (calculate-stat target :energy))
        (setf (energy-of target) (calculate-stat target :energy)))
      ret)))
(defunassert set-player (name malep species)
  (malep boolean
         name simple-string
         species simple-string)
  "Sets the name, gender, and species of the player"
  (setf (name-of (player-of *game*)) name)
  (setf (species-of (player-of *game*)) species)
  (setf (malep (player-of *game*)) malep)
  t)
(defun intro-function (&aux (default (make-instance 'player))
                         (wear '(yadfa-items:short-dress yadfa-items:tshirt yadfa-items:bra yadfa-items:jeans
                                 yadfa-items:boxers yadfa-items:panties yadfa-items:pullups yadfa-items:diaper))
                         name male species clothes bladder bowels fill-rate wings skin tail tail-type bio)
  "This function sets up the player and prints the back story. If you're trying to create your own game with a different storyline using a mod, you can replace this function. Be careful when enabling mods that change the story line this significantly as they can overwrite each other"
  (write-line "Enter your character's name, gender, and species" *query-io*)
  (clim:accepting-values (*query-io* :resynchronize-every-pass t :exit-boxes '((:exit "Accept")))
                         (fresh-line *query-io*)
                         (setf name (clim:accept 'string :prompt "Name" :default (name-of default) :view clim:+text-field-view+ :stream *query-io*))
                         (fresh-line *query-io*)
                         (setf male (clim:accept 'boolean :prompt "Is Male"
                                                          :default (malep default) :view clim:+toggle-button-view+ :stream *query-io*))
                         (fresh-line *query-io*)
                         (setf species (clim:accept 'string :prompt "Species"
                                                            :default (species-of default) :view clim:+text-field-view+ :stream *query-io*))
                         (fresh-line *query-io*)
                         (setf clothes (clim:accept `((clim:subset-completion ,wear) :name-key ,(lambda (o) (name-of (make-instance o))))
                                                    :prompt "Clothes" :view clim:+check-box-view+ :default '(yadfa-items:tshirt yadfa-items:diaper)
                                                    :stream *query-io*))
                         (fresh-line *query-io*)
                         (setf bladder (clim:accept '(clim:completion (:normal :low :overactive))
                                                    :prompt "Bladder capacity" :default :normal :view clim:+option-pane-view+ :stream *query-io*))
                         (fresh-line *query-io*)
                         (setf bowels (clim:accept '(clim:completion (:normal :low :kid))
                                                   :prompt "Bowels capacity" :default :normal :view clim:+option-pane-view+ :stream *query-io*))
                         (fresh-line *query-io*)
                         (setf fill-rate (clim:accept '(clim:completion (:normal :fast :faster))
                                                      :prompt "Bladder/Bowels fill rate" :default :normal :view clim:+option-pane-view+ :stream *query-io*))
                         (fresh-line *query-io*)
                         (setf bio (clim:accept 'string :prompt "Description" :default (description-of default) :view '(clim:text-editor-view :ncolumns 80 :nlines 7)
                                                        :stream *query-io*)))
  (clim:accepting-values (*query-io* :resynchronize-every-pass t :exit-boxes '((:exit "Accept")))
                         (setf tail-type (clim:accept '(clim:completion (:small :medium :large :lizard :bird-small :bird-large nil))
                                                      :prompt "Tail type" :default (car (tail-of default)) :view clim:+option-pane-view+ :stream *query-io*))
                         (fresh-line *query-io*)
                         (setf tail (clim:accept '((clim:subset-completion (:multi :scales :fur :feathers)))
                                                 :prompt "Tail attributes" :default (cdr (tail-of default)) :view clim:+check-box-view+ :stream *query-io*))
                         (fresh-line *query-io*)
                         (setf wings (clim:accept '((clim:subset-completion (:scales :fur :feathers)))
                                                  :prompt "Wings attributes" :default (wings-of default) :view clim:+check-box-view+ :stream *query-io*))
                         (fresh-line *query-io*)
                         (setf skin (clim:accept '((clim:subset-completion (:scales :fur :feathers)))
                                                 :prompt "Skin attributes" :default (skin-of default) :view clim:+check-box-view+ :stream *query-io*)))
  (setf (player-of *game*) (make-instance 'player
                                          :position '(0 0 0 yadfa-zones:home)
                                          :name name
                                          :male male
                                          :species species
                                          :description bio
                                          :skin skin
                                          :wings wings
                                          :tail (when tail-type (cons tail-type tail))
                                          :bladder/need-to-potty-limit (getf '(:normal 300 :low 200 :overactive 149) bladder)
                                          :bladder/potty-dance-limit (getf '(:normal 450 :low 300 :overactive 150) bladder)
                                          :bladder/potty-desperate-limit (getf '(:normal 525 :low 350 :overactive 160) bladder)
                                          :bladder/maximum-limit (getf '(:normal 600 :low 400 :overactive 200) bladder)
                                          :bladder/contents (getf '(:normal 450 :low 300 :overactive 150) bladder)
                                          :bowels/need-to-potty-limit (getf '(:normal 400 :low 800/3 :kid 140) bowels)
                                          :bowels/potty-dance-limit (getf '(:normal 600 :low 400 :kid 210) bowels)
                                          :bowels/potty-desperate-limit (getf '(:normal 700 :low 1400/3 :kid 245) bowels)
                                          :bowels/maximum-limit (getf '(:normal 800 :low 1600/3 :kid 280) bowels)
                                          :bladder/fill-rate (getf '(:normal 25/9
                                                                     :fast 50/9
                                                                     :faster 100/9)
                                                                   fill-rate)
                                          :bowels/fill-rate (getf '(:normal 5/9
                                                                    :fast 10/9
                                                                    :faster 20/9)
                                                                  fill-rate)
                                          :wear (iter (for i in wear)
                                                      (when (s:memq i clothes)
                                                        (collect (make-instance i))))))
  (setf (team-of *game*) (list (player-of *game*)))
  (iter (for i in (iter (for i in '(yadfa-items:diaper yadfa-items:pullups yadfa-items:boxers yadfa-items:panties))
                        (when (s:memq i clothes)
                          (collect i))))
        (dotimes (j (random 20))
          (push (make-instance i)
                (get-items-from-prop :dresser (position-of (player-of *game*))))))
  (write-line "You wake up from sleeping, the good news is that you managed to stay dry throughout the night. Bad news is your bladder filled up during the night. You would get up and head to the toilet, but the bed is too comfy, so you just lay there holding it until the discomfort of your bladder exceeds the comfort of your bed. Then eventually get up while holding yourself and hopping from foot to foot hoping you can make it to a bathroom in time" *query-io*))
