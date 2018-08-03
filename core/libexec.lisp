;;;; files used internally by the game, don't call these unless you're developing (or cheating)
(in-package :yadfa)
(defun lambda-list (lambda-exp)
    (declare (type (or list function) lambda-exp))
    (check-type lambda-exp (or list function))
    (cond
        ((typep lambda-exp 'function)
            (swank-backend:arglist lambda-exp)) ; thanks to the CLIM Listener always being included, so is Swank
        ((and (typep lambda-exp 'list) (equal (car lambda-exp) 'cl:lambda))
            (cadr lambda-exp))
        (t (error "lambda-exp is not a valid Lambda expression"))))
(defun initialize-mod-registry ()
    (setf *mod-registry* (make-hash-table :test 'equal))
    (labels ((preferred-mod (old new)
                 (cond ((not old)
                           new)
                     ((< (list-length (pathname-directory new))
                          (list-length (pathname-directory old)))
                         new)
                     ((<
                          (list-length (pathname-directory old))
                          (list-length (pathname-directory new)))
                         old)
                     ((string< (namestring old) (namestring new))
                         old)
                     (t new))))
        (iter (for i in (uiop:directory-files
                            (format nil "*.asd")
                            (merge-pathnames "mods/**/" (pathname (directory-namestring (truename (uiop:argv0)))))))
            (when (string= (pathname-type i) "asd")
                (setf (gethash (pathname-name i) *mod-registry*)
                    (preferred-mod (gethash (pathname-name i) *mod-registry*) i))))))
(defun clear-mod-registry ()
    (setf *mod-registry* nil))
#+yadfa/mods
(defvar *mods* '())
(defun find-mod (system)
    (unless *mod-registry* (initialize-mod-registry))
    (gethash (asdf:primary-system-name system) *mod-registry*))
(defun load-mods (&rest keys &key compiler-verbose &allow-other-keys)
    #-yadfa/mods nil
    #+yadfa/mods (unless
                     #+yadfa/docs (position "texi" (uiop:command-line-arguments) :test #'string=)
                     #-yadfa/docs nil
                     (unless *game*
                         (when uiop:*image-dumped-p*
                             (pushnew
                                 'yadfa::find-mod
                                 asdf:*system-definition-search-functions*)
                             (uiop:register-clear-configuration-hook 'clear-mod-registry))
                         (asdf:clear-configuration)
                         (let ((file (merge-pathnames "mods.conf"
                                         (if uiop:*image-dumped-p*
                                             (pathname (directory-namestring (truename (uiop:argv0))))
                                             (asdf:system-source-directory :yadfa))))
                                  (mods '()))
                             (handler-case (with-open-file (stream file :if-does-not-exist :error)
                                               (setf mods (read stream)))
                                 (file-error ()
                                     (format t "The configuration file containing the list of enabled mods seems missing, creating a new one~%")
                                     (with-open-file (stream file
                                                         :if-does-not-exist :create
                                                         :direction :output
                                                         :if-exists :supersede)
                                         (write *mods* :stream stream)))
                                 (error ()
                                     (format t "The configuration file containing the list of enabled mods seems broken, ignoring~%")))
                             (if (and
                                     (typep mods 'list)
                                     (iter (for i in mods)
                                         (unless (typep i '(or string symbol asdf/component:component))
                                             (leave nil))
                                         (finally (return t))))
                                 (setf *mods* mods)
                                 (format t "The configuration file containing the list of enabled mods isn't valid, ignoring~%"))))
                     (let ((*compile-verbose* compiler-verbose) (*compile-print* compiler-verbose))
                         (iter (for i in *mods*)
                             (when (asdf:find-system i nil)
                                 (apply #'asdf:load-system i :allow-other-keys t keys))))))
(defun init-game ()
    (load-mods)
    (unless *game*
        (setf *game* (make-instance 'game)))
    (setf *events-in-game* nil)
    (setf *zones-to-initialize* '()))
(defun set-status-condition (status-condition user &key duration)
    (let* ((i
               (if (position
                       status-condition
                       (getf (status-conditions-of *battle*) user)
                       :test (lambda (a b)
                                 (eq a (class-name (class-of b)))))
                   (nth (position
                            status-condition
                            (getf (status-conditions-of *battle*) user)
                            :test (lambda (a b)
                                      (eq a (class-name (class-of b)))))
                       (getf (status-conditions-of *battle*) user))
                   (make-instance status-condition)))
              (duration (if duration duration (duration-of (make-instance status-condition)))))
        (pushnew i (getf (status-conditions-of *battle*) user))
        (when (and (not (eql (duration-of i) t)) (< (duration-of i) duration))
            (setf (duration-of i) duration))))
(defun pathname<= (pathname1 pathname2)
    (string<= (namestring pathname1) (namestring pathname2)))
                                        ;(slynk:eval-in-emacs '(with-current-buffer (sly-mrepl--find-buffer) (insert-image (create-image "/tmp/1523307158.liljdude_renamon_boom.jpg"))))
(defun color-format (color &rest body)
    (cond
        #+(or slynk swank)
        ((not clim-listener::*application-frame*)
            (cl-ansi-text:with-color (color) (apply #'format t body)))
        (clim-listener::*application-frame*
            (clim:with-drawing-options (*standard-output* :ink (eval (intern (format nil "+~a+" color) "CLIM")))
                (apply #'format t body)))))
(defun emacs-prompt (options &optional error-message)
    (eval `(progn
               (,(cond
                     #+slynk ((position "slynk" (uiop:command-line-arguments) :test #'string=) 'slynk:eval-in-emacs)
                     #+swank ((position "swank" (uiop:command-line-arguments) :test #'string=) 'swank:eval-in-emacs))
                   '(progn (require 'widget)
                        (eval-when-compile
                            (require 'wid-edit))
                        (defvar yadfa-contents nil)
                        (defun yadfa-widget ()
                            "Widget generated from the CL side as a dialog for my game"
                            (interactive)
                            ,(if error-message '(switch-to-buffer "*YADFA*") '(switch-to-buffer-other-window "*YADFA*"))
                            (kill-all-local-variables)
                            (let ((inhibit-read-only t))
                                (erase-buffer))
                            (remove-overlays)
                            (widget-insert
                                ,(if error-message
                                     (format nil "~a~%" error-message)
                                     (format nil "Please answer in this \"Window\"~%")))
                            ,@(iter (with j = 0)
                                  (for i in options)
                                  (cond ((equal (first i) 'boolean)
                                            (collect `(widget-insert ,(format nil "~a: " (getf (rest i) :prompt))))
                                            (collect `(set (make-local-variable ',(intern (format nil "a~d" j)))
                                                          (widget-create 'checkbox
                                                              ,(getf (rest i) :default)))))
                                      ((equal (first i) 'string)
                                          (collect `(set (make-local-variable ',(intern (format nil "a~d" j)))
                                                        (widget-create 'editable-field
                                                            :format ,(format nil "~a: %v" (getf (rest i) :prompt))
                                                            ,(getf (rest i) :default)))))
                                      ((equal (first i) 'integer)
                                          (collect `(set (make-local-variable ',(intern (format nil "a~d" j)))
                                                        (widget-create 'integer
                                                            :format ,(format nil "~a: %v" (getf (rest i) :prompt))
                                                            ,(getf (rest i) :default)))))
                                      ((equal (first i) 'number)
                                          (collect `(set (make-local-variable ',(intern (format nil "a~d" j)))
                                                        (widget-create 'number
                                                            :format ,(format nil "~a: %v" (getf (rest i) :prompt))
                                                            ,(getf (rest i) :default))))))
                                  (collect `(widget-insert ,#\linefeed))
                                  (incf j))
                            (widget-create 'push-button
                                :notify (lambda (top widget &optional reason)
                                            (sly-send
                                                (list
                                                    :emacs-return
                                                    ,(cond
                                                         #+yadfa/slynk ((position "slynk"
                                                                            (uiop:command-line-arguments)
                                                                            :test #'string=)
                                                                           (slynk::current-thread-id))
                                                         #+yadfa/swank ((position "swank"
                                                                            (uiop:command-line-arguments)
                                                                            :test #'string=)
                                                                           (swank::current-thread-id)))
                                                    :yadfa-response
                                                    (list ,@(iter (with j = 0) (for i in options)
                                                                (declare (ignorable i))
                                                                (collect `(widget-value ,(intern (format nil "a~d" j))))
                                                                (incf j)))))
                                            (kill-buffer))
                                "Apply")
                            (use-local-map widget-keymap)
                            (widget-setup)
                            nil)
                        (yadfa-widget)))
               (third (,(cond
                            #+slynk ((position "slynk" (uiop:command-line-arguments) :test #'string=)
                                              'slynk::wait-for-event)
                            #+swank ((position "swank" (uiop:command-line-arguments) :test #'string=)
                                              'swank::wait-for-event))
                          '(:emacs-return :yadfa-response result))))))
(defun prompt-for-values (&rest options)
    (cond
        #+(or slynk swank)
        ((not clim-listener::*application-frame*)
            (eval `(let ((out (emacs-prompt ',options))
                            (err nil))
                       (iter (while
                                 (iter (for i in out) (for j in ',options)
                                     (unless (typep i (first j))
                                         (setf err (format nil "~a isn't of type ~a" i (first j)))
                                         (leave t))))
                           (setf out (emacs-prompt ',options err)))
                       out)))
        (clim-listener::*application-frame*
            (eval `(let ((a (make-list ,(list-length options))))
                       (clim:accepting-values (*query-io* :resynchronize-every-pass t)
                           ,@(iter (for i from 0 to (1- (list-length options)))
                                 (collect '(fresh-line *query-io*))
                                 (collect `(setf
                                               (nth ,i a)
                                               (clim:accept ',(first (nth i options))
                                                   :prompt ,(getf (rest (nth i options)) :prompt)
                                                   :default ,(getf (rest (nth i options)) :default) :stream *query-io*)))))
                       a)))))
(defun set-new-battle (enemies &key win-events enter-battle-text continuable)
    (when continuable
        (setf
            (continue-battle-of (get-zone (position-of (player-of *game*))))
            (list
                :enemies enemies
                :win-events win-events)))
    (setf *battle*
        (make-instance 'battle
            :enemies (iter (for j in enemies)
                         (collect (apply #'make-instance (car j) (eval (cdr j)))))
            :win-events win-events
            :enter-battle-text enter-battle-text))
    (format t "~a~%" (enter-battle-text-of *battle*))
    (iter (for j in
              (iter (for i in (enemies-of *battle*))
                  (unless (position (class-name (class-of i)) (seen-enemies-of *game*))
                      (format t "~a was added to your pokedex~%" (name-of i))
                      (push (class-name (class-of i)) (seen-enemies-of *game*))
                      (collect (class-name (class-of i))))))
        (yadfa/bin:pokedex j))
    (unuse-package :yadfa/world :yadfa-user)
    (use-package :yadfa/battle :yadfa-user))
(defun run-equip-effects (user)
    (iter (for i in (wear-of user))
        (when (wear-script-of i)
            (funcall
                (coerce (wear-script-of i) 'function)
                i
                user)))
    (when (and (wield-of user) (wield-script-of (wield-of user)))
        (funcall
            (coerce (wield-script-of (wield-of user)) 'function)
            (wield-of user)
            user)))
(defun print-map (position)
    (iter (for y
              from (- (second position) 15)
              to (+ (second position) 15))
        (iter (for x
                  from (- (first position) 15)
                  to (+ (first position) 15))
            (format t "~a"
                (cond
                    ((and
                         (= x (first (position-of (player-of *game*))))
                         (= y (second (position-of (player-of *game*))))
                         (= (third position) (third (position-of (player-of *game*))))
                         (equal (fourth position) (fourth (position-of (player-of *game*)))))
                        "@")
                    ((and
                         (get-zone (list x y (third position) (fourth position)))
                         (hiddenp (get-zone (list x y (third position) (fourth position))))) " ")
                    ((and
                         (get-zone (list x y (third position) (fourth position)))
                         (warp-points-of (get-zone (list x y (third position) (fourth position)))))
                        "W")
                    ((get-zone (list x y (third position) (fourth position)))
                        ".")
                    (t " "))))
        (format t "~%")))
(defun print-enter-text (position)
    (format t "~a~%" (enter-text-of (get-zone position)))
    (flet ((z (delta direction)
               (when (and (get-zone (append (mapcar #'+ (butlast position) delta)
                                        (last position)))
                         (not (hiddenp (get-zone (append (mapcar #'+ (butlast position) delta)
                                                     (last position))))))
                   (format t "To ~s is ~a. "
                       direction
                       (name-of
                           (get-zone (append (mapcar #'+ (butlast position) delta)
                                         (last position))))))))
        (z '(-1 0 0) :west)
        (z '(1 0 0) :east)
        (z '(0 1 0) :south)
        (z '(0 -1 0) :north)
        (z '(0 0 1) :up)
        (z '(0 0 -1) :down))
    (iter (for (a b) on (warp-points-of (get-zone position)) by #'cddr)
        (when (and (get-zone b) (not (hiddenp (get-zone b))))
            (format t "To ~s is ~a. " a (name-of (get-zone b)))))
    (format t "~%"))
(defun get-inventory-list ()
    (loop for i in (inventory-of (player-of *game*)) collect (symbol-name (type-of i))))
(defmacro ensure-event (event-id &rest args)
    `(progn (unless (gethash
                        ',event-id
                        (events-of-game))
                (setf (gethash
                          ',event-id
                          (events-of-game))
                    (make-event :id ',event-id ,@args))
                (export ',event-id ',(symbol-package event-id)))))
(defun events-of-game ()
    (cond
        (*events-in-game* *events-in-game*)
        (t (events-of *game*))))
(defun get-zone (position)
    (declare (type list position))
    (gethash position
        (zones-of *game*)))
(defun (setf get-zone) (new-value position)
    (setf (gethash position
              (zones-of *game*))
        new-value))
(defun wearingp (clothing type)
    "This function will return all clothes in the list CLOTHING that is of type TYPE"
    (declare (type list clothing))
    (iter (for i in clothing)
        (when (typep i type)(collect i))))
(defun swell-up% (user)
    (iter (for i in (wearingp (wear-of user) 'closed-bottoms))
        (if (waterproofp i)
            (finish)
            (progn
                (setf (sogginess-of i) (sogginess-capacity-of i))
                (collect i)))))
(defun swell-up (user)
    (let ((swollen-clothes (swell-up% user)))
        (cond
            ((wearingp swollen-clothes 'tabbed-briefs)
                (format t "~a's diapers swells up humorously~%~%" (name-of user)))
            ((wearingp swollen-clothes 'pullon)
                (format t "~a's pullups swells up humorously~%~%" (name-of user)))
            ((wearingp swollen-clothes 'incontinence-pad)
                (format t "~a's incontinence pad swells up~%~%" (name-of user))))))
(defun swell-up-all ()
    (swell-up (player-of *game*))
    (loop for i in (allies-of *game*) do (swell-up i)))
(defun get-warp-point (direction position)
    (declare (type symbol direction))
    (getf (warp-points-of (get-zone position))
        (if (typep direction 'keyword)
            (first (member direction (warp-points-of (get-zone position))
                       :test (lambda (a b)
                                 (when (typep b 'symbol)
                                     (string= a b)))))
            direction)))
(defun thickest (clothing)
    "returns the greatest thickess value of a list of clothes passed as the CLOTHING parameter"
    (declare (type list clothing))
    (cond ((not clothing) -1)
        (t (first (sort (iter (for i in clothing) (collect (thickness-of i))) '>)))))
(defun total-thickness (clothing)
    (declare (type list clothing))
    (iter (for i in (wearingp clothing 'closed-bottoms)) (with j = 0) (incf j (thickness-of i)) (finally (return j))))
(defun thickest-sort (clothing)
    (declare (type list clothing))
    (sort (copy-tree clothing) (lambda (a b) (> (thickness-of a) (thickness-of b)))))
(defgeneric toggle-onesie%% (onesie))
(defun toggle-onesie% (onesie underclothes user)
    (cond ((not (typep onesie 'onesie))
              (format t "That's not a onesie~%"))
        ((and (typep onesie 'onesie/opened)
             (not (eq t (car (onesie-thickness-capacity-of onesie))))
             underclothes
             (> (total-thickness underclothes) (car (onesie-thickness-capacity-of onesie))))
            (format t
                "~a struggles to snap the bottom of ~a ~a like a toddler who can't dress ~aself but ~a ~a is too thick~%~%"
                (name-of user)
                (if (malep user) "his" "her")
                (name-of onesie)
                (if (malep user) "him" "her")
                (if (malep user) "his" "her")
                (name-of (first (thickest-sort underclothes)))))
        ((and (typep onesie 'onesie/closed) (lockedp onesie))
            (format t "~a can't unsnap ~a ~a as it's locked~%~%"
                (name-of user)
                (if (malep user) "his" "her")
                (name-of onesie)))
        (t (toggle-onesie%% onesie))))
(defmacro defonesie (base-class &body body)
    "macro that generates the classes and methods of the onesie used to open and close the snaps of them. method used to toggle the onesie is TOGGLE-ONESIE. BASE-CLASS is the name of the class you want to give the onesie. BODY is the slot specifier and class options of BASE-CLASS"
    `(progn
         (defclass ,(intern (format nil "~a" (symbol-name base-class)) (symbol-package base-class))
             (yadfa:onesie) ,@body)
         (defclass ,(intern (format nil "~a/OPENED" (symbol-name base-class)) (symbol-package base-class))
             (,(intern (format nil "~a" (symbol-name base-class)) (symbol-package base-class))
                 yadfa:onesie/opened) ())
         (defclass ,(intern (format nil "~a/CLOSED" (symbol-name base-class)) (symbol-package base-class))
             (,(intern (format nil "~a" (symbol-name base-class)) (symbol-package base-class))
                 yadfa:onesie/closed) ())
         (export ',(intern (format nil "~a" (symbol-name base-class)) (symbol-package base-class)))
         (export ',(intern (format nil "~a/OPENED" (symbol-name base-class)) (symbol-package base-class)))
         (export ',(intern (format nil "~a/CLOSED" (symbol-name base-class)) (symbol-package base-class)))
         (defmethod toggle-onesie%% ((self ,(intern (format nil "~a/OPENED" (symbol-name base-class)) (symbol-package base-class))))
             (change-class self ',(intern (format nil "~a/CLOSED" (symbol-name base-class)) (symbol-package base-class))))
         (defmethod toggle-onesie%% ((self ,(intern (format nil "~a/CLOSED" (symbol-name base-class)) (symbol-package base-class))))
             (change-class self ',(intern (format nil "~a/OPENED" (symbol-name base-class)) (symbol-package base-class))))))
(defmacro setf-init-hook/zone (position key &body body)
    (declare (type symbol key))
    `(progn
         (setf (getf (gethash
                         ',(intern
                               (string-upcase
                                   (format nil
                                       "init-hooks/zone-~{~a~}~a"
                                       (iter (for i in (butlast position)) (collect (format nil "~a-" i)))
                                       (fourth position)))
                               (symbol-package (fourth position)))
                         *inithooks/zone*)
                   ',key)
             '(lambda (zone) ,@body))))
(defun ensure-zones (c)
    (iter (for i in *zones-to-initialize*)
        (unless
            (gethash i
                (zones-of c))
            (setf (gethash i
                      (zones-of c))
                (eval
                    `(make-instance
                         ',(intern
                               (string-upcase
                                   (format nil
                                       "zone-~{~a~}~a"
                                       (iter (for j in (butlast i)) (collect (format nil "~a-" j)))
                                       (fourth i)))
                               (symbol-package (fourth i)))))))
        (export (fourth i) (symbol-package (fourth i)))))
(defmacro defzone (position &body body)
    "defines the classes of the zones and adds an instance of them to the game's map hash table if it's not already there"
    (declare (type list position))
    `(progn
         (defclass
             ,(intern
                  (string-upcase
                      (format nil
                          "zone-~{~a~}~a"
                          (iter (for i in (butlast position)) (collect (format nil "~a-" i)))
                          (fourth position)))
                  (symbol-package (fourth position)))
             (zone)
             ,@body)
         (setf
             (gethash ',(intern
                            (string-upcase
                                (format nil
                                    "init-hooks/zone-~{~a~}~a"
                                    (iter (for i in (butlast position)) (collect (format nil "~a-" i)))
                                    (fourth position)))
                            (symbol-package (fourth position)))
                 *inithooks/zone*)
             ())
         (export ',(intern
                       (string-upcase
                           (format nil
                               "init-hooks/zone-~{~a~}~a"
                               (iter (for i in (butlast position)) (collect (format nil "~a-" i)))
                               (fourth position)))
                       (symbol-package (fourth position)))
             ',(symbol-package (fourth position)))
         (pushnew
             ',position
             *zones-to-initialize*)
         (export ',(fourth position) ',(symbol-package (fourth position)))))
(defmacro make-pocket-zone (position &body body)
    "defines the classes of the zones and adds an instance of them to the game's map hash table if it's not already there"
    (declare (type list position))
    `(setf (gethash '(,@position :pocket-map)
               (zones-of *game*))
         (make-instance 'zone ,@body)))
(defun move-to-secret-underground ()
    (when *battle*
        (format t "To avoid breaking the game due to a few assumtpions made in this function, please don't run this in a battle~%")
        (return-from move-to-secret-underground))
    (when (eql (fourth (position-of (player-of *game*))) :pocket-map)
        (format t "To avoid breaking the game due to a few assumtpions in the game's code, please don't run this in the pocket map~%")
        (return-from move-to-secret-underground))
    (let ((new-position
              '(0 0 0 yadfa/zones:secret-underground))
             (wearing-pants nil))
        (when (and
                  (diapers-only-p (get-zone new-position))
                  (setf wearing-pants
                      (iter (for i in (append (list (player-of *game*)) (allies-of *game*)))
                          (when (or
                                    (<
                                        (list-length (wearingp (wear-of i) 'incontinence-product))
                                        (list-length (wearingp (wear-of i) 'bottoms)))
                                    (< (list-length (wearingp (wear-of i) 'padding)) 1))
                              (collect (name-of i))))))
            (format t "That area is a diapers only pants free zone. Pants are strictly prohibited and padding is manditory.~%The following characters are currently not compliant with this rule:~{~a~}~%"
                (iter (for i in wearing-pants) (collect " ") (collect i)))
            (return-from move-to-secret-underground))
        (when (lockedp (get-zone new-position))
            (if (position-if (lambda (a)
                                 (typep a (lockedp (get-zone new-position))))
                    (append
                        (inventory-of (player-of *game*))
                        (list (wield-of (player-of *game*)))
                        (wear-of (player-of *game*))
                        (let ((a ()))
                            (iter (for i in (allies-of *game*))
                                (push (wield-of i) a)
                                (iter (for j in (wear-of i))
                                    (push j a)))
                            a)))
                (progn
                    (setf (lockedp (get-zone new-position)) nil)
                    (format t "You unlock zone ~a~%" new-position))
                (progn
                    (format t "zone ~a is locked~%" new-position)
                    (return-from move-to-secret-underground))))
        (setf (position-of (player-of *game*))
            new-position)
        (when (underwaterp (get-zone (position-of (player-of *game*)))) (swell-up-all))
        (process-potty)
        (run-equip-effects (player-of *game*))
        (loop for i in (allies-of *game*) do
            (process-potty i)
            (run-equip-effects i))
        (print-enter-text (position-of (player-of *game*)))
        (cond ((continue-battle-of (get-zone (position-of (player-of *game*))))
                  (setf *battle* (make-instance 'battle))
                  (iter
                      (for i in (getf (continue-battle-of (get-zone (position-of (player-of *game*)))) :enemies))
                      (push (apply #'make-instance (car i) (eval (cdr i))) (enemies-of *battle*))
                      (setf
                          (win-events-of *battle*)
                          (getf (continue-battle-of (get-zone (position-of (player-of *game*)))) :win-events)))
                  (format t "~a~%" (enter-battle-text-of *battle*))
                  (iter (for j in
                            (iter (for i in (enemies-of *battle*))
                                (unless (position (class-name (class-of i)) (seen-enemies-of *game*))
                                    (format t "~a was added to your pokedex~%" (name-of i))
                                    (push (class-name (class-of i)) (seen-enemies-of *game*))
                                    (collect (class-name (class-of i))))))
                      (yadfa/bin:pokedex j))
                  (unuse-package :yadfa/world :yadfa-user)
                  (use-package :yadfa/battle :yadfa-user)
                  (return-from move-to-secret-underground))
            ((iter (for i in (events-of (get-zone (position-of (player-of *game*)))))
                 (when (or (not (position i (finished-events-of *game*))) (event-repeatable i))
                     (funcall (coerce (event-lambda i) 'function) i)
                     (pushnew i (finished-events-of *game*))
                     (collect i)))
                (return-from move-to-secret-underground))
            ((enemy-spawn-list-of (get-zone (position-of (player-of *game*))))
                (iter
                    (for i in (enemy-spawn-list-of (get-zone (position-of (player-of *game*)))))
                    (let ((random (if (getf i :random) (getf i :random) 1)))
                        (when (< (strong-random (getf i :max-random)) random)
                            (setf *battle* (make-instance 'battle
                                               :enemies (iter (for j in (getf i :enemies))
                                                            (collect (apply #'make-instance (car j) (eval (cdr j)))))))
                            (format t "~a~%" (enter-battle-text-of *battle*))
                            (iter (for j in
                                      (iter (for i in (enemies-of *battle*))
                                          (unless (position (class-name (class-of i)) (seen-enemies-of *game*))
                                              (format t "~a was added to your pokedex~%" (name-of i))
                                              (push (class-name (class-of i)) (seen-enemies-of *game*))
                                              (collect (class-name (class-of i))))))
                                (yadfa/bin:pokedex j))
                            (unuse-package :yadfa/world :yadfa-user)
                            (use-package :yadfa/battle :yadfa-user)
                            (return-from move-to-secret-underground))))))))
(defun move-to-pocket-map ()
    (when *battle*
        (format t "To avoid breaking the game due to a few assumtpions made in this function, please don't run this in a battle~%")
        (return-from move-to-pocket-map))
    (unless (get-zone '(0 0 0 pocket-map))
        (make-pocket-zone (0 0 0)
            :name "Pocket Map Entrace"
            :description "Welcome to the Pocket Map. It's like the secret bases in Pokemon, except you customize it by scripting, and you can take it with you."
            :enter-text "You're at the start of the Pocket Map. Use the Pocket Map machine again at anytime to exit."))
    (let ((new-position
              (if (pocket-map-point-of (player-of *game*))
                  (pocket-map-point-of (player-of *game*))
                  '(0 0 0 :pocket-map)))
             (wearing-pants nil))
        (when (and
                  (diapers-only-p (get-zone new-position))
                  (setf wearing-pants
                      (iter (for i in (append (list (player-of *game*)) (allies-of *game*)))
                          (when (or
                                    (<
                                        (list-length (wearingp (wear-of i) 'incontinence-product))
                                        (list-length (wearingp (wear-of i) 'bottoms)))
                                    (< (list-length (wearingp (wear-of i) 'padding)) 1))
                              (collect (name-of i))))))
            (format t "That area is a diapers only pants free zone. Pants are strictly prohibited and padding is manditory.~%The following characters are currently not compliant with this rule:~{~a~}~%"
                (iter (for i in wearing-pants) (collect " ") (collect i)))
            (return-from move-to-pocket-map))
        (when (lockedp (get-zone new-position))
            (if (position-if (lambda (a)
                                 (typep a (lockedp (get-zone new-position))))
                    (append
                        (inventory-of (player-of *game*))
                        (list (wield-of (player-of *game*)))
                        (wear-of (player-of *game*))
                        (let ((a ()))
                            (iter (for i in (allies-of *game*))
                                (push (wield-of i) a)
                                (iter (for j in (wear-of i))
                                    (push j a)))
                            a)))
                (progn
                    (setf (lockedp (get-zone new-position)) nil)
                    (format t "You unlock zone ~a~%" new-position))
                (progn
                    (format t "zone ~a is locked~%" new-position)
                    (return-from move-to-pocket-map))))
        (setf (pocket-map-point-of (player-of *game*))
            (if (pocket-map-point-of (player-of *game*))
                nil
                (position-of (player-of *game*))))
        (setf (position-of (player-of *game*))
            new-position)
        (when (underwaterp (get-zone (position-of (player-of *game*)))) (swell-up-all))
        (process-potty)
        (run-equip-effects (player-of *game*))
        (loop for i in (allies-of *game*) do
            (process-potty i)
            (run-equip-effects i))
        (print-enter-text (position-of (player-of *game*)))
        (cond ((continue-battle-of (get-zone (position-of (player-of *game*))))
                  (setf *battle* (make-instance 'battle))
                  (iter
                      (for i in (getf (continue-battle-of (get-zone (position-of (player-of *game*)))) :enemies))
                      (push (apply #'make-instance (car i) (eval (cdr i))) (enemies-of *battle*))
                      (setf
                          (win-events-of *battle*)
                          (getf (continue-battle-of (get-zone (position-of (player-of *game*)))) :win-events)))
                  (format t "~a~%" (enter-battle-text-of *battle*))
                  (iter (for j in
                            (iter (for i in (enemies-of *battle*))
                                (unless (position (class-name (class-of i)) (seen-enemies-of *game*))
                                    (format t "~a was added to your pokedex~%" (name-of i))
                                    (push (class-name (class-of i)) (seen-enemies-of *game*))
                                    (collect (class-name (class-of i))))))
                      (yadfa/bin:pokedex j))
                  (unuse-package :yadfa/world :yadfa-user)
                  (use-package :yadfa/battle :yadfa-user)
                  (return-from move-to-pocket-map))
            ((iter (for i in (events-of (get-zone (position-of (player-of *game*)))))
                 (when (or (not (position i (finished-events-of *game*))) (event-repeatable i))
                     (funcall (coerce (event-lambda i) 'function) i)
                     (pushnew i (finished-events-of *game*))
                     (collect i)))
                (return-from move-to-pocket-map))
            ((enemy-spawn-list-of (get-zone (position-of (player-of *game*))))
                (iter
                    (for i in (enemy-spawn-list-of (get-zone (position-of (player-of *game*)))))
                    (let ((random (if (getf i :random) (getf i :random) 1)))
                        (when (< (strong-random (getf i :max-random)) random)
                            (setf *battle* (make-instance 'battle
                                               :enemies (iter (for j in (getf i :enemies))
                                                            (collect (apply #'make-instance (car j) (eval (cdr j)))))))
                            (format t "~a~%" (enter-battle-text-of *battle*))
                            (iter (for j in
                                      (iter (for i in (enemies-of *battle*))
                                          (unless (position (class-name (class-of i)) (seen-enemies-of *game*))
                                              (format t "~a was added to your pokedex~%" (name-of i))
                                              (push (class-name (class-of i)) (seen-enemies-of *game*))
                                              (collect (class-name (class-of i))))))
                                (yadfa/bin:pokedex j))
                            (unuse-package :yadfa/world :yadfa-user)
                            (use-package :yadfa/battle :yadfa-user)
                            (return-from move-to-pocket-map))))))))
(defmacro do-push (item &rest places)
    `(progn ,@(loop for place in places collect `(push ,item ,place))))
(defun wet (&key (wet-amount t) force-fill-amount pants-down accident force-wet-amount (wetter (player-of *game*)))
    (declare
        (type (or null number) force-fill-amount force-wet-amount)
        (type (or boolean number) wet-amount))
    (let* ((return-value ()) (affected-clothes ()) (random (strong-random 4)) (amount nil))
        (when force-fill-amount
            (setf (bladder/contents-of wetter) force-fill-amount))
        (cond (force-wet-amount
                  (setf amount force-wet-amount))
            ((< (bladder/contents-of wetter) (bladder/need-to-potty-limit-of wetter))
                (return-from wet (list
                                     :old-bladder-contents (bladder/contents-of wetter)
                                     :new-bladder-contents (bladder/contents-of wetter)
                                     :affected-clothes ()
                                     :leak-amount 0
                                     :wet-amount 0)))
            (accident
                (setf amount
                    (cond
                        ((< random 2) (bladder/contents-of wetter))
                        ((< random 3) 300)
                        ((< random 4) 10))))
            ((eq wet-amount t)
                (setf amount (bladder/contents-of wetter)))
            ((> wet-amount (bladder/contents-of wetter))
                (setf amount (bladder/contents-of wetter)))
            (t (setf amount wet-amount)))
        (setf (getf return-value :old-bladder-contents) (bladder/contents-of wetter))
        (let* ( (amount-left amount))
            (cond ((or pants-down (not (wearingp (wear-of wetter) 'closed-bottoms)))
                      (decf (bladder/contents-of wetter) amount)
                      (setf amount-left 0))
                (t
                    (decf (bladder/contents-of wetter) amount)
                    (iter (while (> amount-left 0)) (for i in (reverse (wear-of wetter)))
                        (when (typep i 'closed-bottoms)
                            (cond
                                ((> amount-left (- (sogginess-capacity-of i) (sogginess-of i)))
                                    (decf amount-left (- (sogginess-capacity-of i) (sogginess-of i)))
                                    (setf (sogginess-of i) (sogginess-capacity-of i))
                                    (push i affected-clothes))
                                ((> amount-left 0)
                                    (incf (sogginess-of i) amount-left)
                                    (setf amount-left 0)
                                    (push i affected-clothes)))))))
            (setf (getf return-value :new-bladder-contents) (bladder/contents-of wetter))
            (setf (getf return-value :affected-clothes) affected-clothes)
            (setf (getf return-value :leak-amount) amount-left)
            (setf (getf return-value :wet-amount) amount))
        return-value))

(defun mess (&key (mess-amount t) force-fill-amount pants-down accident force-mess-amount (messer (player-of *game*)))
    (declare
        (type (or null number) force-fill-amount force-mess-amount)
        (type (or boolean number) mess-amount))
    (let* ((return-value ()) (affected-clothes ()) (amount nil))
        (when force-fill-amount
            (setf (bowels/contents-of messer) force-fill-amount))
        (cond (force-mess-amount
                  (setf amount force-mess-amount))
            ((< (bowels/contents-of messer) (bowels/need-to-potty-limit-of messer))
                (return-from mess (list
                                      :old-bowels-contents (bowels/contents-of messer)
                                      :new-bowels-contents (bowels/contents-of messer)
                                      :affected-clothes ()
                                      :leak-amount 0
                                      :mess-amount 0)))
            (accident
                (setf amount (bowels/contents-of messer)))
            ((eq mess-amount t)
                (setf amount (bowels/contents-of messer)))
            ((> mess-amount (bowels/contents-of messer))
                (setf amount (bowels/contents-of messer)))
            (t (setf amount mess-amount)))
        (setf (getf return-value :old-bowels-contents) (bowels/contents-of messer))
        (let* ( (amount-left amount))
            (cond ((or pants-down (not (wearingp (wear-of messer) 'closed-bottoms)))
                      (decf (bowels/contents-of messer) amount)
                      (setf amount-left 0))
                (t
                    (decf (bowels/contents-of messer) amount)
                    (iter (while (> amount-left 0)) (for i in (reverse (wear-of messer)))
                        (when (typep i 'closed-bottoms)
                            (cond
                                ((> amount-left (- (messiness-capacity-of i) (messiness-of i)))
                                    (decf amount-left (- (messiness-capacity-of i) (messiness-of i)))
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
        return-value))
(defun change-the-baby (user &rest new-diaper)
    (let ((b (apply #'make-instance new-diaper)))
        (iter (for i in (wearingp (wear-of user) 'bottoms))
            (setf (lockedp i) nil)
            (when (typep i 'onesie/closed)
                (toggle-onesie%% i)))
        (setf
            (inventory-of (player-of *game*)) (append (inventory-of (player-of *game*))
                                                  (wearingp (wear-of user) 'closed-bottoms))
            (wear-of user) (remove-if
                               (lambda (a)
                                   (typep a 'closed-bottoms))
                               (wear-of user)))
        (if (wear-of user) (push b (cdr (last (wear-of user)))) (push b (wear-of user)))
        (iter (with i = 0) (while (< (1+ i) (list-length (wear-of user))))
            (if
                (or
                    (and
                        (typep (nth i (wear-of user)) 'bottoms)
                        (not (eq (thickness-capacity-of (nth i (wear-of user))) t))
                        (nthcdr (1+ i) (wear-of user))
                        (>
                            (total-thickness (nthcdr (1+ i) (wear-of user)))
                            (thickness-capacity-of (nth i (wear-of user)))))
                    (and
                        (typep (nth i (wear-of user)) 'bottoms)
                        (or
                            (>=
                                (sogginess-of (nth i (wear-of user)))
                                (/ (sogginess-capacity-of (nth i (wear-of user))) 4))
                            (>=
                                (messiness-of (nth i (wear-of user)))
                                (/ (messiness-capacity-of (nth i (wear-of user))) 4)))))
                (progn
                    (push (nth i (wear-of user)) (inventory-of (player-of *game*)))
                    (setf (wear-of user) (remove-nth i (wear-of user))))
                (incf i)))))
(defun trigger-diaper-police (user)
    (format t "*Seems ~a's mess got the attention of the diaper police.*~%~%"
        (name-of user))
    (if (wearingp (wear-of user) 'padding)
        (progn
            (format t "Diaper Police: Seems this ~a needs a diaper change. Better change ~a~%~%"
                (species-of user)
                (if (malep user) "him" "her"))
            (format t "~a: Hey!!! Don't change me here!!! Everyone can see me!!!~%~%"
                (name-of user)))
        (progn
            (format t "Seems this ~a isn't potty trained, better diaper ~a~%~%"
                (species-of user)
                (if (malep user) "him" "her"))
            (format t "~a: Hey!!! I don't need diapers!!! Stop!!!~%~%"
                (name-of user))))
    (change-the-baby user 'yadfa/items:kurikia-thick-diaper :locked t)
    (format t "*The diaper police straps the squirmy and heavily blushy ~a down on a public changing table, strips all of ~a's soggy clothes off (and all the clothes that won't fit over the new diaper), and puts a thick diaper on ~a. All while the local bystandards watch, snicker, giggle, and point*~%~%"
        (name-of user)
        (if (malep user) "he" "she")
        (if (malep user) "him" "her"))
    (format t "Diaper Police: And to be sure the baby keeps ~a diapers on~%~%"
        (if (malep user) "his" "her"))
    (format t "*The diaper police locks the diaper on to prevent ~a from removing it*~%~%"
        (name-of user))
    (format t "*The diaper police unstrap ~a from the table. The diaper is so thick ~a's legs are spread apart forcing ~a to waddle*~%~%"
        (name-of user)
        (name-of user)
        (if (malep user) "him" "her"))
    (format t
        "Diaper Police: Aww, it looks like the baby is learning how to walk for the first time~%~%")
    (format t "*~a whines and covers ~a face with ~a paws in embarrassment*~%~%"
        (name-of user)
        (if (malep user) "his" "her")
        (if (malep user) "his" "her"))
    (unless (position (gethash 'yadfa/events:get-diaper-locked-1 (events-of-game)) (finished-events-of *game*))
        (format t "*~a tugs at the tabs trying to remove them, but they won't budge. Better find a solution before its too late*~%~%"
            (name-of user))
        (push (gethash 'yadfa/events:get-diaper-locked-1 (events-of-game)) (finished-events-of *game*))))
(defun potty-on-toilet (prop &key wet mess pants-down (user (player-of *game*)))
    (declare
        (type toilet prop)
        (type (or boolean number) wet mess))
    (cond
        ((and (not (eq (player-of *game*) user))
             (or (eq (potty-training-of user) :none) (eq (potty-training-of user) :rebel)))
            (format t "Yeah, that's not going to happen~%")
            (return-from potty-on-toilet))
        ((and pants-down
             (iter (for i in (wearingp (wear-of user) 'closed-bottoms))
                 (when (lockedp i)
                     (format t
                         "~a struggles to remove ~a ~a, realizes ~a can't, then starts panicking while doing a potty dance.~%"
                         (name-of user)
                         (if (malep user) "his" "her")
                         (name-of i)
                         (if (malep user) "he" "she"))
                     (leave t))))
            (return-from potty-on-toilet)))
    (let*
        ((mess-return-value (when (or mess (and (not wet) (not mess)))
                                (mess :mess-amount (if mess mess t) :pants-down pants-down :messer user)))
            (wet-return-value (cond
                                  ((and mess-return-value wet)
                                      (wet :pants-down pants-down :wetter user))
                                  ((or wet (and (not wet) (not mess)))
                                      (wet :wet-amount (if wet wet t) :pants-down pants-down :wetter user)))))
        (when
            (and
                (or (not wet-return-value) (and wet-return-value (= (getf wet-return-value :wet-amount) 0)))
                (or (not mess-return-value) (and mess-return-value (= (getf mess-return-value :mess-amount) 0))))
            (format t "~a doesn't have to go~%" (name-of user))
            (return-from potty-on-toilet))
        (if (or
                pants-down
                (not (wearingp (wear-of user) 'closed-bottoms)))
            (format t "~a used the ~a like a big ~a"
                (name-of user)
                (name-of prop)
                (if (malep user) "boy" "girl"))
            (let* ((names ()) (out ()))
                (push (if (and wet-return-value (> (getf wet-return-value :wet-amount) 0)) "soggy butt" "mushy butt") names)
                (push
                    (format nil "~a ~a"
                        (if (and wet-return-value (> (getf wet-return-value :wet-amount) 0)) "soggy" "mushy")
                        (if (malep user) "boy" "girl"))
                    names)
                (when (and wet-return-value (> (getf wet-return-value :wet-amount) 0))
                    (push (format nil "piddle ~a" (if (malep user) "prince" "princess")) names))
                (push
                    (format nil "Looks like you missed a step ~a" (nth (strong-random (list-length names)) names))
                    out)
                (push
                    (format nil "Aww, looks like the little ~a forgot to take ~a ~a first"
                        (let ((a names))
                            (push (format nil "baby ~a" (if (malep user) "boy" "girl")) a)
                            (nth (strong-random (list-length a)) a))
                        (if (malep user) "his" "her")
                        (cond ((wearingp (wear-of user) 'tabbed-briefs)
                                  "diapers")
                            ((wearingp (wear-of user) 'pullon)
                                "pullups")
                            (t "panties")))
                    out)
                (format t "~a~%" (nth (strong-random (list-length out)) out))))))
(defun potty-on-self-or-prop (prop &key wet mess pants-down (user (player-of *game*)))
    (declare (type (or boolean number) wet mess))
    (cond ((and (not (eq (player-of *game*) user))
               (or (eq (potty-training-of user) :none) (eq (potty-training-of user) :rebel))
               pants-down)
              (format t "Yeah, that's not going to happen~%")
              (return-from potty-on-self-or-prop))
        ((and (no-puddles-p (get-zone (position-of (player-of *game*)))) pants-down)
            (format t "~a isn't allowed to go potty there, it's against the rules in this zone~%"
                (name-of user)))
        ((and
             (not pants-down)
             (no-puddles-p (get-zone (position-of (player-of *game*))))
             (not (wearingp (wear-of user) 'incontinence-product))
             (or
                 (not (wearingp (wear-of user) 'closed-bottoms))
                 (eq wet t)
                 (and (typep wet 'number) (> wet 10))
                 mess
                 (and (not wet) (not mess))))
            (format t "~a isn't allowed to do that or ~a'll make ~a, which is against the rules in this zone~%"
                (name-of user)
                (if (malep user) "he" "she")
                (if mess "a mess on the floor" "puddles")))
        ((and pants-down
             (iter (for i in (wearingp (wear-of user) 'closed-bottoms))
                 (when (lockedp i)
                     (format t
                         "~a struggles to remove ~a ~a, realizes ~a can't, then starts panicking while doing a potty dance.~%"
                         (name-of user)
                         (if (malep user) "his" "her")
                         (name-of i)
                         (if (malep user) "he" "she"))
                     (leave t))))
            (return-from potty-on-self-or-prop)))
    (let*
        ((mess-return-value (when (or mess (and (not wet) (not mess)))
                                (mess :mess-amount (if mess mess t) :pants-down pants-down :messer user)))
            (wet-return-value (cond
                                  ((and mess-return-value wet)
                                      (wet :pants-down pants-down :wetter user))
                                  ((or wet (and (not wet) (not mess)))
                                      (wet :wet-amount (if wet wet t) :pants-down pants-down :wetter user))))
            (clothes (cond
                         ((wearingp (wear-of user) 'tabbed-briefs)
                             (list "diapers" "pamps" "huggies" "pampers" "padding"))
                         ((wearingp (wear-of user) 'pullon)
                             (list "pullups" "padding"))
                         ((not (wearingp (wear-of user) 'closed-pants))
                             (list "undies" "panties"))
                         (t (list "pants")))))
        (when (and
                  (or (not wet-return-value) (and wet-return-value (= (getf wet-return-value :wet-amount) 0)))
                  (or (not mess-return-value) (and mess-return-value (= (getf mess-return-value :mess-amount) 0))))
            (format t "~a doesn't have to go~%" (name-of user))
            (return-from potty-on-self-or-prop))
        (let ((wet-list ()) (mess-list ()) (both-list ()) (wet-leak-list ()) (mess-leak-list ()) (both-leak-list ()))
            (flet ((format-lists ()
                       (cond
                           ((and
                                wet-return-value
                                mess-return-value
                                (> (getf wet-return-value :wet-amount) 0)
                                (> (getf mess-return-value :mess-amount) 0)
                                both-list)
                               (format t "~a~%" (nth (strong-random (list-length both-list)) both-list)))
                           ((and mess-return-value (> (getf mess-return-value :mess-amount) 0) mess-list)
                               (format t "~a~%" (nth (strong-random (list-length mess-list)) mess-list)))
                           ((and wet-return-value (> (getf wet-return-value :wet-amount) 0) wet-list)
                               (format t "~a~%" (nth (strong-random (list-length wet-list)) wet-list))))
                       (setf wet-list () mess-list () both-list()))
                      (format-leak-lists ()
                          (cond
                              ((and
                                   wet-return-value
                                   mess-return-value
                                   (> (getf wet-return-value :leak-amount) 0)
                                   (> (getf mess-return-value :leak-amount) 0)
                                   both-leak-list)
                                  (format t "~a~%" (nth (strong-random (list-length both-leak-list)) both-leak-list)))
                              ((and mess-return-value (> (getf mess-return-value :leak-amount) 0) mess-leak-list)
                                  (format t "~a~%" (nth (strong-random (list-length mess-leak-list)) mess-leak-list)))
                              ((and wet-return-value (> (getf wet-return-value :leak-amount) 0) wet-leak-list)
                                  (format t "~a~%" (nth (strong-random (list-length wet-leak-list)) wet-leak-list))))
                          (setf wet-leak-list () mess-leak-list () both-leak-list())))
                (cond
                    ;; player pulls his pants down then potty
                    ((and pants-down (wearingp (wear-of user) 'closed-bottoms))
                        (do-push (format nil
                                     "~a pulled down ~a ~a and went potty on the ~a"
                                     (name-of user)
                                     (if (malep user) "his" "her")
                                     (nth (strong-random (list-length clothes)) clothes)
                                     (if prop
                                         (name-of prop)
                                         "floor"))
                            both-list wet-list mess-list)
                        (do-push (format nil
                                     "~a pulls down ~a ~a and marks ~a territory"
                                     (name-of user)
                                     (if (malep user) "his" "her")
                                     (nth (strong-random (list-length clothes)) clothes)
                                     (if (malep user) "his" "her"))
                            both-list wet-list mess-list)
                        (push (format nil
                                  "~a pulled down ~a ~a and peed on the ~a"
                                  (name-of user)
                                  (if (malep user) "his" "her")
                                  (nth (strong-random (list-length clothes)) clothes)
                                  (if prop
                                      (name-of prop)
                                      "floor"))
                            wet-list)
                        (push (format nil
                                  "~a pulled down ~a ~a and squats down and mess"
                                  (name-of user)
                                  (if (malep user) "his" "her")
                                  (nth (strong-random (list-length clothes)) clothes))
                            mess-list)
                        (do-push (format nil
                                     "Bad ~a! No going potty on the ~a!"
                                     (species-of user)
                                     (if prop
                                         (name-of prop)
                                         "floor"))
                            wet-list mess-list both-list)
                        (format-lists))
                    ;; If the player specifies to pull his pants down without any on, assume he's intentionally going on the floor or prop
                    (pants-down
                        (push (format nil
                                  "~a goes potty on the ~a like an animal"
                                  (name-of user)
                                  (if prop
                                      (name-of prop)
                                      "floor"))
                            both-list)
                        (push (format nil
                                  "~a pees on the ~a like an animal"
                                  (name-of user)
                                  (if prop
                                      (name-of prop)
                                      "floor"))
                            wet-list)
                        (push (format nil
                                  "~a squats down and messes on the ~a like an animal"
                                  (name-of user)
                                  (if prop
                                      (name-of prop)
                                      "floor"))
                            mess-list)
                        (push (format nil "~a lifts ~a leg and pees on the ~a, then squats down on all fours and mess"
                                  (name-of user)
                                  (if (malep user) "his" "her")
                                  (if prop
                                      (name-of prop)
                                      "floor"))
                            both-list)
                        (push (format nil
                                  "~a lifts ~a leg and pees on the ~a"
                                  (name-of user)
                                  (if (malep user) "his" "her")
                                  (if prop
                                      (name-of prop)
                                      "floor"))
                            wet-list)
                        (push (format nil "~a squat down on all fours and messes like an animal" (name-of user)) mess-list)
                        (do-push (format nil
                                     "Bad ~a! No going potty on the ~a!"
                                     (species-of user)
                                     (if prop
                                         (name-of prop)
                                         "floor"))
                            wet-list mess-list both-list)
                        (format-lists))
                    ;; otherwise assume the player is just standing there and lets go, possibly forgetting that he's not wearing padding
                    ((not (wearingp (wear-of user) 'closed-bottoms))
                        (if prop
                            (progn
                                (push (format nil
                                          "~a lifts ~a leg and pees on the ~a"
                                          (name-of user)
                                          (if (malep user) "his" "her")
                                          (name-of prop))
                                    wet-list)
                                (push (format nil
                                          "~a squats down on all fours and mess like an animal"
                                          (name-of user))
                                    mess-list)
                                (push (format nil
                                          "~a lifts ~a leg and pees on the ~a, then squats down on all fours and mess"
                                          (name-of user)
                                          (if (malep user) "his" "her")
                                          (name-of prop))
                                    both-list)
                                (do-push (format nil
                                             "Bad ~a! No going potty on the ~a!"
                                             (species-of user)
                                             (name-of prop))
                                    wet-list mess-list both-list))
                            (do-push (format nil
                                         "~a realized ~a made a horrible mistake. ~a weren't wearing any padding!!!"
                                         (name-of user)
                                         (if (malep user) "he" "she")
                                         (if (malep user) "He" "She"))
                                both-list wet-list mess-list))
                        (format-lists))
                    ;; player is using his pants like a toilet
                    (t
                        (cond
                            ((and (not prop) wet-return-value (< (getf wet-return-value :wet-amount) 30))
                                (push (format nil
                                          "~a lets a little out to relieve the pressure"
                                          (name-of user))
                                    wet-list)
                                (push (format nil
                                          "Bad idea as ~a just made a puddle on the floor"
                                          (if (malep user) "he" "she"))
                                    wet-leak-list)
                                (format-lists)
                                (format-leak-lists))
                            ((wearingp (wear-of user) 'tabbed-briefs)
                                (when prop
                                    (push (format nil
                                              "~a lifts ~a leg near the ~a and floods ~a pamps"
                                              (name-of user)
                                              (if (malep user) "his" "her")
                                              (name-of prop)
                                              (if (malep user) "his" "her"))
                                        wet-list)
                                    (push
                                        (format nil
                                            "~a lifts ~a leg near the ~a and floods ~a pamps. Looks like the little ~a isn't house-trained"
                                            (name-of user)
                                            (if (malep user) "his" "her")
                                            (name-of prop)
                                            (if (malep user) "his" "her")
                                            (species-of user))
                                        wet-list)
                                    (push
                                        (format nil
                                            "You lift your leg near the ~a and flood your pamps, then squat down on all fours and mess"
                                            (name-of prop))
                                        both-list)
                                    (push
                                        (format nil
                                            "~a squats down on all fours with ~a tail raised like an animal and messes ~a diapers"
                                            (name-of user)
                                            (if (malep user) "his" "her")
                                            (if (malep user) "his" "her"))
                                        mess-list))
                                (do-push (format nil
                                             "~a goes potty in ~a diapers like a toddler"
                                             (name-of user)
                                             (if (malep user) "his" "her"))
                                    wet-list mess-list both-list)
                                (when (wearingp (wear-of user) 'diaper)
                                    (do-push (format nil
                                                 "Aww, is the baby using ~a diapers?"
                                                 (if (malep user) "his" "her"))
                                        wet-list mess-list both-list))
                                (push (format nil
                                          "~a pauses and floods ~a diapers"
                                          (name-of user)
                                          (if (malep user) "his" "her"))
                                    wet-list)
                                (push (format nil
                                          "~a squats down and grunts filling ~a diapers"
                                          (name-of user)
                                          (if (malep user) "his" "her"))
                                    mess-list)
                                (push (format nil "heh, the baby blorted ~a diapers" (if (malep user) "his" "her")) mess-list)
                                (push (format nil "~a diapers sprung a leak" (name-of user)) wet-leak-list)
                                (do-push (format nil
                                             "~a's diapers leak all over, there goes the carpet" (name-of user))
                                    wet-leak-list mess-leak-list both-leak-list)
                                (push (format nil "Blowout!!!") mess-leak-list)
                                (push (format nil "Heh, baby made a puddle") wet-leak-list)
                                (push (format nil "~a piddles ~a pamps" (name-of user) (if (malep user) "his" "her")) wet-list))
                            ((wearingp (wear-of user) 'pullon)
                                (when prop
                                    (push (format nil
                                              "~a lifts ~a leg near the ~a and floods ~a pullups"
                                              (name-of user)
                                              (if (malep user) "his" "her")
                                              (name-of prop)
                                              (if (malep user) "his" "her"))
                                        wet-list)
                                    (push (format nil
                                              "~a lifts ~a leg near the ~a and floods ~a pullups, then squats down on all fours and messes"
                                              (name-of user)
                                              (if (malep user) "his" "her")
                                              (name-of prop)
                                              (if (malep user) "his" "her"))
                                        both-list)
                                    (push (format nil
                                              "~a squats down on all fours with ~a tail raised like an animal and messes ~a pullups"
                                              (name-of user)
                                              (if (malep user) "his" "her")
                                              (if (malep user) "his" "her"))
                                        mess-list))
                                (do-push (format nil
                                             "~a's pullups leak all over, there goes the carpet" (name-of user))
                                    wet-leak-list mess-leak-list both-leak-list)
                                (when (wearingp (wear-of user) 'pullup)
                                    (do-push
                                        (format nil
                                            "Bad ~a! You know you're supposed to use the toilet like a big kid"
                                            (if (malep user) "boy" "girl")
                                            )
                                        wet-list mess-list both-list)
                                    (do-push
                                        (format nil
                                            "Bad ~a! Look at the mess you made leaking everywhere like that! Do we have to put you back in diapers?!"
                                            (if (malep user) "boy" "girl")
                                            )
                                        wet-leak-list mess-leak-list both-leak-list))
                                (push (format nil
                                          "~a squats down and messes ~a pullups like a toddler"
                                          (name-of user)
                                          (if (malep user) "his" "her"))
                                    mess-list)
                                (do-push (format nil
                                             "~a goes potty in ~a pullups like a toddler"
                                             (name-of user)
                                             (if (malep user) "his" "her"))
                                    mess-list wet-list both-list)
                                (push (format nil
                                          "~a pauses and floods ~a pullups"
                                          (name-of user)
                                          (if (malep user) "his" "her"))
                                    wet-list)
                                (push (format nil
                                          "~a floods ~a pullups like a toddler"
                                          (name-of user)
                                          (if (malep user) "his" "her"))
                                    wet-list)
                                (push (format nil
                                          "~a mess falls out of ~a pullups and on the floor"
                                          (name-of user)
                                          (if (malep user) "his" "her"))
                                    mess-leak-list)
                                (push (format nil
                                          "~a's pullups sprung a leak"
                                          (name-of user))
                                    wet-leak-list)
                                (push (format nil
                                          "~a makes a puddle"
                                          (name-of user))
                                    wet-leak-list)
                                (when (eq user (player-of *game*))
                                    (push (format nil
                                              "You made a puddle on the floor. You sure you're ready for pullups?")
                                        wet-leak-list)))
                            ((wearingp (wear-of user) 'incontinence-pad)
                                (push (format nil "~a floods ~aself like a toddler"
                                          (name-of user)
                                          (if (malep user) "him" "her"))
                                    wet-list)
                                (push (format nil "~a squats down and mess ~aself like a toddler"
                                          (name-of user)
                                          (if (malep user) "him" "her"))
                                    mess-list)
                                (do-push (format nil "~a goes potty in ~a pants like a toddler"
                                             (name-of user)
                                             (if (malep user) "his" "her"))
                                    wet-list mess-list both-list)
                                (push (format nil "A puddle forms on the floor. Maybe ~a should start wearing diapers"
                                          (if (eq user (player-of *game*)) "you" (name-of user)))
                                    wet-leak-list)
                                (do-push
                                    (format nil "Heh, baby made a mess on the floor")
                                    wet-leak-list mess-leak-list both-leak-list))
                            (t
                                (do-push (format nil
                                             "~a realized ~a made a horrible mistake. ~a's not wearing any padding!!!"
                                             (name-of user)
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
                                             (if (malep user) "his" "her"))
                                    wet-list mess-list both-list)
                                (push (format nil "A puddle forms on the floor. Maybe ~a should start wearing diapers"
                                          (if (eq user (player-of *game*)) "you" (name-of user)))
                                    wet-leak-list)
                                (do-push
                                    (format nil "Heh, baby made a mess on the floor")
                                    wet-leak-list mess-leak-list both-leak-list)))
                        (format-lists)
                        (format-leak-lists)
                        (when (and
                                  (no-puddles-p (get-zone (position-of (player-of *game*))))
                                  (or
                                      (and
                                          (getf wet-return-value :leak-amount)
                                          (> (getf wet-return-value :leak-amount) 0))
                                      (and
                                          (getf mess-return-value :leak-amount)
                                          (> (getf mess-return-value :leak-amount) 0))))
                            (trigger-diaper-police user))))))))
;;; This long ass function could probably be better
(defun process-potty (&optional (user (player-of *game*)))
    (declare (type (or player ally) user))
    (incf (bladder/contents-of user) (bladder/fill-rate-of user))
    (incf (bowels/contents-of user) (bowels/fill-rate-of user))
    (let ((had-accident
              (if (or (eq (player-of *game*) user) (eq (potty-training-of user) :last-minute))
                  (cons
                      (when (>= (bladder/contents-of user) (bladder/maximum-limit-of user))
                          (wet :accident t :wetter user))
                      (when (>= (bowels/contents-of user) (bowels/maximum-limit-of user))
                          (mess :accident t :messer user)))
                  (cons
                      (when (>= (bladder/contents-of user) (bladder/need-to-potty-limit-of user))
                          (wet :wetter user))
                      (when (>= (bowels/contents-of user) (bowels/need-to-potty-limit-of user))
                          (mess :messer user))))))
        (cond
            ((eq (player-of *game*) user)
                (cond ((and (car had-accident) (> (getf (car had-accident) :wet-amount) 0))
                          (format t "~a~%"
                              (let ((j (cond ((<= (getf (car had-accident) :wet-amount) 10)
                                                 (list "You ghasp in horror as a little leaks out"
                                                     "You think you just leaked a little"
                                                     (format nil
                                                         "A little squirts out. You quickly grab yourself with a ~a, but manage to stop the flood"
                                                         (nth (strong-random 2) (list "groan" "whine")))))
                                           ((and (<= (getf (car had-accident) :wet-amount) 300) (> (getf (car had-accident) :wet-amount) 10))
                                               (list "You gasp in  horror as you flood yourself, but manage to stop yourself"))
                                           ((> (getf (car had-accident) :wet-amount) 300)
                                               (list "After doing a potty dance like a 5 year old, you freeze and pee yourself"
                                                   "Grabbing your crotch you pause and blush as you flood yourself like an infant"
                                                   "You cross your legs in a vain attempt to hold it in but fail miserably"
                                                   "You gasp in embaressment as you flood yourself like a toddler"
                                                   "You let out a groan as your bladder empties itself")
                                               )
                                           )))
                                  (when (wearingp (wear-of user) 'closed-bottoms)
                                      (cond ((and (wearingp (wear-of user) 'diaper))
                                                (when (>= (getf (car had-accident) :wet-amount) 300)
                                                    (push (format nil "Aww, the baby is using ~a diapers?" (if (malep user) "his" "her")) j)))
                                          ((wearingp (wear-of user) 'pullup)
                                              (when (>= (getf (car had-accident) :wet-amount) 300)
                                                  (push "The little pictures on the front of your pullups fade showing everyone what you did" j))
                                              (when (>= (getf (car had-accident) :wet-amount) 300)
                                                  (push (format nil
                                                            "Naughty ~a wetting your pullups. You know you're supposed to use the toilet like a big kid."
                                                            (if (malep user) "boy" "girl"))
                                                      j)))
                                          ((wearingp (wear-of user) 'closed-pants)
                                              '(push "a wet patch forms on the front of your pants" j))))
                                  (nth (strong-random (list-length j)) j)))
                          (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
                              (format t "~a~%"
                                  (cond
                                      ((wearingp (wear-of user) 'tabbed-briefs)
                                          (let ((out ()))
                                              (push "Your face turns red as you leak everywhere" out)
                                              (push "Your diaper leaks all over the place, this is why you're supposed to change it" out)
                                              (push "What's the point in having a diaper if you're just going to leak everywhere like you're doing now." out)
                                              (push "Your diaper leaks. There goes the carpet." out)
                                              (push "Heh, baby made a puddle" out)
                                              (push "Your diapers sprung a leak" out)
                                              (nth (strong-random (list-length out)) out)))
                                      ((wearingp (wear-of user) 'pullon)
                                          (let ((out ()))
                                              (push "Your face turns red as you leak everywhere" out)
                                              (when (wearingp (wear-of user) 'pullup)
                                                  (push "Your pullups leaks all over the place, You sure you're ready for those?" out))
                                              (push "Your pullups leak. There goes the carpet." out)
                                              (push "Heh, baby made a puddle" out)
                                              (push "Your pullups sprung a leak" out)
                                              (nth (strong-random (list-length out)) out)))
                                      (t
                                          (format nil "~a"
                                              (let ((j (list "Maybe you should start wearing diapers"
                                                           (format nil "Bad ~a! No going potty in the house!" (if (strong-random 2) (species-of user) (name-of user)))
                                                           "A puddle appears on the floor"
                                                           "There goes the carpet"
                                                           "Heh, baby made a puddle")))
                                                  (when (wearingp (wear-of user) 'closed-pants)
                                                      (push  "Your pants are ruined" j)
                                                      (push (format nil "Heh, baby wet ~a pants" (if (malep user) "his" "her")) j)
                                                      (push (format nil
                                                                "Bad ~a! Look what you did to your pants!"
                                                                (if (strong-random 2) (species-of user) (name-of user)))
                                                          j))
                                                  (nth (strong-random (list-length j)) j))))))))
                    ((>= (bladder/contents-of user) (bladder/potty-dance-limit-of user))
                        (format t "~a~%"
                            (let ((out ()))
                                (push "You feel like your bladder is going to explode" out)
                                (push "You're doing a potty dance like a 5 year old" out)
                                (push "You feel like you're going to wet yourself" out)
                                (push "You whine as you hold yourself in desperation" out)
                                (push "Aww, does the baby need to potty?~%" out)
                                (nth (strong-random (list-length out)) out))))
                    ((>= (bladder/contents-of user) (bladder/need-to-potty-limit-of user))
                        (format t "You need to pee~%")))
                (cond ((and (cdr had-accident) (> (getf (cdr had-accident) :mess-amount) 0))
                          (format t "~a~%"
                              (let ((j (list "Reaching the breaking point, you instinctively squat down with your tail up and mess"
                                           "Your struggle to hold it in, but your bowels decide to empty themselves anyway"
                                           "You try to fart to relieve the pressure, except it wasn't a fart"
                                           "You end up messing your self")))
                                  (when (wearingp (wear-of user) 'closed-bottoms)
                                      (cond ((wearingp (wear-of user) 'tabbed-briefs)
                                                (push "The back of your diaper expands as you accidentally mess yourself" j)
                                                (when (wearingp (wear-of user) 'diaper)
                                                    (push (format nil "Aww, is the baby messing ~a diapers" (if (malep user) "his" "her")) j))
                                                (push (format nil "Heh, the baby blorted ~a pamps." (if (malep user) "his" "her")) j))
                                          ((wearingp (wear-of user) 'pullon)
                                              (push "The back of your pullups expands as you accidentally mess yourself" j)
                                              (when (wearingp (wear-of user) 'pullup)
                                                  (push (format nil
                                                            "Naughty ~a messing your pullups. You know you're supposed to use the toilet like a big kid"
                                                            (if (malep user) "boy" "girl"))
                                                      j)))
                                          (t
                                              (push "a lump forms at the seat of your pants" j))))
                                  (nth (strong-random (list-length j)) j)))
                          (when (and (cdr had-accident) (> (getf (cdr had-accident) :leak-amount) 0))
                              (format t "~a~%"
                                  (cond
                                      ((wearingp (wear-of user) 'tabbed-briefs)
                                          (let ((out ()))
                                              (push "Your face turns red as your mess falls out the leg guards" out)
                                              (push "Your diaper leaks all over the place, this is why you're supposed to change it" out)
                                              (push "What's the point in having a diaper if you're just going to leak everywhere like you're doing now." out)
                                              (push "Your diaper leaks. There goes the carpet." out)
                                              (push "Not on the carpet!!!" out)
                                              (push "Blowout!!!!" out)
                                              (nth (strong-random (list-length out)) out)))
                                      ((wearingp (wear-of user) 'pullon)
                                          (let ((out ()))
                                              (push "Your face turns red as your mess falls out the leg guards" out)
                                              (push "Your pullups leaks all over the place, You sure you're ready for those?" out)
                                              (push "Your pullups leak. There goes the carpet." out)
                                              (push "Not on the carpet!!!" out)
                                              (nth (strong-random (list-length out)) out)))
                                      (t
                                          (format nil "~a"
                                              (let ((j (list "Maybe you should start wearing diapers"
                                                           (format nil
                                                               "Bad ~a! No going potty in the house!"
                                                               (if (strong-random 2) (species-of user) (name-of user)))
                                                           "There goes the carpet"
                                                           "Heh, baby made a mess")))
                                                  (when (wearingp (wear-of user) 'closed-pants)
                                                      (push  "Your pants are ruined" j)
                                                      (push (format nil "Heh, baby messed ~a pants" (if (malep user) "his" "her")) j)
                                                      (push (format nil
                                                                "Bad ~a! Look what you did to your pants!"
                                                                (if (strong-random 2) (species-of user) (name-of user)))
                                                          j))
                                                  (nth (strong-random (list-length j)) j))))))))
                    ((>= (bowels/contents-of user) (bowels/potty-dance-limit-of user))
                        (format t "~a~%"
                            (let ((out ()))
                                (push "You feel like you're gonna mess yourself" out)
                                (push "You clench hard trying to avoid messing" out)
                                (push "You fart a little due to the pressure" out)
                                (push "Aww, does the baby need to potty?" out)
                                (nth (strong-random (list-length out)) out))))
                    ((>= (bowels/contents-of user) (bowels/need-to-potty-limit-of user))
                        (format t "You need to poo~%"))))
            ((or (eq (potty-training-of user) :last-minute))
                (let ((wet-list ())
                         (mess-list ())
                         (wet-leak-list ())
                         (mess-leak-list ())
                         (gotta-pee-list ())
                         (gotta-poo-list ())
                         (gotta-both-list ())
                         (potty-dance-both-list ())
                         (potty-dance-pee-list ())
                         (potty-dance-poo-list ()))
                    (flet ((format-lists ()
                               (if (or (car had-accident) (cdr had-accident))
                                   (progn
                                       (cond
                                           ((and
                                                (car had-accident)
                                                (> (getf (car had-accident) :leak-amount) 0))
                                               (format t "~a" (nth (strong-random (list-length wet-leak-list)) wet-leak-list)))
                                           ((and
                                                (car had-accident)
                                                (> (getf (car had-accident) :wet-amount) 0))
                                               (format t "~a" (nth (strong-random (list-length wet-list)) wet-list))))
                                       (cond
                                           ((and
                                                (cdr had-accident)
                                                (> (getf (cdr had-accident) :leak-amount) 0))
                                               (format t "~a" (nth (strong-random (list-length mess-leak-list)) mess-leak-list)))
                                           ((and
                                                (cdr had-accident)
                                                (> (getf (cdr had-accident) :mess-amount) 0))
                                               (format t "~a" (nth (strong-random (list-length mess-list)) mess-list))))
                                       )
                                   (cond
                                       ((and (>=
                                                 (bladder/contents-of user)
                                                 (+ (bladder/potty-dance-limit-of user)
                                                     (/
                                                         (-
                                                             (bladder/maximum-limit-of user)
                                                             (bladder/potty-dance-limit-of user))
                                                         2)))
                                            (>=
                                                (bowels/contents-of user)
                                                (+ (bowels/potty-dance-limit-of user)
                                                    (/
                                                        (-
                                                            (bowels/maximum-limit-of user)
                                                            (bowels/potty-dance-limit-of user))
                                                        2))))
                                           (format t "~a" (nth (strong-random (list-length gotta-both-list)) gotta-both-list)))
                                       ((>=
                                            (bladder/contents-of user)
                                            (+ (bladder/potty-dance-limit-of user)
                                                (/
                                                    (-
                                                        (bladder/maximum-limit-of user)
                                                        (bladder/potty-dance-limit-of user))
                                                    2)))
                                           (format t "~a" (nth (strong-random (list-length gotta-pee-list)) gotta-pee-list)))
                                       ((>=
                                            (bowels/contents-of user)
                                            (+ (bowels/potty-dance-limit-of user)
                                                (/
                                                    (-
                                                        (bowels/maximum-limit-of user)
                                                        (bowels/potty-dance-limit-of user))
                                                    2)))
                                           (format t "~a" (nth (strong-random (list-length gotta-poo-list)) gotta-poo-list)))
                                       ((and
                                            (>= (bowels/contents-of user) (bowels/potty-dance-limit-of user))
                                            (>= (bladder/contents-of user) (bladder/potty-dance-limit-of user)))
                                           (format t "~a" (nth (strong-random (list-length potty-dance-both-list)) potty-dance-both-list)))
                                       ((>= (bowels/contents-of user) (bowels/potty-dance-limit-of user))
                                           (format t "~a" (nth (strong-random (list-length potty-dance-poo-list)) potty-dance-poo-list)))
                                       ((>= (bladder/contents-of user) (bladder/potty-dance-limit-of user))
                                           (format t "~a" (nth (strong-random (list-length potty-dance-pee-list)) potty-dance-pee-list)))))))
                        (cond
                            ((and (car had-accident) (= (getf (car had-accident) :wet-amount) 10))
                                (do-push (with-output-to-string (s)
                                             (format s "~a: *gasps in horror* I think a little just came out!!!!~%~%" (name-of user))
                                             (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
                                                 (format s "~a: You're making a puddle~%~%" (name-of (player-of *game*)))
                                                 (format s "~a: NUUUUU!!!!~%~%" (name-of user))))
                                    wet-list wet-leak-list)
                                (do-push (with-output-to-string (s)
                                             (format s "*~a quietly moans at a little squirts out~%~%" (name-of user))
                                             (format s "~a: Did you wet yourself?~%~%" (name-of (player-of *game*)))
                                             (format s "~a: *quietly* No ~%~%" (name-of user))
                                             (cond
                                                 ((wearingp (wear-of user) 'tabbed-briefs)
                                                     (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
                                                         (format s "~a Your diaper's leaking~%~%" (name-of (player-of *game*)))
                                                         (format s "~a: GAH!!!!~%~%" (name-of (player-of *game*))))
                                                     )
                                                 ((wearingp (wear-of user) 'pullup)
                                                     (format s "~a: Those pictures on the front of your pullups better not fade~%~%"
                                                         (name-of (player-of *game*)))
                                                     (format s "~a: They're not, honest ~%~%" (name-of user))
                                                     (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
                                                         (format s "*~a checks ~a's pullups. Notices they're drenched~%~%" (name-of (player-of *game*)) (name-of user))
                                                         (format s "~a: Uh huh, sure~%~%" (name-of (player-of *game*))))
                                                     )))
                                    wet-list wet-leak-list))
                            ((not (wearingp (wear-of user) 'closed-bottoms))
                                (do-push
                                    (with-output-to-string (s)
                                        (format s
                                            "*~a crosses ~a legs pressing ~a paws against ~a crotch as a puddle forms beneath ~a feet*~%~%"
                                            (name-of user)
                                            (if (malep user) "his" "her")
                                            (if (malep user) "his" "her")
                                            (if (malep user) "his" "her")
                                            (if (malep user) "his" "her")))
                                    wet-list wet-leak-list)
                                (do-push
                                    (with-output-to-string (s)
                                        (format s
                                            "*~a accidentally messes ~aself as it falls on the floor*~%~%"
                                            (name-of user)
                                            (if (malep user) "his" "her"))
                                        (format s "~a: Bad ~a!!! No going potty on the floor!!!~%~%"
                                            (name-of (player-of *game*))
                                            (name-of user))
                                        (format s "~a: I didn't mean to!!!~%~%"
                                            (name-of user))
                                        (format s "~a: A likely story~%~%"
                                            (name-of (player-of *game*))))
                                    mess-list mess-leak-list)
                                (do-push
                                    (with-output-to-string (s)
                                        (format s "*~a has an accident and makes a mess on the floor. " (name-of user))
                                        (format s "Then walks away heavily blushing hoping no one will notice*~%~%"))
                                    wet-list wet-leak-list mess-list mess-leak-list))
                            ((wearingp (wear-of user) 'tabbed-briefs)
                                (do-push (with-output-to-string (s)
                                             (format s "*~a has an accident*~%~%"
                                                 (name-of user))
                                             (format s "~a: Aww, did the baby wet ~a diapers~%~%"
                                                 (name-of (player-of *game*))
                                                 (if (malep user) "his" "her"))
                                             (format s "~a: *heavily blushing* No *tries to hide it with ~a paws and tail*~%~%"
                                                 (name-of user)
                                                 (if (malep user) "his" "her"))
                                             (format s "*~a squishes ~a's diaper*~%~%"
                                                 (name-of (player-of *game*))
                                                 (name-of user))
                                             (format s "~a: Looks like it to me~%~%" (name-of (player-of *game*))))
                                    wet-list)
                                (do-push (with-output-to-string (s)
                                             (format s "*~a has an accident*~%~%"
                                                 (name-of user))
                                             (format s "~a: Aww, did the baby wet ~a diapers~%~%"
                                                 (name-of (player-of *game*))
                                                 (if (malep user) "his" "her"))
                                             (format s "~a: *heavily blushing* No *tries to hide it with ~a paws and tail*~%~%"
                                                 (name-of user)
                                                 (if (malep user) "his" "her"))
                                             (format s "~a: Aww, the poor baby made puddles~%~%" (name-of (player-of *game*)))
                                             (format s "*~a gasps with a horrified look on ~a face when ~a notices it.~%~%"
                                                 (name-of user)
                                                 (if (malep user) "his" "her")
                                                 (if (malep user) "he" "she")))
                                    wet-leak-list)
                                (do-push (with-output-to-string (s)
                                             (format s "*~a has an accident*~%~%"
                                                 (name-of user))
                                             (format s "~a: Aww, did the baby mess ~a diapers~%~%"
                                                 (name-of (player-of *game*))
                                                 (if (malep user) "his" "her"))
                                             (format s "~a: *heavily blushing* No *tries to hide it with ~a paws and tail*~%~%"
                                                 (name-of user)
                                                 (if (malep user) "his" "her"))
                                             (format s "*~a pats the back of ~a's diaper causing ~a to scrunch ~a face*~%~%"
                                                 (name-of (player-of *game*))
                                                 (name-of user)
                                                 (name-of user)
                                                 (if (malep user) "his" "her"))
                                             (format s "~a: Looks like it to me~%~%" (name-of (player-of *game*))))
                                    mess-list)
                                (do-push (with-output-to-string (s)
                                             (format s "*~a has an accident*~%~%"
                                                 (name-of user))
                                             (format s "~a: Aww, did the baby mess ~a diapers~%~%"
                                                 (name-of (player-of *game*))
                                                 (if (malep user) "his" "her"))
                                             (format s "~a: *heavily blushing* No *tries to hide it with ~a paws and tail*~%~%"
                                                 (name-of user)
                                                 (if (malep user) "his" "her"))
                                             (format s "~a: Aww, the poor baby made a mess on the floor~%~%" (name-of (player-of *game*)))
                                             (format s "*~a gasps with a horrified look on ~a face when ~a notices it.~%~%"
                                                 (name-of user)
                                                 (if (malep user) "his" "her")
                                                 (if (malep user) "he" "she")))
                                    mess-leak-list)
                                (do-push (with-output-to-string (s)
                                             (format s "*~a bounces up and down with ~a knees knocked together and paws pressed against ~a crotch, pauses when ~a bladder gives out looks down and notices "
                                                 (name-of user)
                                                 (if (malep user) "his" "her")
                                                 (if (malep user) "his" "her")
                                                 (if (malep user) "his" "her"))
                                             (format s "~a soggy padding, blushes heavily and quickly covers ~a soggy padding with ~a paws and tail hoping no one will notice*~%~%"
                                                 (if (malep user) "his" "her")
                                                 (if (malep user) "his" "her")
                                                 (if (malep user) "his" "her"))
                                             )
                                    wet-list)
                                (do-push (with-output-to-string (s)
                                             (format s "*~a bounces up and down with ~a knees knocked together and paws pressed against ~a crotch, pauses when ~a bladder gives out looks down and notices "
                                                 (name-of user)
                                                 (if (malep user) "his" "her")
                                                 (if (malep user) "his" "her")
                                                 (if (malep user) "his" "her"))
                                             (format s "~a padding is leaking, blushes heavily and quickly covers ~a soggy padding with ~a paws and tail hoping no one will notice*~%~%"
                                                 (if (malep user) "his" "her")
                                                 (if (malep user) "his" "her")
                                                 (if (malep user) "his" "her"))
                                             )
                                    wet-leak-list)
                                (do-push (with-output-to-string (s)
                                             (format s "*~a instinctively squats down and accidentally messes ~a diapers then gasps in horror when ~a realized what ~a did*~%~%"
                                                 (name-of user)
                                                 (if (malep user) "his" "her")
                                                 (if (malep user) "he" "she")
                                                 (if (malep user) "he" "she")))
                                    mess-list)
                                (do-push (with-output-to-string (s)
                                             (format s "*~a instinctively squats down and accidentally messes ~a diapers then gasps in horror when ~a notices the poo falling down the leg guards*~%~%"
                                                 (name-of user)
                                                 (if (malep user) "his" "her")
                                                 (if (malep user) "he" "she")))
                                    mess-leak-list))
                            ((wearingp (wear-of user) 'pullon)
                                (do-push (with-output-to-string (s)
                                             (format s "*~a bounces up and down with ~a knees knocked together and paws pressed against ~a crotch, pauses when ~a bladder gives out looks down and notices "
                                                 (name-of user)
                                                 (if (malep user) "his" "her")
                                                 (if (malep user) "his" "her")
                                                 (if (malep user) "his" "her"))
                                             (if (wearingp (wear-of user) 'pullup)
                                                 (format s "the pictures on ~a pullups have faded, blushes heavily and quickly covers ~a soggy pullups with ~a paws and tail hoping no one will notice*~%~%"
                                                     (if (malep user) "his" "her")
                                                     (if (malep user) "his" "her")
                                                     (if (malep user) "his" "her"))
                                                 (format s "that ~a wetted ~a pullups, blushes heavily and quickly covers ~a soggy pullups with ~a paws and tail hoping no one will notice*~%~%"
                                                     (if (malep user) "he" "she")
                                                     (if (malep user) "his" "her")
                                                     (if (malep user) "his" "her")
                                                     (if (malep user) "his" "her"))))
                                    wet-list wet-leak-list)
                                (do-push (with-output-to-string (s)
                                             (format s "*~a instinctively squats down and accidentally messes ~a pullups then gasps in horror when ~a realized what ~a did*~%~%"
                                                 (name-of user)
                                                 (if (malep user) "his" "her")
                                                 (if (malep user) "he" "she")
                                                 (if (malep user) "he" "she")))
                                    mess-list)
                                (do-push (with-output-to-string (s)
                                             (format s "*~a instinctively squats down and accidentally messes ~a pullups then gasps in horror when ~a notices the poo falling down the leg guards*~%~%"
                                                 (name-of user)
                                                 (if (malep user) "his" "her")
                                                 (if (malep user) "he" "she")))
                                    mess-leak-list)
                                (do-push (with-output-to-string (s)
                                             (format s "*~a has an accident*~%~%"
                                                 (name-of user))
                                             (format s "~a: Bad ~a. You know you're supposed to use the toilet like a big ~a. Just look what you did to your pullups~%~%"
                                                 (name-of (player-of *game*))
                                                 (name-of user)
                                                 (if (malep user) "boy" "girl")))
                                    wet-list mess-list)
                                (when (wearingp (wear-of user) 'pullup)
                                    (do-push (with-output-to-string (s)
                                                 (format s "*~a has an accident and leaks*~%~%"
                                                     (name-of user))
                                                 (format s "~a: Bad ~a. You know you're supposed to use the toilet like a big ~a. Just look at the mess you made on the floor~%~%"
                                                     (name-of (player-of *game*))
                                                     (name-of user)
                                                     (if (malep user) "boy" "girl")))
                                        wet-leak-list mess-leak-list)))
                            (t
                                (do-push (with-output-to-string (s)
                                             (format s "*~a bounces up and down with ~a knees knocked together and paws pressed against ~a crotch, pauses when ~a bladder gives out looks down and notices "
                                                 (name-of user)
                                                 (if (malep user) "his" "her")
                                                 (if (malep user) "his" "her")
                                                 (if (malep user) "his" "her"))
                                             (format s "~a flooded ~aself, blushes heavily and quickly covers the front of ~a pants with ~a paws and tail hoping no one will notice*~%~%"
                                                 (if (malep user) "his" "her")
                                                 (if (malep user) "him" "her")
                                                 (if (malep user) "his" "her")
                                                 (if (malep user) "his" "her")))
                                    wet-list wet-leak-list)
                                (do-push (with-output-to-string (s)
                                             (format s "*~a instinctively squats down and accidentally messes ~a pants then gasps in horror when ~a realized what ~a did*~%~%"
                                                 (name-of user)
                                                 (if (malep user) "his" "her")
                                                 (if (malep user) "he" "she")
                                                 (if (malep user) "he" "she")))
                                    mess-list mess-leak-list)))
                        (if (= (strong-random 5) 0)
                            (progn
                                (do-push (with-output-to-string (s)
                                             (format s "~a: ~a, do you need to potty?~%~%"
                                                 (name-of (player-of *game*))
                                                 (name-of user))
                                             (format s "~a: No, I'm fine *bounces up and down holding ~aself*~%~%"
                                                 (name-of user)
                                                 (if (malep user) "him" "her")))
                                    potty-dance-both-list
                                    potty-dance-pee-list
                                    potty-dance-poo-list)
                                (do-push (with-output-to-string (s)
                                             (format s "~a: ~a, do you need to potty?~%~%"
                                                 (name-of (player-of *game*))
                                                 (name-of user))
                                             (format s "~a: No, I'm ok *hops from foot to foot holding ~a crotch*~%~%"
                                                 (name-of user)
                                                 (if (malep user) "his" "her")))
                                    potty-dance-both-list
                                    potty-dance-pee-list
                                    potty-dance-poo-list)
                                (do-push (with-output-to-string (s)
                                             (format s "~a: ~a, do you need to potty?~%~%"
                                                 (name-of (player-of *game*))
                                                 (name-of user))
                                             (format s "~a: No, I'm alright *moans with ~a legs twisted holding ~a crotch*~%~%"
                                                 (name-of user)
                                                 (if (malep user) "his" "her")
                                                 (if (malep user) "his" "her")))
                                    potty-dance-both-list
                                    potty-dance-pee-list
                                    potty-dance-poo-list)
                                (do-push (with-output-to-string (s)
                                             (format s "~a: ~a!!! I GOTTY POTTY!!! *bounces up and down holding ~aself*~%~%"
                                                 (name-of user)
                                                 (name-of (player-of *game*))
                                                 (if (malep user) "him" "her")))
                                    gotta-pee-list
                                    gotta-poo-list
                                    gotta-both-list)
                                (do-push (with-output-to-string (s)
                                             (format s "~a: ~a!!! HURRY!!! I CAN'T HOLD IT MUCH LONGER!!! *hops from foot to foot holding ~a crotch*~%~%"
                                                 (name-of user)
                                                 (name-of (player-of *game*))
                                                 (if (malep user) "his" "her")))
                                    gotta-pee-list
                                    gotta-poo-list
                                    gotta-both-list)
                                (do-push (with-output-to-string (s)
                                             (format s "~a: ~a!!! PLEASE TAKE ME TO THE POTTY!!! I'M ABOUT TO WET MYSELF!!! *bounces up and down holding ~aself*~%~%"
                                                 (name-of user)
                                                 (name-of (player-of *game*))
                                                 (if (malep user) "him" "her")))
                                    gotta-pee-list
                                    gotta-both-list))
                            (progn
                                (do-push (with-output-to-string (s)
                                             (format s "*~a is doing a potty dance like a 5 year old*~%~%"
                                                 (name-of user)))
                                    gotta-pee-list
                                    gotta-poo-list
                                    gotta-both-list
                                    potty-dance-both-list
                                    potty-dance-pee-list
                                    potty-dance-poo-list)
                                (do-push (with-output-to-string (s)
                                             (format s "*~a farts to relieve the pressure*~%~%"
                                                 (name-of user)))
                                    gotta-poo-list gotta-both-list potty-dance-both-list potty-dance-poo-list)
                                (do-push (with-output-to-string (s)
                                             (format s "*~a is bouncing up and down with ~a knees knocked together holding ~aself*~%~%"
                                                 (name-of user)
                                                 (if (malep user) "his" "her")
                                                 (if (malep user) "him" "her")))
                                    gotta-pee-list
                                    gotta-poo-list
                                    gotta-both-list
                                    potty-dance-both-list
                                    potty-dance-pee-list
                                    potty-dance-poo-list)
                                (do-push (with-output-to-string (s)
                                             (format s "*~a is hopping from foot to foot*~%~%"
                                                 (name-of user)))
                                    gotta-pee-list
                                    gotta-poo-list
                                    gotta-both-list
                                    potty-dance-both-list
                                    potty-dance-pee-list
                                    potty-dance-poo-list)
                                (do-push (with-output-to-string (s)
                                             (format s "*~a starts moaning with ~a legs crossed*~%~%"
                                                 (name-of user)
                                                 (if (malep user) "his" "her")))
                                    gotta-pee-list
                                    gotta-poo-list
                                    gotta-both-list
                                    potty-dance-both-list
                                    potty-dance-pee-list
                                    potty-dance-poo-list)))
                        (format-lists))))
            (t
                (let ((wet-list ())
                         (mess-list ())
                         (wet-leak-list ())
                         (mess-leak-list ()))
                    (flet ((format-lists ()
                               (when (or (car had-accident) (cdr had-accident))
                                   (cond
                                       ((and
                                            (car had-accident)
                                            (> (getf (car had-accident) :leak-amount) 0))
                                           (format t "~a" (nth (strong-random (list-length wet-leak-list)) wet-leak-list)))
                                       ((and
                                            (car had-accident)
                                            (> (getf (car had-accident) :wet-amount) 0))
                                           (format t "~a" (nth (strong-random (list-length wet-list)) wet-list))))
                                   (cond
                                       ((and
                                            (cdr had-accident)
                                            (> (getf (cdr had-accident) :leak-amount) 0))
                                           (format t "~a" (nth (strong-random (list-length mess-leak-list)) mess-leak-list)))
                                       ((and
                                            (cdr had-accident)
                                            (> (getf (cdr had-accident) :mess-amount) 0))
                                           (format t "~a" (nth (strong-random (list-length mess-list)) mess-list)))))))
                        (cond ((eq (potty-training-of user) :rebel)
                                  (do-push (with-output-to-string (s)
                                               (format s "*~a stops in his tracks*~%~%"
                                                   (name-of user))
                                               (format s "~a: Is something the matter?~%~%"
                                                   (name-of (player-of *game*)))
                                               (format s "~a: what do you mean? *a soft hiss can be heard coming from the front of ~a diaper*~%~%"
                                                   (name-of user)
                                                   (if (malep user) "his" "her"))
                                               (format s "~a: Oh, never mind~%~%"
                                                   (name-of (player-of *game*))))
                                      wet-list)
                                  (do-push (with-output-to-string (s)
                                               (format s "*~a pauses and floods ~a diapers*~%~%"
                                                   (name-of user)
                                                   (if (malep user) "his" "her")))
                                      wet-list)
                                  (do-push (with-output-to-string (s)
                                               (format s "*~a stops in his tracks*~%~%"
                                                   (name-of user))
                                               (format s "~a: Is something the matter?~%~%"
                                                   (name-of (player-of *game*)))
                                               (format s "~a: what do you mean? *a soft hiss can be heard coming from the front of ~a diaper, then yellow streams flow down ~a legs from the leg guards and a puddle starts forming at ~a feet*~%~%"
                                                   (name-of user)
                                                   (if (malep user) "his" "her")
                                                   (if (malep user) "his" "her")
                                                   (if (malep user) "his" "her"))
                                               (format s "~a: You're making a puddle~%~%"
                                                   (name-of (player-of *game*)))
                                               (format s "~a: Oh No!!!~%"
                                                   (name-of user)))
                                      wet-leak-list)))
                        (do-push (with-output-to-string (s)
                                     (format s "*~a floods ~a diapers*~%~%"
                                         (name-of user)
                                         (if (malep user) "his" "her")))
                            wet-list)
                        (do-push (with-output-to-string (s)
                                     (format s "*~a floods ~a nappies, then leaks and leaves puddles*~%~%"
                                         (name-of user)
                                         (if (malep user) "his" "her")))
                            wet-leak-list)
                        (do-push (with-output-to-string (s)
                                     (format s "*~a floods his nappies, then gets an expression of horror on ~a face when ~a diaper leaks and a puddle forms, then starts waddling with ~a legs spread apart*~%~%"
                                         (name-of user)
                                         (if (malep user) "his" "her")
                                         (if (malep user) "his" "her")
                                         (if (malep user) "his" "her")))
                            wet-leak-list)
                        (do-push (with-output-to-string (s)
                                     (format s "*~a decides to flood ~a already waterlogged diaper, then acts all surprised when it leaks*~%~%"
                                         (name-of user)
                                         (if (malep user) "his" "her")))
                            wet-leak-list)
                        (do-push (with-output-to-string (s)
                                     (format s "*~a floods his diapers and starts leaving a puddle, then freaks and waddles towards ~a with ~a legs spread apart like a 5 year old who didn't make it*~%~%"
                                         (name-of user)
                                         (name-of (player-of *game*))
                                         (if (malep user) "his" "her"))
                                     (format s "~a: Umm ~a, I think I need a change.~%~%"
                                         (name-of user)
                                         (name-of (player-of *game*)))
                                     (format s "~a: No shit~%~%"
                                         (name-of (player-of *game*))))
                            wet-leak-list)
                        (do-push (with-output-to-string (s)
                                     (format s "*~a squats down and pushes a big load into ~a diaper like an infant*~%~%"
                                         (name-of user)
                                         (if (malep user) "his" "her")))
                            mess-list)
                        (do-push (with-output-to-string (s)
                                     (format s "*~a squats down and pushes a big load into ~a already loaded diaper, then predictably has a blowout*~%~%"
                                         (name-of user)
                                         (if (malep user) "his" "her")))
                            mess-leak-list)
                        (format-lists)))))
        (when (and
                  (no-puddles-p (get-zone (position-of (player-of *game*))))
                  (or
                      (and (getf (car had-accident) :leak-amount) (> (getf (car had-accident) :leak-amount) 0))
                      (and (getf (cdr had-accident) :leak-amount) (> (getf (cdr had-accident) :leak-amount) 0))))
            (trigger-diaper-police user))
        had-accident))
(defun get-props-from-zone (position)
    (props-of (eval (get-zone position))))
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
(defun nshuffle (sequence)
    (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (strong-random i))
               (elt sequence (1- i))))
    sequence)
(defun remove-nth (n sequence)
    (remove-if (constantly t) sequence :start n :count 1))
(defun insert (list value n)
    (if (<= n 0)
        (cons value list)
        (cons (car list) (insert (cdr list) value (- n 1)))))
(defun pushnewmove (move* user)
    (pushnew (make-instance move*) (moves-of user)
        :test (lambda (a b)
                  (eql (class-name (class-of a)) (class-name (class-of b))))))
(defun get-move (move* user)
    (first
        (member move* (moves-of user)
            :test (lambda (a b)
                      (if (typep a 'keyword)
                          (string= a (class-name (class-of b)))
                          (eql a (class-name (class-of b))))))))
(defun random-from-range (start end)
    (+ start (strong-random (+ 1 (- end start)))))
(defun calculate-diaper-usage (user)
    (iter (with j = (list :sogginess 0 :sogginess-capacity 0 :messiness 0 :messiness-capacity 0))
        (for i in (wearingp (wear-of user) 'closed-bottoms))
        (incf (getf j :sogginess) (sogginess-of i))
        (incf (getf j :sogginess-capacity) (sogginess-capacity-of i))
        (incf (getf j :messiness) (messiness-of i))
        (incf (getf j :messiness-capacity) (messiness-capacity-of i))
        (finally (return j))))
(defun calculate-level-to-exp (level)
    (expt level 3))
(defun calculate-exp-yield (target)
    ($ (exp-yield-of target) * (level-of target) / 7))
(defun calculate-wear-stats (user)
    (declare (type base-character user))
    (iter
        (with j = (list :health 0 :attack 0 :defense 0 :energy 0))
        (for i in (wear-of user))
        (iter
            (for (a b) on (wear-stats-of i) by #'cddr)
            (incf (getf j a) b))
        (finally (return j))))
(defun calculate-wield-stats (user)
    (declare (type base-character user))
    (iter
        (with j = (list :health 0 :attack 0 :defense 0 :energy 0))
        (for (a b) on (if (wield-of user) (wield-stats-of (wield-of user)) ()) by #'cddr)
        (incf (getf j a) b)
        (finally (return j))))
(defun calculate-stat-delta (user)
    (declare (type base-character user))
    (iter
        (with j = (list :health 0 :attack 0 :defense 0 :energy 0))
        (for i in (when *battle* (getf (status-conditions-of *battle*) user)))
        (iter
            (for (a b) on (stat-delta-of i) by #'cddr)
            (incf (getf j a) b))
        (finally (return j))))
(defun calculate-stat-multiplier (user)
    (declare (type base-character user))
    (iter
        (with j = (list :health 1 :attack 1 :defense 1 :energy 1))
        (for i in (when *battle* (getf (status-conditions-of *battle*) user)))
        (iter
            (for (a b) on (stat-multiplier-of i) by #'cddr)
            (declare (ignorable b))
            (setf (getf j a) (* (getf j a))))
        (finally (return j))))
(defun calculate-stat (user stat-key)
    (declare (type base-character user))
    (round (if (or (eq stat-key :health) (eq stat-key :energy))
               ($ ($ ($ ($ ($ (getf (base-stats-of user) stat-key) +
                               (getf (iv-stats-of user) stat-key) +
                               (getf (calculate-wear-stats user) stat-key) +
                               (getf (calculate-wield-stats user) stat-key) +
                               (getf (calculate-stat-delta user) stat-key))
                            * (getf (calculate-stat-multiplier user) stat-key)
                            * 2)
                         * (level-of user))
                      / 100)
                   + (level-of user) + 10)
               ($ ($ ($ ($ ($ (getf (base-stats-of user) stat-key) +
                               (getf (iv-stats-of user) stat-key) +
                               (getf (calculate-wear-stats user) stat-key) +
                               (getf (calculate-wield-stats user) stat-key) +
                               (getf (calculate-stat-delta user) stat-key))
                            * (getf (calculate-stat-multiplier user) stat-key)
                            * 2)
                         * (level-of user))
                      / 100)
                   + 5))))

(defun calculate-damage (target user attack-base)
    (declare (type base-character user target) (type number attack-base))
    (round ($ ($ ($ ($ ($ ($ ($ 2 * (level-of user)) / 5) + 2) * attack-base * ($ (calculate-stat user :attack) / (calculate-stat target :defense)))
                     / 50)
                  + 2)
               * ($ (random-from-range 85 100) / 100))))

(defun format-stats (user)
    (format t "Name: ~a~%"
        (name-of user))
    (format t "Species: ~a~%"
        (species-of user))
    (format t "Description: ~a~%"
        (description-of user))
    (format t "Health: ")
    (color-format (cond ((> (health-of user) (* (calculate-stat user :health) 1/2)) :green)
                      ((> (health-of user) (* (calculate-stat user :health) 1/4)) :yellow)
                      (t :red)) "[~{~a~}]"
        (iter (with i = 0)
            (while (< i 40))
            (collect (if (< i (* (/ (health-of user) (calculate-stat user :health)) 40)) "#" " "))
            (incf i)))
    (format t " ~a~%Energy: "
        (if (member user (append (allies-of *game*) (list (player-of *game*))))
            (format nil "(~a/~a)"
                (health-of user)
                (calculate-stat user :health))
            ""))
    (color-format (cond ((> (energy-of user) (* (calculate-stat user :energy) 1/2)) :green)
                      ((> (energy-of user) (* (calculate-stat user :energy) 1/4)) :yellow)
                      (t :red)) "[~{~a~}]"
        (iter (with i = 0)
            (while (< i 40))
            (collect (if (< i (* (/ (energy-of user) (calculate-stat user :energy)) 40)) "#" " "))
            (incf i)))
    (format t " ~a~%"
        (if (member user (append (allies-of *game*) (list (player-of *game*))))
            (format nil "(~a/~a)"
                (energy-of user)
                (calculate-stat user :energy))
            ""))
    (when *battle*
        (format t "Conditions: ")
        (iter (for i in (getf (status-conditions-of *battle*) user))
            (format t "`~a' " (name-of i)))
        (format t "~%"))
    (format t "Stats: ~a~%Base-Stats: ~a~%"
        (let ((wield-stats (calculate-wield-stats user))
                 (wear-stats (calculate-wear-stats user)))
            (iter (for (a b) on (base-stats-of user) by #'cddr)
                (collect a)
                (collect (+ b (getf wield-stats a) (getf wear-stats a)))))
        (base-stats-of user))
    (cond
        ((or
             (wearingp (wear-of user) 'tabbed-briefs)
             (wearingp (wear-of user) 'pullon)
             (wearingp (wear-of user) 'incontinence-pad))
            (format t "Diaper State: ~{~a~}~%"
                (let ((a ()) (b (calculate-diaper-usage user)))
                    (cond
                        ((>= (getf b :sogginess) (getf b :sogginess-capacity))
                            (push "Leaking" a)
                            (push " " a))
                        ((>= (getf b :sogginess) 100)
                            (push "Soggy" a)
                            (push " " a)))
                    (cond
                        ((>= (getf b :messiness) (getf b :messiness-capacity))
                            (push "Blowout" a))
                        ((>= (getf b :messiness) 100)
                            (push "Messy" a)))
                    (unless a (push "Clean" a))
                    a)))
        ((wearingp (wear-of user) 'closed-bottoms)
            (format t "Pants State: ~{~a~}~%"
                (let ((a ()) (b (calculate-diaper-usage user)))
                    (cond
                        ((>= (getf b :sogginess) (getf b :sogginess-capacity))
                            (push "Wet" a)
                            (push " " a)))
                    (cond
                        ((>= (getf b :messiness) (getf b :messiness-capacity))
                            (push "Messy" a)))
                    (unless a (push "Clean" a))
                    a))))
    (format t "Bladder State: ")
    (color-format (cond ((>= (bladder/contents-of user) (bladder/potty-dance-limit-of user)) :red)
                      ((>= (bladder/contents-of user) (bladder/need-to-potty-limit-of user)) :yellow)
                      (t :green)) "[~{~a~}]~%"
        (iter (with i = 0)
            (while (< i 40))
            (collect (if (< i (* (/ (bladder/contents-of user) (bladder/maximum-limit-of user)) 40)) "#" " "))
            (incf i)))
    (format t "Bowels State:  ")
    (color-format (cond ((>= (bowels/contents-of user) (bowels/potty-dance-limit-of user)) :red)
                      ((>= (bowels/contents-of user) (bowels/need-to-potty-limit-of user)) :yellow)
                      (t :green)) "[~{~a~}]~%"
        (iter (with i = 0)
            (while (< i 40))
            (collect (if (< i (* (/ (bowels/contents-of user) (bowels/maximum-limit-of user)) 40)) "#" " "))
            (incf i))))

(defun finish-battle (&optional lose)
    (if lose
        (progn
            (format t "~a was defeated~%" (name-of (player-of *game*)))
            (setf (position-of (player-of *game*)) (warp-on-death-point-of (player-of *game*)))
            (format t
                "~a blacked out and flooded and messed ~aself~%~a wakes up and looks at ~a GPS to find out ~a's at ~a at ~a~%"
                (name-of (player-of *game*))
                (if (malep (player-of *game*)) "him" "her")
                (name-of (player-of *game*))
                (if (malep (player-of *game*)) "his" "her")
                (if (malep (player-of *game*)) "he" "she")
                (name-of (get-zone (position-of (player-of *game*))))
                (position-of (player-of *game*)))
            (iter (for i in (append (list (player-of *game*)) (allies-of *game*)))
                (setf (health-of i) (calculate-stat i :health))
                (setf (energy-of i) (calculate-stat i :energy))
                (let ((exp-gained
                          (/
                              (iter
                                  (for i in (enemies-of *battle*))
                                  (with j = 0)
                                  (incf j (calculate-exp-yield i))
                                  (finally (return j)))
                              2)))
                    (iter
                        (for k in (team-of *game*))
                        (incf (exp-of k) exp-gained)
                        (let ((old-level (level-of k)))
                            (iter (while (>=
                                             (exp-of k)
                                             (calculate-level-to-exp (+ (level-of k) 1))))
                                (incf (level-of k)))
                            (when (> (level-of k) old-level)
                                (format t "~a level-uped to ~d~%" (name-of k) (level-of k))
                                (iter (for i from (1+ old-level) to (level-of k))
                                    (iter (for j in (learned-moves-of k))
                                        (when (= (car j) i)
                                            (unless (get-move (cdr j) k)
                                                (pushnewmove (cdr j) k)
                                                (format t "~a learned ~a~%" (name-of k) (name-of (get-move (cdr j) k))))))))))
                    (setf *battle* nil)))
            (loop for i in (team-of *game*) do
                (wet
                    :force-fill-amount (bladder/maximum-limit-of i))
                (mess
                    :force-fill-amount (bowels/maximum-limit-of i))))
        (progn
            (format t "~a won the battle~%~%" (name-of (player-of *game*)))
            (let ((items-looted
                      (iter
                          (for i in (enemies-of *battle*))
                          (with j = ())
                          (setf j (append j (inventory-of i) (wear-of i)))
                          (finally (return j))))
                     (bitcoins-looted
                         (iter
                             (for i in (enemies-of *battle*))
                             (with j = 0)
                             (incf j (bitcoins-of i))
                             (finally (return j))))
                     (exp-gained
                         (iter
                             (for i in (enemies-of *battle*))
                             (with j = 0)
                             (incf j (calculate-exp-yield i))
                             (finally (return j))))
                     (win-events (win-events-of *battle*)))
                (iter
                    (for k in (team-of *game*))
                    (incf (exp-of k) exp-gained)
                    (let ((old-level (level-of k)))
                        (iter (while (>=
                                         (exp-of k)
                                         (calculate-level-to-exp (+ (level-of k) 1))))
                            (incf (level-of k)))
                        (when (> (level-of k) old-level)
                            (format t "~a level-uped to ~d~%" (name-of k) (level-of k))
                            (iter (for i from (1+ old-level) to (level-of k))
                                (iter (for j in (learned-moves-of k))
                                    (when (= (car j) i)
                                        (unless (get-move (cdr j) k)
                                            (pushnewmove (cdr j) k)
                                            (format t "~a learned ~a~%" (name-of k) (name-of (get-move (cdr j) k))))))))))
                (cond
                    ((and
                         items-looted
                         (> bitcoins-looted 0))
                        (format t
                            "~a loots ~d bitcoins and ~d ~a from the enemies~%"
                            (name-of (player-of *game*))
                            bitcoins-looted
                            (list-length items-looted)
                            (if (= (list-length items-looted) 1)
                                "item"
                                "items")))
                    (items-looted
                        (format t "~a loots ~d ~a from the enemy~%"
                            (name-of (player-of *game*))
                            (list-length items-looted)
                            (if (= (list-length items-looted) 1)
                                "item"
                                "items")))
                    ((> bitcoins-looted 0)
                        (format t "~a loots ~d bitcoins from the enemy~%" (name-of (player-of *game*)) bitcoins-looted)))
                (incf (bitcoins-of (player-of *game*)) bitcoins-looted)
                (setf (inventory-of (player-of *game*)) (append (inventory-of (player-of *game*)) items-looted))
                (setf *battle* nil)
                (setf (continue-battle-of (get-zone (position-of (player-of *game*)))) nil)
                (iter (for i in win-events)
                    (when (or (not (position i (finished-events-of *game*))) (event-repeatable i))
                        (funcall (coerce (event-lambda i) 'function) i)
                        (pushnew i (finished-events-of *game*))
                        (collect i)))
                (when *battle* (return-from finish-battle)))))
    (unuse-package :yadfa/battle :yadfa-user)
    (use-package :yadfa/world :yadfa-user))
(defun wash (clothing)
    (loop for i in (wearingp clothing 'bottoms) do (when (not (disposablep i)) (setf (sogginess-of i) 0 (messiness-of i) 0))))
(defun go-to-sleep% (user)
    (incf (bladder/contents-of user) (* (bladder/fill-rate-of user) 60))
    (incf (bowels/contents-of user) (* (bowels/fill-rate-of user) 60))
    (setf (health-of user) (calculate-stat user :health))
    (setf (energy-of user) (calculate-stat user :energy))
    (cons (wet :wetter user) (mess :messer user)))
(defun go-to-sleep ()
    (iter
        (for i in (append (list (player-of *game*)) (allies-of *game*)))
        (let ((return-value (go-to-sleep% i))
                 (out ()))
            (format t "~a wakes up " (name-of i))
            (when (> (getf (car return-value) :wet-amount) 0)
                (cond
                    ((wearingp (wear-of i) 'tabbed-briefs)
                        (if (> (getf (car return-value) :leak-amount) 0)
                            (progn
                                (push (format nil
                                          "feeling all cold and soggy. ~a checks ~a diaper and to ~a embarrassment finds out it's leaking profusely. Seems ~a wet the bed.~%"
                                          (if (malep i) "He" "She")
                                          (if (malep i) "his" "her")
                                          (if (malep i) "his" "her")
                                          (name-of i))
                                    out)
                                (format t "~a" (nth (strong-random (list-length out)) out))
                                (setf out ()))
                            (progn
                                (push (format nil
                                          "and hears a squish . ~a looks down at ~a diaper, notices that it's soggy and then folds ~a ears back and blushes. Looks like ~a wet the bed~%"
                                          (if (malep i) "He" "She")
                                          (if (malep i) "his" "her")
                                          (if (malep i) "his" "her")
                                          (name-of i))
                                    out)
                                (push (format nil
                                          "and looks down and pokes ~a diaper, then gets all blushy when it squishes. Seems ~a wet the bed~%"
                                          (if (malep i) "his" "her")
                                          (name-of i))
                                    out)
                                (format t "~a" (nth (strong-random (list-length out)) out))
                                (setf out ()))))
                    ((wearingp (wear-of i) 'pullon)
                        (if (> (getf (car return-value) :leak-amount) 0)
                            (progn
                                (push (format nil
                                          "feeling all cold and soggy. ~a checks ~a pullups and to ~a embarrassment finds out it's leaking profusely. Seems ~a wet the bed.~%"
                                          (if (malep i) "He" "She")
                                          (if (malep i) "his" "her")
                                          (if (malep i) "his" "her")
                                          (name-of i))
                                    out)
                                (format t "~a" (nth (strong-random (list-length out)) out))
                                (setf out ()))
                            (progn
                                (push (format nil
                                          "and hears a squish. ~a looks down at ~a pullups, notices that ~a and then folds ~a ears back and blushes. Looks like ~a wet the bed~%"
                                          (if (malep i) "He" "She")
                                          (if (wearingp (wear-of i) 'pullup)
                                              "the little pictures have faded"
                                              "it's soggy")
                                          (if (malep i) "his" "her")
                                          (if (malep i) "his" "her")
                                          (name-of i))
                                    out)
                                (format t "~a" (nth (strong-random (list-length out)) out))
                                (setf out ()))))
                    ((wearingp (wear-of i) 'incontinence-pad)
                        (if (> (getf (car return-value) :leak-amount) 0)
                            (progn
                                (push (format nil
                                          "feeling all cold and soggy. ~a notices ~a PJs, the padding under ~a PJs, and bed are soaked. Seems ~a wet the bed~%"
                                          (if (malep i) "He" "She")
                                          (if (malep i) "his" "her")
                                          (if (malep i) "his" "her")
                                          (name-of i))
                                    out)
                                (format t "~a" (nth (strong-random (list-length out)) out))
                                (setf out ()))
                            (progn
                                (push (format nil
                                          "and hears a squish from under ~a PJs. ~a checks the incontinence pad under them and notices that they're soaked and then folds ~a ears back and blushes. Looks like ~a wet the bed~%"
                                          (if (malep i) "his" "her")
                                          (if (malep i) "He" "She")
                                          (if (malep i) "his" "her")
                                          (name-of i))
                                    out)
                                (format t "~a" (nth (strong-random (list-length out)) out))
                                (setf out ()))))
                    ((wearingp (wear-of i) 'closed-bottoms)
                        (push (format nil
                                  "feeling all cold and soggy. ~a notices ~a PJs and bed are soaked then folds ~a ears back and blushes. Seems ~a wet the bed~%"
                                  (if (malep i) "He" "She")
                                  (if (malep i) "his" "her")
                                  (if (malep i) "his" "her")
                                  (name-of i))
                            out)
                        (format t "~a" (nth (strong-random (list-length out)) out))
                        (setf out ()))
                    (t
                        (push (format nil
                                  "feeling all cold and soggy. ~a notices the bed is soaked then folds ~a ears back and blushes. Seems ~a wet the bed~%"
                                  (if (malep i) "He" "She")
                                  (if (malep i) "his" "her")
                                  (name-of i))
                            out)
                        (format t "~a" (nth (strong-random (list-length out)) out))
                        (setf out ()))))
            (when (and (> (getf (cdr return-value) :mess-amount) 0) (> (getf (car return-value) :wet-amount) 0))
                (format t "~a is also " (name-of i)))
            (when (> (getf (cdr return-value) :mess-amount) 0)
                (cond
                    ((wearingp (wear-of i) 'tabbed-briefs)
                        (if (> (getf (cdr return-value) :leak-amount) 0)
                            (progn
                                (push (format nil
                                          "feeling all mushy. ~a notices to ~a embarrassment that ~a diaper is leaking poo all over the bed. Seems ~a messed the bed~%"
                                          (if (malep i) "He" "She")
                                          (if (malep i) "his" "her")
                                          (if (malep i) "his" "her")
                                          (name-of i))
                                    out)
                                (format t "~a" (nth (strong-random (list-length out)) out))
                                (setf out ()))
                            (progn
                                (push (format nil
                                          "feeling all mushy. ~a notices to ~a embarrassment that ~a diaper is filled with poo. Seems ~a messed the bed~%"
                                          (if (malep i) "He" "She")
                                          (if (malep i) "his" "her")
                                          (if (malep i) "his" "her")
                                          (name-of i))
                                    out)
                                (format t "~a" (nth (strong-random (list-length out)) out))
                                (setf out ()))))
                    ((wearingp (wear-of i) 'pullon)
                        (if (> (getf (cdr return-value) :leak-amount) 0)
                            (progn
                                (push (format nil
                                          "feeling all mushy. ~a notices to ~a embarrassment that ~a pullups is leaking poo all over the bed. Seems ~a messed the bed~%"
                                          (if (malep i) "He" "She")
                                          (if (malep i) "his" "her")
                                          (if (malep i) "his" "her")
                                          (name-of i))
                                    out)
                                (format t "~a" (nth (strong-random (list-length out)) out))
                                (setf out ()))
                            (progn
                                (push (format nil
                                          "feeling all mushy. ~a notices to ~a embarrassment that ~a pullup is filled with poo. Seems ~a messed the bed~%"
                                          (if (malep i) "He" "She")
                                          (if (malep i) "his" "her")
                                          (if (malep i) "his" "her")
                                          (name-of i))
                                    out)
                                (format t "~a" (nth (strong-random (list-length out)) out))
                                (setf out ()))))
                    ((wearingp (wear-of i) 'incontinence-pad)
                        (if (> (getf (cdr return-value) :leak-amount) 0)
                            (progn
                                (push (format nil
                                          "feeling all mushy. ~a notices to ~a embarrassment that ~a incontinence pad is leaking poo all over the bed and PJs. Seems ~a messed the bed~%"
                                          (if (malep i) "He" "She")
                                          (if (malep i) "his" "her")
                                          (if (malep i) "his" "her")
                                          (name-of i))
                                    out)
                                (format t "~a" (nth (strong-random (list-length out)) out))
                                (setf out ()))
                            (progn
                                (push (format nil
                                          "feeling all mushy. ~a notices to ~a embarrassment that ~a incontinence pad is filled with poo. Seems ~a messed the bed~%"
                                          (if (malep i) "He" "She")
                                          (if (malep i) "his" "her")
                                          (if (malep i) "his" "her")
                                          (name-of i))
                                    out)
                                (format t "~a" (nth (strong-random (list-length out)) out))
                                (setf out ()))))
                    ((wearingp (wear-of i) 'closed-bottoms)
                        (push (format nil
                                  "feeling all mushy. ~a notices to ~a embarrassment that ~a PJs have poo in them and is getting on the bed. Seems ~a messed the bed~%"
                                  (if (malep i) "He" "She")
                                  (if (malep i) "his" "her")
                                  (if (malep i) "his" "her")
                                  (name-of i))
                            out)
                        (format t "~a" (nth (strong-random (list-length out)) out))
                        (setf out ()))
                    (t
                        (push (format nil
                                  "feeling all mushy. ~a notices to ~a embarrassment that ~a bed has poo on it. Seems ~a messed the bed~%"
                                  (if (malep i) "He" "She")
                                  (if (malep i) "his" "her")
                                  (if (malep i) "his" "her")
                                  (name-of i))
                            out)
                        (format t "~a" (nth (strong-random (list-length out)) out))
                        (setf out ())))))))
(defun shopfun (items-for-sale &key items-to-buy items-to-sell user format-items)
    (declare
        (type list items-for-sale)
        (type (or list null) items-to-buy items-to-sell)
        (type (or base-character null) user))
    (when items-to-buy
        (iter (for i in items-to-buy)
            (let ((item
                      (when (< (car i) (list-length items-for-sale))
                          (apply #'make-instance
                              (car (nth (car i) items-for-sale))
                              (eval (cdr (nth (car i) items-for-sale)))))))
                (cond
                    ((not item)
                        (format t "item ~d doesn't exist~%" (car i)))
                    ((>
                         (* (value-of item) (cdr i))
                         (bitcoins-of user))
                        (format t "You don't have enough bitcoins to buy ~a~%"
                            (if (= (cdr i) 1)
                                (format nil
                                    "that ~a"
                                    (name-of item))
                                (format nil
                                    "~d ~a"
                                    (cdr i)
                                    (if (plural-name-of item)
                                        (plural-name-of item)
                                        (format nil "~as" (name-of item)))))))
                    (t (loop for j from 1 to (cdr i) do
                           (push
                               (apply #'make-instance
                                   (car (nth (car i) items-for-sale))
                                   (eval (cdr (nth (car i) items-for-sale))))
                               (inventory-of user)))
                        (decf (bitcoins-of user) (* (value-of item) (cdr i)))
                        (format t
                            "You buy ~d ~a for ~f bitcoins~%"
                            (cdr i)
                            (if (plural-name-of item)
                                (plural-name-of item)
                                (format nil "~as" (name-of item)))
                            (* (value-of item) (cdr i))))))))
    (when items-to-sell
        (let ((items (sort (copy-tree items-to-sell) #'>)))
            (if (>= (car items) (list-length (inventory-of user)))
                (format t "item ~d doesn't exist~%" (car items))
                (progn
                    (when (member-if #'(lambda (i) (not (sellablep (nth i (inventory-of user)))))
                              items)
                        (format t "To avoid breaking the game, you can't sell that item~%~%")
                        (return-from shopfun))
                    (loop for i in items do
                        (format t
                            "You sell your ~a for ~f bitcoins~%"
                            (name-of (nth i (inventory-of user)))
                            (/ (value-of (nth i (inventory-of user))) 2))
                        (incf (bitcoins-of user) (/ (value-of (nth i (inventory-of user))) 2))
                        (setf (inventory-of user) (remove-nth i (inventory-of user))))))))
    (when format-items
        (format t "~10a~40a~10@a~%" "Index" "Item" "Price")
        (iter
            (for i in items-for-sale)
            (with j = 0)
            (let ((item (apply #'make-instance (car i) (eval (cdr i)))))
                (format t
                    "~10a~40a~10@a~%"
                    j
                    (name-of item)
                    (value-of item))
                (incf j)))))
(defun getf-action-from-prop (position prop action)
    (getf (actions-of (getf (get-props-from-zone position) prop)) action))
(defun (setf getf-action-from-prop) (new-value position prop action)
    (setf (getf (actions-of (getf (get-props-from-zone position) prop)) action) new-value))
(defun wash-in-washer (washer)
    "washes your dirty diapers and all the clothes you've ruined. WASHER is an instance of a washer you want to put your clothes in."
    (declare (type (or washer null) washer) (ignorable washer))
    (wash (inventory-of (player-of *game*)))
    (format t "You washed all your soggy and messy clothing. Try not to wet and mess them next time~%"))
(defun process-battle (&key attack item user target friendly-target)
    (declare (type (or null integer) user target) (type (or symbol boolean) attack))
    (when (and (not attack) (not item))
        (format t "You need to either specify an attack or an item to use~%")
        (return-from process-battle))
    (let* ((selected-user
               (if user
                   (cond
                       ((>= user (list-length (team-of *game*)))
                           (format t "You only have ~d members in your team~%" (list-length (team-of *game*)))
                           (return-from process-battle))
                       ((<= (health-of (nth user (team-of *game*))) 0)
                           (format t "~a is unconscious and can't fight~%" (name-of (nth user (team-of *game*))))
                           (pushnew (nth user (team-of *game*)) (members-finished-of *battle*))
                           (return-from process-battle))
                       ((position (nth user (team-of *game*)) (members-finished-of *battle*))
                           (format t "~a has already gone~%" (name-of (nth user (team-of *game*))))
                           (return-from process-battle))
                       (t
                           (nth user (team-of *game*))))
                   (iter
                       (for i in (team-of *game*))
                       (when (<= (health-of i) 0)
                           (pushnew i (members-finished-of *battle*)))
                       (unless (position i (members-finished-of *battle*))
                           (leave i)))))
              (selected-target
                  (cond
                      ((and target (>= target (list-length (enemies-of *battle*))))
                          (format t "That target doesn't exist~%")
                          (return-from process-battle))
                      ((and friendly-target (>= friendly-target (list-length (team-of *game*))))
                          (format t "That target doesn't exist~%")
                          (return-from process-battle))
                      (target (nth target (enemies-of *battle*)))
                      (friendly-target (nth friendly-target (team-of *game*)))
                      (t
                          (iter (for i in (enemies-of *battle*))
                              (when (>= (health-of i) 0)
                                  (leave i)))))))
        (unless (or (eq attack t) (get-move attack selected-user))
            (format t "~a doesn't know ~a~%" (name-of selected-user) attack)
            (return-from process-battle))
        (tagbody
            (when (>= (list-length (members-finished-of *battle*)) (list-length (team-of *game*)))
                (go enemies-start-fighting))
            (iter (for i in (copy-tree (getf (status-conditions-of *battle*) selected-user)))
                (if (or (eq (duration-of i) t) (> (duration-of i) 0))
                    (progn
                        (funcall (coerce (battle-script-of i) 'function) (target-of i) selected-user i)
                        (when (typep (duration-of i) 'number)
                            (decf (duration-of i))))
                    (setf (getf (status-conditions-of *battle*) selected-user)
                        (remove i (getf (status-conditions-of *battle*) selected-user)))))
            (run-equip-effects selected-user)
            (when (<= (health-of selected-user) 0)
                (format t "~a has fainted~%~%" (name-of selected-user))
                (pushnew selected-user (members-finished-of *battle*))
                (cond
                    ((not (iter (for i in (team-of *game*)) (when (> (health-of i) 0) (leave t))))
                        (finish-battle t))
                    ((>= (list-length (members-finished-of *battle*)) (list-length (team-of *game*)))
                        (go enemies-start-fighting))
                    (t
                        (return-from process-battle))))
            (incf (bladder/contents-of selected-user) (* (bladder/fill-rate-of selected-user) 5))
            (incf (bowels/contents-of selected-user) (* (bowels/fill-rate-of selected-user) 5))
            (when (and (not (eq (player-of *game*) selected-user))
                      (or (eq (potty-training-of user) :none))
                      (>= (bladder/contents-of selected-user) (bladder/need-to-potty-limit-of selected-user)))
                (let ((wet-status (wet :wetter selected-user)))
                    (format t "~a wet ~aself~%"
                        (name-of selected-user)
                        (if (malep selected-user) "him" "her"))
                    (when (> (getf wet-status :leak-amount) 0))
                    (format t "~a leaks and leaves puddles~%"
                        (name-of selected-user))))
            (when (and (not (eq (player-of *game*) selected-user))
                      (or (eq (potty-training-of user) :none))
                      (>= (bowels/contents-of selected-user) (bowels/need-to-potty-limit-of selected-user)))
                (let ((mess-status (mess :messer selected-user)))
                    (format t "~a messed ~aself~%"
                        (name-of selected-user)
                        (if (malep selected-user) "him" "her"))
                    (when (> (getf mess-status :leak-amount) 0))
                    (format t "~a has a blowout and leaves a mess~%"
                        (name-of selected-user))))
            (cond ((and (not (eq (player-of *game*) selected-user))
                     (or (eq (potty-training-of user) :rebel))
                     (not (typep (get-move attack selected-user)
                              'yadfa/moves:watersport))
                     (>= (bladder/contents-of selected-user) (bladder/need-to-potty-limit-of selected-user)))
                    (let ((a (make-instance 'yadfa/moves:watersport)))
                        (format t "~a: YOU DON'T HAVE ENOUGH BADGES TO TRAIN ME!~%~%" (name-of selected-user))
                        (format t "*~a uses ~a instead*~%~%" (name-of selected-user) (name-of a))
                        (funcall
                            (coerce
                                (attack-of a)
                                'function)
                            selected-target
                            selected-user
                            a)))
                ((and (not (eq (player-of *game*) selected-user))
                     (or (eq (potty-training-of user) :rebel))
                     (not (typep (get-move attack selected-user) 'yadfa/moves:mudsport))
                     (>= (bowels/contents-of selected-user) (bowels/need-to-potty-limit-of selected-user)))
                    (let ((a (make-instance 'yadfa/moves:mudsport)))
                        (format t "~a: YOU DON'T HAVE ENOUGH BADGES TO TRAIN ME!~%~%" (name-of selected-user))
                        (format t "*~a uses ~a instead*~%~%" (name-of selected-user) (name-of a))
                        (funcall
                            (coerce
                                (attack-of a)
                                'function)
                            selected-target
                            selected-user
                            a)))
                ((or
                     (>= (bladder/contents-of selected-user) (bladder/maximum-limit-of selected-user))
                     (>= (bowels/contents-of selected-user) (bowels/maximum-limit-of selected-user)))
                    (when (>= (bladder/contents-of selected-user) (bladder/maximum-limit-of selected-user))
                        (format t
                            "~a lets out a quiet moan as ~a accidentally wets ~aself in battle~%"
                            (name-of selected-user)
                            (if (malep selected-user) "he" "she")
                            (if (malep selected-user) "him" "her"))
                        (wet :wetter selected-user)
                        (set-status-condition 'yadfa/status-conditions:wetting selected-user))
                    (when (>= (bladder/contents-of selected-user) (bladder/maximum-limit-of selected-user))
                        (format t
                            "~a instinctively squats down as ~a accidentally messes ~aself in battle~%"
                            (name-of selected-user)
                            (if (malep selected-user) "he" "she")
                            (if (malep selected-user) "him" "her"))
                        (mess :messer selected-user)
                        (set-status-condition 'yadfa/status-conditions:messing selected-user)))
                ((iter (for j in (getf (status-conditions-of *battle*) selected-user))
                       (when (blocks-turn-of j)
                           (leave t))))
                ((and
                     (or
                         (>= (bladder/contents-of selected-user) (bladder/potty-dance-limit-of selected-user))
                         (>= (bowels/contents-of selected-user) (bowels/potty-dance-limit-of selected-user)))
                     (<
                         (car (sort (let ((a ())
                                             (x
                                                 (-
                                                     (bladder/maximum-limit-of selected-user)
                                                     (bladder/contents-of selected-user)))
                                             (y
                                                 (-
                                                     (bladder/maximum-limit-of selected-user)
                                                     (bladder/potty-dance-limit-of selected-user))))
                                        (when (>=
                                                  (bladder/contents-of selected-user)
                                                  (bladder/potty-dance-limit-of selected-user))
                                            (push (strong-random (expt ($ x / y * (expt 5 1.3)) (/ 1 1.3)))
                                                a))
                                        (setf
                                            x (-
                                                  (bowels/maximum-limit-of selected-user)
                                                  (bowels/contents-of selected-user))
                                            y (- (bowels/maximum-limit-of selected-user)
                                                  (bowels/potty-dance-limit-of selected-user)))
                                        (when (>=
                                                  (bowels/contents-of selected-user)
                                                  (bowels/potty-dance-limit-of selected-user))
                                            (push (strong-random (expt ($ x / y * (expt 5 2)) (/ 1 2)))
                                                a))
                                        a)
                                  '<))
                         1)
                     (not (or
                              (typep (get-move attack selected-user) 'yadfa/moves:watersport)
                              (typep (get-move attack selected-user) 'yadfa/moves:mudsport))))
                    (format t "~a is too busy doing a potty dance to fight~%" (name-of selected-user)))
                (item
                    (format t "~a used ~a ~a on ~a~%"
                        (name-of selected-user)
                        (if (malep selected-user) "his" "her")
                        (name-of (nth item (inventory-of (player-of *game*))))
                        (name-of selected-target))
                    (use-item%
                        (nth item (inventory-of (player-of *game*)))
                        (player-of *game*)
                        :target selected-target)
                    (when (<= (health-of selected-target) 0)
                        (format t "~a has fainted~%" (name-of selected-target))))
                ((eq attack t)
                    (if (wield-of selected-user)
                        (funcall
                            (coerce
                                (attack-of (default-move-of (wield-of selected-user)))
                                'function)
                            selected-target
                            selected-user
                            (default-move-of (wield-of selected-user)))
                        (let ((a (make-instance 'yadfa/moves:default)))
                            (funcall
                                (coerce
                                    (attack-of a)
                                    'function)
                                selected-target
                                selected-user
                                a)))
                    (when (<= (health-of selected-target) 0)
                        (format t "~a has fainted~%" (name-of selected-target))))
                (attack
                    (funcall
                        (coerce
                            (attack-of (get-move attack selected-user))
                            'function)
                        selected-target
                        selected-user
                        (get-move attack selected-user))
                    (when (<= (health-of selected-target) 0)
                        (format t "~a has fainted~%" (name-of selected-target)))))
            enemies-start-fighting
            (unless (iter (for i in (enemies-of *battle*)) (when (> (health-of i) 0) (leave t)))
                (finish-battle)
                (return-from process-battle t))
            (iter
                (for i in (enemies-of *battle*))
                (iter (for j in (copy-tree (getf (status-conditions-of *battle*) i)))
                    (if (or (eq (duration-of j) t) (> (duration-of j) 0))
                        (progn
                            (funcall
                                (coerce (battle-script-of j) 'function)
                                (target-of j)
                                i
                                j)
                            (when (typep (duration-of j) 'number)
                                (decf (duration-of j))))
                        (setf (getf (status-conditions-of *battle*) i)
                            (remove j (getf (status-conditions-of *battle*) i)))))
                (run-equip-effects i)
                (when (<= (health-of i) 0)
                    (next-iteration))
                (incf (bladder/contents-of i) (* (bladder/fill-rate-of i) 5))
                (incf (bowels/contents-of i) (* (bowels/fill-rate-of i) 5))
                (cond ((or
                         (>= (bladder/contents-of i) (bladder/maximum-limit-of i))
                         (>= (bowels/contents-of i) (bowels/maximum-limit-of i)))
                        (when (>= (bladder/contents-of i) (bladder/maximum-limit-of i))
                            (format t
                                "~a lets out a quiet moan as ~a accidentally wets ~aself in battle~%"
                                (name-of i)
                                (if (malep i) "he" "she")
                                (if (malep i) "him" "her"))
                            (wet :wetter i)
                            (set-status-condition 'yadfa/status-conditions:wetting i))
                        (when (>= (bowels/contents-of i) (bowels/maximum-limit-of i))
                            (format t
                                "~a instinctively squats down as ~a accidentally messes ~aself in battle~%"
                                (name-of i)
                                (if (malep i) "he" "she")
                                (if (malep i) "him" "her"))
                            (mess :messer i)
                            (set-status-condition 'yadfa/status-conditions:messing i)))
                    ((and
                         (watersport-limit-of i)
                         (<= (- (bladder/maximum-limit-of i) (bladder/contents-of i)) (watersport-limit-of i))
                         (< (strong-random (watersport-chance-of i)) 1))
                        (let ((a (make-instance 'yadfa/moves:watersport)))
                            (funcall (coerce (attack-of a) 'function) (player-of *game*) i a)))
                    ((and
                         (mudsport-limit-of i)
                         (<= (- (bowels/maximum-limit-of i) (bowels/contents-of i)) (mudsport-limit-of i))
                         (< (strong-random (mudsport-chance-of i)) 1))
                        (let ((a (make-instance 'yadfa/moves:mudsport)))
                            (funcall (coerce (attack-of a) 'function) (player-of *game*) i a)))
                    ((iter (for j in (getf (status-conditions-of *battle*) i))
                           (when (blocks-turn-of j)
                               (leave t))))
                    ((and
                         (or
                             (>= (bladder/contents-of i) (bladder/potty-dance-limit-of i))
                             (>= (bowels/contents-of i) (bowels/potty-dance-limit-of i)))
                         (<
                             (car (sort (let* ((a ())
                                                  (x
                                                      (-
                                                          (bladder/maximum-limit-of i)
                                                          (bladder/contents-of i)))
                                                  (y
                                                      (-
                                                          (bladder/maximum-limit-of i)
                                                          (bladder/potty-dance-limit-of i))))
                                            (when (>=
                                                      (bladder/contents-of i)
                                                      (bladder/potty-dance-limit-of i))
                                                (push (strong-random (expt ($ x / y * (expt 5 1.3)) (/ 1 1.3)))
                                                    a))
                                            (setf
                                                x (-
                                                      (bowels/maximum-limit-of i)
                                                      (bowels/contents-of i))
                                                y (- (bowels/maximum-limit-of i)
                                                      (bowels/potty-dance-limit-of i)))
                                            (when (>=
                                                      (bowels/contents-of i)
                                                      (bowels/potty-dance-limit-of i))
                                                (push (strong-random (expt ($ x / y * (expt 5 2)) (/ 1 2)))
                                                    a))
                                            a)
                                      '<))
                             1))
                        (format t "~a is too busy doing a potty dance to fight~%" (name-of i)))
                    (t
                        (funcall
                            (coerce (battle-script-of i) 'function)
                            i
                            (nth (strong-random (list-length (team-of *game*))) (team-of *game*)))))
                (when (<= (health-of i) 0)
                    (format t "~a has fainted~%" (name-of i))
                    (next-iteration)))
            (unless (iter (for i in (team-of *game*)) (when (> (health-of i) 0) (leave t)))
                (finish-battle t)
                (return-from process-battle t))
            (unless (iter (for i in (enemies-of *battle*)) (when (> (health-of i) 0) (leave t)))
                (finish-battle)
                (return-from process-battle t)))))
(defun ally-join (ally)
    (format t "~a Joins the team~%" (name-of ally))
    (when (> (bitcoins-of ally) 0)
        (format t
            "~a gets ~f bitcoins from ~a~%"
            (name-of (player-of *game*))
            (bitcoins-of ally)
            (name-of ally)))
    (when (inventory-of ally)
        (format t
            "~a gets some loot from ~a~%"
            (name-of (player-of *game*))
            (name-of ally))
        (pushnew ally (allies-of *game*)))
    (incf (bitcoins-of (player-of *game*)) (bitcoins-of ally))
    (setf
        (inventory-of (player-of *game*)) (append (inventory-of (player-of *game*)) (inventory-of ally))
        (inventory-of ally) ()
        (bitcoins-of ally) 0))
(defun use-item% (item user &rest keys &key target action &allow-other-keys)
    (let ((script
              (if action
                  (action-lambda (getf (special-actions-of item) action))
                  (use-script-of item))))
        (if script
            (progn
                (apply (coerce script 'function) item target (when action keys))
                (when (consumablep item)
                    (setf (inventory-of user) (remove item (inventory-of user)))))
            (format t "You can't do that with that item~%"))))