;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa"; coding: utf-8-unix; -*-
;;;; files used internally by the game, don't call these unless you're developing/modding (or cheating)
(in-package :yadfa)
(defmacro defunassert (name-args-declares asserts &body body)
  "Wrapper macro that brings the behavior of SBCL's type declaration to other implementations, NAME-ARGS-DECLARES is the function name, lambda list, and optionally the docstring and declarations (omitting the type declarations) ASSERTS is the type specifiers for the lambda list as a plist, BODY is the body of the function"
  (flet ((get-var (_ var)
           (cond ((eq _ '&key)
                  (cond ((and (typep var 'list)
                              (typep (car var) 'list)
                              (typep (caar var) 'keyword))
                         (cadar var))
                        ((typep var 'list)
                         (car var))
                        (t var)))
                 ((eq _ '&optional)
                  (cond ((typep var 'list)
                         (car var))
                        (t var)))
                 (t var))))
    (macrolet ((check (&optional check-type)
                 (let ((check-type% (gensym)))
                   `(let ((,check-type% ,check-type))
                      (iter
                        (with l = nil)
                        (with m = nil)
                        (for j in (second name-args-declares))
                        (setf l (iter (for k in '(&rest &key &optional &allow-other-keys))
                                  (when (eq j k)
                                    (setf m k)
                                    (leave k))))
                        (unless l
                          (collect (if ,check-type%
                                       `(check-type ,(get-var m j) ,(getf asserts (get-var m j) t))
                                       `(type ,(getf asserts (get-var m j) t) ,(get-var m j))))))))))
      (let* ((sbclp (uiop:featurep :sbcl))
             (declarep (and sbclp
                            (> (length name-args-declares) 2)
                            (typep (car (last name-args-declares)) 'list)
                            (eq (caar (last name-args-declares)) 'declare)))
             (types (check)))
        `(defun
             ,@(if declarep
                   (butlast name-args-declares)
                   name-args-declares)
             ,@(if sbclp
                `(,(append (if declarep
                            (car (last name-args-declares))
                            '(declare))
                    types)
                  ,@body)
                (append
                 (check t)
                 `((locally (declare ,@types)
                     ,@body)))))))))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun set-logical-pathnames ()
    (setf (logical-pathname-translations "YADFA")
          (list (list "yadfa:data;**;*.*.*" (uiop:merge-pathnames*
                                             (make-pathname
                                              :directory '(:relative "yadfa" :wild-inferiors)
                                              :name :wild
                                              :type :wild
                                              :version :wild)
                                             (uiop:xdg-data-home)))
                (list "yadfa:config;**;*.*.*" (uiop:merge-pathnames*
                                               (make-pathname
                                                :directory '(:relative "yadfa" :wild-inferiors)
                                                :name :wild
                                                :type :wild
                                                :version :wild)
                                               (uiop:xdg-config-home)))
                (list "yadfa:home;**;*.*.*" (uiop:merge-pathnames*
                                             (make-pathname
                                              :directory '(:relative :wild-inferiors)
                                              :type :wild
                                              :name :wild
                                              :version :wild)
                                             (if uiop:*image-dumped-p*
                                                 (make-pathname
                                                  :device (pathname-device (truename (uiop:argv0)))
                                                  :directory (pathname-directory (truename (uiop:argv0))))
                                                 (asdf:component-pathname (asdf:find-system "yadfa"))))))))
  (set-logical-pathnames))
(defunassert (get-positions-of-type (type list))
    (type (or null (and symbol (not keyword)) list class)
          list list)
  (iter (for i in list)
    (for j upfrom 0)
    (when (typep i type)
      (collect j))))
(defun finished-events (events)
  (=
   (list-length events)
   (list-length (intersection
                 events
                 (finished-events-of *game*)))))
(defunassert (get-diaper-expansion (item))
    (item bottoms)
  (+
   (* 10
      (/
       (+ (sogginess-of item)
          (messiness-of item))
       (-
        (* 72 36)
        (*
         (/
          (* 72 5/7)
          2)
         (/ 18 2)
         pi))))
   (thickness-of item)))
(defun initialize-mod-registry ()
  (set-logical-pathnames)
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
    (iter (for i in (uiop:directory*
                     #P"yadfa:data;mods;**;*.asd"))
      (when (string= (pathname-type i) "asd")
        (setf (gethash (pathname-name i) *mod-registry*)
              (preferred-mod (gethash (pathname-name i) *mod-registry*)
                             (if (typep i 'logical-pathname)
                                 i
                                 (translate-pathname i
                                                     (uiop:merge-pathnames*
                                                      (make-pathname
                                                       :directory '(:relative "yadfa" "mods" :wild-inferiors)
                                                       :name :wild
                                                       :type :wild)
                                                      (uiop:xdg-data-home))
                                                     "yadfa:data;mods;**;*.*.*"))))))))
(defun clear-pattern-cache ()
  (clrhash *pattern-cache*))
(defun clear-mod-registry ()
  (setf *mod-registry* nil))
(defun find-mod (system)
  (unless *mod-registry* (initialize-mod-registry))
  (gethash (asdf:primary-system-name system) *mod-registry*))
(defun load-mods (&rest keys &key compiler-verbose &allow-other-keys)
  #-yadfa-mods nil
  #+yadfa-mods (unless
                   #+yadfa-docs (find "texi" (uiop:command-line-arguments) :test #'string=)
                   #-yadfa-docs nil
                   (when uiop:*image-dumped-p*
                     (pushnew
                      'yadfa::find-mod
                      asdf:*system-definition-search-functions*)
                     (uiop:register-clear-configuration-hook 'clear-mod-registry)
                     (uiop:register-clear-configuration-hook 'clear-pattern-cache))
                   (asdf:clear-configuration)
                   (set-logical-pathnames)
                   (let* ((file #P"yadfa:config;mods.conf")
                          (mods '()))
                     (ensure-directories-exist #P"yadfa:config;")
                     (handler-case (with-open-file (stream file :if-does-not-exist :error)
                                     (setf mods (read stream)))
                       (file-error ()
                         (write-line "The configuration file containing the list of enabled mods seems missing, creating a new one")
                         (with-open-file (stream file
                                                 :if-does-not-exist :create
                                                 :direction :output
                                                 :if-exists :supersede)
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
  (getf (getf (direction-attributes-of (get-zone position)) direction) attribute (when (eq attribute :locked) :nil)))
(defun remf-direction (position direction attribute)
  (remf (getf (direction-attributes-of (get-zone position)) direction) attribute)
  (unless (getf (direction-attributes-of (get-zone position)) direction)
    (remf (direction-attributes-of (get-zone position)) direction)))
(defun set-status-condition (status-condition user &key duration)
  (let* ((status-conditions (iter (for i in (getf (status-conditions-of *battle*) user))
                              (when (eq (type-of i) status-condition)
                                (collect i))))
         (i (if (or (eq (accumulative-of (make-instance status-condition)) t)
                    (< (length status-conditions) (accumulative-of (make-instance status-condition))))
                (make-instance status-condition)
                (car (sort status-conditions (lambda (a b)
                                               (cond ((eq b t)
                                                      nil)
                                                     ((eq a t)
                                                      t)
                                                     (t (< a b))))
                           :key #'duration-of))))
         (duration (if duration duration (duration-of (make-instance status-condition)))))
    (pushnew i (getf (status-conditions-of *battle*) user))
    (when (and (not (eq (duration-of i) t)) (< (duration-of i) duration))
      (setf (duration-of i) duration))))
(defun pathname<= (pathname1 pathname2)
  (string<= (namestring pathname1) (namestring pathname2)))
;;;(slynk:eval-in-emacs '(with-current-buffer (sly-mrepl--find-buffer) (insert-image (create-image "/tmp/1523307158.liljdude_renamon_boom.jpg"))))
(defun color-format (color &rest body)
  (cl-ansi-text:with-color (color) (apply #'format t body)))
#+(or slynk swank)
(define-condition invalid-emacs-type-for-promp (error)
  ((invalid-type :initarg :invalid-type :reader invalid-type))
  (:report (lambda (condition stream)
             (format stream "Invalid Emacs Type: ~S"
                     (invalid-type condition)))))
;;; error handling of these 2 functions could be better
#+(or slynk swank)
(defun emacs-prompt (options &optional error-message)
  (let* ((eval-in-emacs (cond
                          #+slynk ((find "slynk" (uiop:command-line-arguments) :test #'string=) #'slynk:eval-in-emacs)
                          #+swank ((find "swank" (uiop:command-line-arguments) :test #'string=) #'swank:eval-in-emacs)))
         (wait-for-event (cond
                           #+slynk ((find "slynk" (uiop:command-line-arguments) :test #'string=)
                                    #'slynk::wait-for-event)
                           #+swank ((find "swank" (uiop:command-line-arguments) :test #'string=)
                                    #'swank::wait-for-event))))
    (funcall eval-in-emacs
             `(progn (require 'widget)
                     (eval-when-compile
                      (require 'wid-edit))
                     (defvar yadfa-contents nil)
                     (defun yadfa-widget ()
                       "Widget generated from the CL side as a dialog for my game"
                       (interactive)
                       (switch-to-buffer-other-window "*YADFA*")
                       (kill-all-local-variables)
                       (let ((inhibit-read-only t))
                         (erase-buffer))
                       (remove-overlays)
                       (widget-insert
                        ,(if error-message
                             (format nil "~a~%" error-message)
                             (format nil "Please answer in this \"Window\"~%")))
                       ,@(iter (for j from 0)
                           (for i in options)
                           (cond
                             ((and
                               (typep (first i) 'list)
                               (member (caar i)
                                       '(member clim:completion)
                                       :test #'eq))
                              (collect `(set (make-local-variable ',(intern (format nil "a~d" j)))
                                             (widget-create 'menu-choice
                                                            :tag ,(format nil "~a: " (getf (rest i) :prompt))
                                                            :value ,(if (and
                                                                         (typep (getf (rest i) :default) 'list)
                                                                         (eq (car (getf (rest i) :default)) 'quote)
                                                                         (typep (cdr (getf (rest i) :default)) 'list)
                                                                         (typep (cadr (getf (rest i) :default)) '(and symbol (not keyword))))
                                                                        (list
                                                                         'quote
                                                                         (list :intern
                                                                               (symbol-name (cadr (getf (rest i) :default)))
                                                                               (package-name (symbol-package (cadr (getf (rest i) :default))))))
                                                                        (eval (getf (rest i) :default)))
                                                            ,@(iter (for k in (cond
                                                                                ((eq (caar i) 'member)
                                                                                 (cdar i))
                                                                                ((eq (caar i) 'clim:completion)
                                                                                 (cadar i))))
                                                                (if (typep k '(and symbol (not keyword)))
                                                                    (collect `',(list 'item
                                                                                      :tag (symbol-name k)
                                                                                      (list
                                                                                       :intern
                                                                                       (symbol-name k)
                                                                                       (package-name (symbol-package k)))))
                                                                    (collect `',(list 'item :value k))))))))
                             ((member (first i) '(boolean string integer number)
                                      :test #'eq)
                              (collect `(widget-insert ,(format nil "~a: " (getf (rest i) :prompt))))
                              (collect `(set (make-local-variable ',(intern (format nil "a~d" j)))
                                             (widget-create ',(if (member (first i) '(boolean string))
                                                                  (getf
                                                                   '(boolean checkbox
                                                                     string editable-field)
                                                                   (first i))
                                                                  (first i))
                                                            ,(when (getf (rest i) :default)
                                                               (eval (getf (rest i) :default)))))))
                             (t
                              (error 'invalid-emacs-type-for-promp
                                     :invalid-type i)))
                           (collect `(widget-insert ,#\linefeed)))
                       (widget-create 'push-button
                                      :notify (lambda (top widget &optional reason)
                                                (sly-send (list :emacs-return ,(cond
                                                                                 #+slynk ((find "slynk"
                                                                                                (uiop:command-line-arguments)
                                                                                                :test #'string=)
                                                                                          (slynk::current-thread-id))
                                                                                 #+swank ((find "swank"
                                                                                                (uiop:command-line-arguments)
                                                                                                :test #'string=)
                                                                                          (swank::current-thread-id)))
                                                                :yadfa-response (list ,@(iter (for j from 0) (for i in options)
                                                                                          (declare (ignorable i))
                                                                                          (collect `(widget-value ,(intern (format nil "a~d" j))))))))
                                                (kill-buffer-and-window))
                                      "Apply")
                       (use-local-map widget-keymap)
                       (widget-setup)
                       nil)
                     (yadfa-widget)))
    (iter (for i in (third (funcall wait-for-event '(:emacs-return :yadfa-response result))))
      (collect (if (and (typep i 'list) (eq (car i) :intern))
                   (apply #'intern (cdr i))
                   i)))))
(defmacro prompt-for-values (&rest options)
  `(cond
     #+(or slynk swank)
     ((not clim-listener::*application-frame*)
      (let ((out (emacs-prompt ',options))
            (err nil))
        (iter (while
               (iter (for i in out) (for j in ',options)
                 (unless (cond
                           ((typep (first j) 'list)
                            t)
                           (t (typep i (first j))))
                   (setf err (format nil "~a isn't of type ~a" i (first j)))
                   (leave t))))
          (setf out (emacs-prompt ',options err)))
        out))
     (clim-listener::*application-frame*
      (clim:accepting-values (*query-io* :resynchronize-every-pass t :exit-boxes '((:exit "Accept")))
        (list
         ,@(iter (for i in options)
             (collect `(progn
                         (fresh-line *query-io*)
                         (clim:accept ',(first i) ,@(rest i) :stream *query-io*)))))))))
(defun trigger-event (event-id)
  (when (and
         (not (and (major-event-of *game*) (event-major (get-event event-id))))
         (funcall (coerce (event-predicate (get-event event-id)) 'function)
                  (get-event event-id))
         (or (event-repeatable (get-event event-id)) (not (finished-events (list event-id))))
         (finished-events (event-finished-depends (get-event event-id)))
         (or (not (event-optional (get-event event-id)))
             (car (prompt-for-values (boolean
                                      :prompt "accept quest"
                                      :default t
                                      :view clim:+toggle-button-view+)))))
    (setf (major-event-of *game*) event-id)
    (funcall (coerce (event-lambda (get-event event-id)) 'function) (get-event event-id))
    (unless (event-major (get-event event-id))
      (pushnew event-id (finished-events-of *game*)))
    event-id))
(defun set-new-battle (enemies &rest keys &key win-events enter-battle-text continuable)
  (when continuable
    (setf
     (continue-battle-of (get-zone (position-of (player-of *game*))))
     (list :enemies enemies
           :win-events win-events
           :enter-battle-text enter-battle-text)))
  (setf *battle*
        (apply #'make-instance 'battle
               :enemies (iter (for j in enemies)
                          (collect (apply #'make-instance (car j) (eval (cdr j)))))
               keys))
  (format t "~a~%" (enter-battle-text-of *battle*))
  (iter (for j in (iter (for i in (enemies-of *battle*))
                    (unless (member (class-name (class-of i)) (seen-enemies-of *game*))
                      (format t "~a was added to your pokedex~%" (name-of i))
                      (push (class-name (class-of i)) (seen-enemies-of *game*))
                      (collect (class-name (class-of i))))))
    (yadfa-bin:pokedex j))
  (unuse-package :yadfa-world :yadfa-user)
  (use-package :yadfa-battle :yadfa-user)
  (process-battle :attack t :no-team-attack t))
(defun run-equip-effects (user)
  (iter (for i in (wear-of user))
    (when (wear-script-of i)
      (funcall (coerce (wear-script-of i) 'function) i user)))
  (when (and (wield-of user) (wield-script-of (wield-of user)))
    (funcall (coerce (wield-script-of (wield-of user)) 'function) (wield-of user) user)))
(defunassert (get-warp-point (direction position))
    (direction symbol)
  (getf (warp-points-of (get-zone position))
        (if (typep direction 'keyword)
            (find direction (warp-points-of (get-zone position))
                  :test (lambda (a b)
                          (when (typep b 'symbol)
                            (string= a b))))
            direction)))
(defun get-destination (direction position)
  (macrolet ((a (pos x y z)
               `(let ((b (append (mapcar #'+ (butlast ,pos) '(,x ,y ,z)) (last ,pos))))
                  (when (get-zone b)
                    b))))
    (case direction
      (:north (a position 0 -1 0))
      (:south (a position 0 1 0))
      (:east (a position 1 0 0))
      (:west (a position -1 0 0))
      (:up (a position 0 0 1))
      (:down (a position 0 0 -1))
      (otherwise (get-warp-point direction position)))))
(defun get-path-end (destination &optional position direction)
  (unless (get-zone destination)
    (return-from get-path-end (values nil (format nil "Pick a direction the game knows about~%"))))
  (when (or (hiddenp (get-zone destination)) (and position direction (getf-direction position direction :hidden)))
    (return-from get-path-end (values nil (format nil "Pick a direction the game knows about~%"))))
  (when (or (and (not (eq (lockedp (get-zone destination)) :nil))
                 (not (member-if (lambda (a)
                                   (typep a (lockedp (get-zone destination))))
                                 (append (inventory-of (player-of *game*))
                                         (cons (wield-of (player-of *game*))
                                               (wear-of (player-of *game*)))
                                         (let ((a ()))
                                           (iter (for i in (allies-of *game*))
                                             (push (wield-of i) a)
                                             (iter (for j in (wear-of i))
                                               (push j a)))
                                           a)))))
            (and position direction
                 (not (eq (getf-direction position direction :locked) :nil))
                 (not (member-if (lambda (a)
                                   (typep a (getf-direction position direction :locked)))
                                 (append (inventory-of (player-of *game*))
                                         (cons (wield-of (player-of *game*))
                                               (wear-of (player-of *game*)))
                                         (let ((a ()))
                                           (iter (for i in (allies-of *game*))
                                             (push (wield-of i) a)
                                             (iter (for j in (wear-of i))
                                               (push j a)))
                                           a))))))
    (return-from get-path-end (values nil (format nil "zone ~a is locked~%" destination))))
  destination)
(defun print-map-pattern-cache (path designs)
  (if (gethash (list :map-pattern path designs) *pattern-cache*)
      (gethash (list :map-pattern path designs) *pattern-cache*)
      (setf (gethash (list :map-pattern path designs) *pattern-cache*)
            (clim:make-pattern-from-bitmap-file
             (uiop:merge-pathnames*
              path
              #P"yadfa:home;pixmaps;map-patterns;")
             :format :xpm
             :designs designs))))
(defun print-map (position)
  (labels ((travelablep (position direction)
             (and (get-zone (get-destination direction position))
                  (get-zone position)
                  (not (getf-direction position direction :hidden))
                  (not (hiddenp (get-zone (get-destination direction position))))))
           (a (position)
             (let ((b 0)
                   (array
                     (if clim-listener::*application-frame*
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
                               #P"dot.xpm")
                         "╋╋╋┼┫┫┫┤┣┣┣├┃┃┃│┻┻┻┴┛┛┛┘┗┗┗└╹╹╹╵┳┳┳┬┓┓┓┐┏┏┏┌╻╻╻╷━━━─╸╸╸╴╺╺╺╶▮▮▮▯")))
               (if clim-listener::*application-frame*
                   (progn (unless (travelablep position :north)
                            (setf b (logior b (shl 1 8 3))))
                          (unless (travelablep position :south)
                            (setf b (logior b (shl 1 8 2))))
                          (unless (travelablep position :west)
                            (setf b (logior b (shl 1 8 1))))
                          (unless (travelablep position :east)
                            (setf b (logior b (shl 1 8 0)))))
                   (progn (unless (travelablep position :north)
                            (setf b (logior b (shl 1 8 5))))
                          (unless (travelablep position :south)
                            (setf b (logior b (shl 1 8 4))))
                          (unless (travelablep position :west)
                            (setf b (logior b (shl 1 8 3))))
                          (unless (travelablep position :east)
                            (setf b (logior b (shl 1 8 2))))
                          (unless (travelablep position :up)
                            (setf b (logior b (shl 1 8 1))))
                          (unless (travelablep position :down)
                            (setf b (logior b (shl 1 8 0))))))
               (if clim:*application-frame*
                   (eval (aref array b))
                   (aref array b)))))
    (let ((pattern (print-map-pattern-cache #P"blank.xpm"
                                            (list clim:+background-ink+ clim:+foreground-ink+)))
          (start-position (when clim-listener::*application-frame*
                            (multiple-value-list (clim:stream-cursor-position *standard-output*)))))
      (if clim-listener::*application-frame*
          (push (clim:updating-output (t)
                  (let ((position (if (eq position t)
                                      (position-of (player-of *game*))
                                      position)))
                    (iter (for y
                               from (- (second position) 15)
                               to (+ (second position) 15))
                      (for y-pos
                           from (second start-position)
                           to (+ (second start-position) (* 30 (clim:pattern-height pattern)))
                           by (clim:pattern-height pattern))
                      (iter (for x
                                 from (- (first position) 15)
                                 to (+ (first position) 15))
                        (for x-pos
                             from (first start-position)
                             to (+ (first start-position) (* 30 (clim:pattern-width pattern)))
                             by (clim:pattern-width pattern))
                        (let* ((current-position (append (list x y) (cddr position)))
                               (char (cons (if (or (and (get-zone current-position)
                                                        (hiddenp (get-zone current-position)))
                                                   (not (get-zone current-position)))
                                               #P"blank.xpm"
                                               (a current-position))
                                           (clim:make-rgb-color (if (and (get-zone current-position)
                                                                         (warp-points-of (get-zone current-position)))
                                                                    1
                                                                    0)
                                                                (if (equal current-position (position-of (player-of *game*)))
                                                                    0.7
                                                                    0)
                                                                (if (or (travelablep current-position :up)
                                                                        (travelablep current-position :down))
                                                                    1
                                                                    0)))))
                          (setf pattern (print-map-pattern-cache (car char) (list clim:+background-ink+ (cdr char))))
                          (when (get-zone current-position)
                            (clim:with-output-as-presentation
                                (*standard-output* (get-zone (list x y (third position) (fourth position))) 'zone)
                              (clim:draw-pattern* *standard-output* pattern x-pos y-pos))))))))
                *records*)
          (let ((position (if (eq position t)
                              (position-of (player-of *game*))
                              position)))
            (iter (for y
                       from (- (second position) 15)
                       to (+ (second position) 15))
              (iter (for x
                         from (- (first position) 15)
                         to (+ (first position) 15))
                (let* ((current-position (append (list x y) (cddr position)))
                       (char (cond ((equal current-position (position-of (player-of *game*)))
                                    #\@)
                                   ((and (get-zone current-position) (hiddenp (get-zone current-position)))
                                    #\Space)
                                   ((and (get-zone current-position) (warp-points-of (get-zone current-position)))
                                    #\▒)
                                   ((get-zone current-position)
                                    (a current-position))
                                   (t #\Space))))
                  (format t "~a" char)))
              (terpri))))
      (when clim-listener::*application-frame*
        (clim:stream-set-cursor-position *standard-output* (first start-position) (+ (second start-position) (* 31 (clim:pattern-height pattern))))))))
(defun get-zone-text (text)
  (cond
    ((typep text 'simple-string)
     text)
    ((typep text 'coerced-function)
     (funcall (coerce text 'function)))))
(defun print-enter-text (position &optional old-position old-direction)
  (let ((old-direction (find old-direction (warp-points-of (get-zone old-position))
                             :test (lambda (a b)
                                     (when (typep b 'symbol)
                                       (string= a b))))))
    (format t "~a~%" (get-zone-text (if (and old-position old-direction (getf-direction old-position old-direction :exit-text))
                                        (getf-direction old-position old-direction :exit-text)
                                        (enter-text-of (get-zone position))))))
  (flet ((z (delta direction)
           (let ((current-position (append (mapcar #'+ (butlast position) delta) (last position))))
             (when (and (get-zone current-position)
                        (not (hiddenp (get-zone current-position))))
               (format t "To ~s is ~a. " direction (name-of (get-zone current-position)))))))
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
  (iter (for i in (inventory-of (player-of *game*))) (collect (symbol-name (type-of i)))))
(defmacro defevent (event-id &rest args)
  `(progn
     (setf (gethash ',event-id (events-of *game*)) (make-event :id ',event-id ,@args))
     (export ',event-id ',(symbol-package event-id))
     ',event-id))
(defun get-event (event-id)
  (gethash event-id (events-of *game*)))
(defun (setf get-event) (new-value event-id)
  (setf (gethash event-id (events-of *game*)) new-value))
(defunassert (get-zone (position))
    (position list)
  (gethash position (zones-of *game*)))
(defunassert ((setf get-zone) (new-value position))
    (position list
              new-value zone)
  (setf (position-of new-value) position)
  (setf (gethash position (zones-of *game*)) new-value))
(defunassert (filter-items (items type)
                           "This function will return all items in the list ITEMS that is of type TYPE")
    (items list)
  (iter (for i in items)
    (when (typep i type)
      (collect i))))
(defun swell-up% (user)
  (iter (for i in (filter-items (wear-of user) 'closed-bottoms))
    (if (waterproofp i)
        (finish)
        (progn
          (setf (sogginess-of i) (sogginess-capacity-of i))
          (collect i)))))
(defun swell-up (user)
  (let ((swollen-clothes (swell-up% user)))
    (cond
      ((filter-items swollen-clothes 'tabbed-briefs)
       (format t "~a's diapers swells up humorously~%~%" (name-of user)))
      ((filter-items swollen-clothes 'pullon)
       (format t "~a's pullups swells up humorously~%~%" (name-of user)))
      ((filter-items swollen-clothes 'incontinence-pad)
       (format t "~a's incontinence pad swells up~%~%" (name-of user))))
    (pop-from-expansion user)))
(defun swell-up-all ()
  (swell-up (player-of *game*))
  (iter (for i in (allies-of *game*)) (swell-up i)))
(defunassert (total-thickness (clothing))
    (clothing list)
  (iter (for i in (filter-items clothing 'closed-bottoms))
    (with j = 0)
    (incf j (get-diaper-expansion i))
    (finally (return j))))
(defunassert (pop-from-expansion (user &optional wet/mess))
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
    (let ((last (car (last (wear-of user))))
          (return ()))
      (iter
        (for i in (reverse (wear-of user)))
        (let* ((clothing (when (or (and (eq i last)
                                        (typep i 'bottoms)
                                        (thickness-capacity-of i)
                                        (thickness-capacity-threshold-of i))
                                   (and (not (eq i last))
                                        (typep i 'bottoms)))
                           (member i (wear-of user))))
               (total-thickness (if (and (typep i 'bottoms)
                                         (cadr clothing)
                                         (thickness-capacity-of i)
                                         (thickness-capacity-threshold-of i)
                                         (> (total-thickness (cdr clothing))
                                            (+ (thickness-capacity-of i) (thickness-capacity-threshold-of i))))
                                    (total-thickness (cdr clothing)))))
          (when
              (or (and (eq i last)
                       (typep i 'bottoms)
                       (cadr clothing)
                       (thickness-capacity-of i)
                       (thickness-capacity-threshold-of i)
                       (> total-thickness (+ (thickness-capacity-of i) (thickness-capacity-threshold-of i))))
                  (and (not (eq i last)) (typep i 'bottoms)))
            (cond
              ((and (typep i 'onesie/closed)
                    (cadr clothing)
                    (thickness-capacity-of i)
                    (thickness-capacity-threshold-of i)
                    (> total-thickness (+ (thickness-capacity-of i) (thickness-capacity-threshold-of i))))
               (toggle-onesie%% i)
               (if (lockedp i)
                   (progn (format t "~a's ~a pops open from the expansion destroying the lock in the process~%~%"
                                  (name-of user)
                                  (name-of i))
                          (setf (lockedp i) nil))
                   (format t "~a's ~a pops open from the expansion~%~%"
                           (name-of user)
                           (name-of i)))
               (pushclothing i wet/mess return))
              ((or (typep i 'incontinence-product)
                   (and (typep i 'closed-bottoms)
                        (not (and (thickness-capacity-of i)
                                  (thickness-capacity-threshold-of i)
                                  (> total-thickness (+ (thickness-capacity-of i) (thickness-capacity-threshold-of i)))))))
               (push i (inventory-of (if (typep user 'team-member)
                                         (player-of *game*)
                                         user)))
               (removef (wear-of user) i :count 1)
               (format t "~a's ~a comes off from the expansion~%~%"
                       (name-of user)
                       (name-of i))
               (pushclothing i wet/mess return))
              ((and (typep i '(and bottoms (not incontinence-product)))
                    (thickness-capacity-of i)
                    (thickness-capacity-threshold-of i)
                    (> total-thickness (+ (thickness-capacity-of i) (thickness-capacity-threshold-of i))))
               (removef (wear-of user) i :count 1)
               (format t "~a's ~a tears from the expansion and is destroyed~%~%"
                       (name-of user)
                       (name-of i))
               (pushclothing i wet/mess return))))))
      (cond ((or (getf (car wet/mess) :popped) (getf (cdr wet/mess) :popped))
             (values wet/mess :wet/mess))
            (return (values return :return))
            (t (values nil nil))))))
(defunassert (thickest-sort (clothing))
    (clothing list)
  (sort (iter (for i in clothing)
          (when (typep i 'bottoms)
            (collect i)))
        '>
        :key 'get-diaper-expansion))
(defgeneric toggle-onesie%% (onesie))
(defgeneric toggle-onesie% (onesie underclothes user))
(defmethod toggle-onesie% (onesie underclothes user)
  (write-line "That's not a onesie"))
(defmethod toggle-onesie% ((onesie onesie/opened) underclothes (user base-character))
  (if (and (car (onesie-thickness-capacity-of onesie))
           underclothes
           (> (total-thickness underclothes) (car (onesie-thickness-capacity-of onesie))))
      (format t "~a struggles to snap the bottom of ~a ~a like a toddler who can't dress ~aself but ~a ~a is too thick~%~%"
              (name-of user)
              (if (malep user) "his" "her")
              (name-of onesie)
              (if (malep user) "him" "her")
              (if (malep user) "his" "her")
              (name-of (first (thickest-sort underclothes))))
      (toggle-onesie%% onesie)))
(defmethod toggle-onesie% ((onesie onesie/closed) underclothes (user base-character))
  (if (lockedp onesie)
      (format t "~a can't unsnap ~a ~a as it's locked~%~%"
              (name-of user)
              (if (malep user) "his" "her")
              (name-of onesie))
      (toggle-onesie%% onesie)))
(defmacro defonesie (base-class &body body)
  "macro that generates the classes and methods of the onesie used to open and close the snaps of them. method used to toggle the onesie is TOGGLE-ONESIE. BASE-CLASS is the name of the class you want to give the onesie. BODY is the slot specifier and class options of BASE-CLASS"
  `(progn
     (defclass ,(format-symbol (symbol-package base-class) "~a" (symbol-name base-class))
         (yadfa:onesie) ,@body)
     (defclass ,(format-symbol (symbol-package base-class) "~a/OPENED" (symbol-name base-class))
         (,(format-symbol (symbol-package base-class) "~a" (symbol-name base-class))
          yadfa:onesie/opened) ())
     (defclass ,(format-symbol (symbol-package base-class) "~a/CLOSED" (symbol-name base-class))
         (,(format-symbol (symbol-package base-class) "~a" (symbol-name base-class))
          yadfa:onesie/closed) ())
     (export ',(format-symbol (symbol-package base-class) "~a" (symbol-name base-class)))
     (export ',(format-symbol (symbol-package base-class) "~a/OPENED" (symbol-name base-class)))
     (export ',(format-symbol (symbol-package base-class) (format nil "~a/CLOSED" (symbol-name base-class))))
     (defmethod toggle-onesie%% ((self ,(format-symbol (symbol-package base-class) "~a/OPENED" (symbol-name base-class))))
       (change-class self ',(format-symbol (symbol-package base-class) "~a/CLOSED" (symbol-name base-class))))
     (defmethod toggle-onesie%% ((self ,(format-symbol (symbol-package base-class) "~a/CLOSED" (symbol-name base-class))))
       (change-class self ',(format-symbol (symbol-package base-class) "~a/OPENED" (symbol-name base-class))))))
(defmacro ensure-zone (position &body body)
  "defines the classes of the zones and adds an instance of them to the game's map hash table if it's not already there"
  #+sbcl (declare (type list position))
  (check-type position list)
  `(progn (unless (get-zone ',position)
            (setf (get-zone ',position)
                  (make-instance 'zone ,@body)))
          (export ',(fourth position) ',(symbol-package (fourth position)))
          (get-zone ',position)))
(defmacro defzone (position &body body)
  "defines the classes of the zones and adds an instance of them to the game's map hash table. Intended to be used to replace existing zones in more intrusive mods. Best to wrap this in an event and run trigger-event so it doesn't overwrite the zone every time this piece of code is loaded"
  #+sbcl (declare (type list position))
  (check-type position list)
  `(progn
     (setf (get-zone ',position)
           (make-instance 'zone ,@body))
     (export ',(fourth position) ',(symbol-package (fourth position)))
     (get-zone ',position)))
(defmacro make-pocket-zone (position &body body)
  "defines the classes of the zones and adds an instance of them to the game's map hash table if it's not already there"
  #+sbcl (declare (type list position))
  (check-type position list)
  `(setf (get-zone '(,@position :pocket-map))
         (make-instance 'zone ,@body)))
(defun move-to-secret-underground ()
  (when *battle*
    (write-line "To avoid breaking the game due to a few assumtpions made in this function, please don't run this in a battle~%")
    (return-from move-to-secret-underground))
  (unless (get-path-end '(0 0 0 yadfa-zones:secret-underground))
    (format t "~a" (second
                    (multiple-value-list
                     (get-path-end '(0 0 0 yadfa-zones:secret-underground)))))
    (return-from move-to-secret-underground))
  (when
      (iter (for i in (cons (player-of *game*) (allies-of *game*)))
        (when (or (and (< (list-length (filter-items (wear-of i)
                                                     (car (must-wear-of (get-zone '(0 0 0 yadfa-zones:secret-underground))))))
                          1)
                       (not (funcall (coerce (cdr (must-wear-of (get-zone '(0 0 0 yadfa-zones:secret-underground)))) 'function)
                                     i)))
                  (and
                   (> (list-length (filter-items (wear-of i)
                                                 (car (must-not-wear-of (get-zone '(0 0 0 yadfa-zones:secret-underground))))))
                      0)
                   (not (funcall (coerce (cdr (must-not-wear-of (get-zone '(0 0 0 yadfa-zones:secret-underground)))) 'function)
                                 i))))
          (leave t)))
    (return-from move-to-secret-underground))
  (incf (time-of *game*))
  (unless (eq (lockedp (get-zone '(0 0 0 yadfa-zones:secret-underground))) :nil)
    (format t "You unlock zone ~a~%" '(0 0 0 yadfa-zones:secret-underground))
    (setf (lockedp (get-zone '(0 0 0 yadfa-zones:secret-underground))) :nil))
  (setf (position-of (player-of *game*)) '(0 0 0 yadfa-zones:secret-underground))
  (when (underwaterp (get-zone (position-of (player-of *game*)))) (swell-up-all))
  (process-potty)
  (run-equip-effects (player-of *game*))
  (iter (for i in (allies-of *game*))
    (process-potty i)
    (run-equip-effects i))
  (print-enter-text (position-of (player-of *game*)))
  (cond ((continue-battle-of (get-zone (position-of (player-of *game*))))
         (set-new-battle (getf (continue-battle-of (get-zone (position-of (player-of *game*)))) :enemies)
                         :win-events (getf (continue-battle-of (get-zone (position-of (player-of *game*)))) :win-events)
                         :continuable t
                         :enter-battle-text (getf (continue-battle-of (get-zone (position-of (player-of *game*)))) :enter-battle-text))
         (return-from move-to-secret-underground))
        ((iter (for i in (events-of (get-zone (position-of (player-of *game*)))))
           (when (trigger-event i)
             (collect i)))
         (return-from move-to-secret-underground))
        ((enemy-spawn-list-of (get-zone (position-of (player-of *game*))))
         (iter (for i in (enemy-spawn-list-of (get-zone (position-of (player-of *game*)))))
           (let ((random (if (getf i :random) (getf i :random) 1)))
             (when (< (random (getf i :max-random)) random)
               (set-new-battle (cond ((getf i :eval)
                                      (eval (getf i :eval)))
                                     ((getf i :lambda)
                                      (funcall (coerce (getf i :lambda) 'function)))
                                     (t (getf i :enemies))))
               (return-from move-to-secret-underground)))))))
(defun move-to-pocket-map (item)
  (when *battle*
    (write-line "To avoid breaking the game due to a few assumtpions made in this function, please don't run this in a battle~%")
    (return-from move-to-pocket-map))
  (unless (get-zone '(0 0 0 pocket-map))
    (make-pocket-zone (0 0 0)
      :name "Pocket Map Entrace"
      :description "Welcome to the Pocket Map. It's like the secret bases in Pokemon, except you customize it by scripting, and you can take it with you."
      :enter-text "You're at the start of the Pocket Map. Use the Pocket Map machine again at anytime to exit."))
  (let ((new-position
          (if (eq (fourth (position-of (player-of *game*))) :pocket-map)
              (getf (attributes-of item) :pocket-map-position)
              '(0 0 0 :pocket-map))))
    (unless (get-path-end new-position)
      (format t "~a"
              (second (multiple-value-list (get-path-end '(0 0 0 pocket-map)))))
      (return-from move-to-pocket-map))
    (when
        (iter (for i in (cons (player-of *game*) (allies-of *game*)))
          (when (or (and (< (list-length (filter-items (wear-of i) (car (must-wear-of (get-zone new-position)))))
                            1)
                         (not (funcall (coerce (cdr (must-wear-of (get-zone new-position)))
                                               'function)
                                       i)))
                    (and (> (list-length (filter-items (wear-of i) (car (must-not-wear-of (get-zone new-position)))))
                            0)
                         (not (funcall (coerce (cdr (must-not-wear-of (get-zone new-position))) 'function)
                                       i))))
            (leave t)))
      (return-from move-to-pocket-map))
    (incf (time-of *game*))
    (unless (eq (fourth (position-of (player-of *game*))) :pocket-map)
      (setf (getf (attributes-of item) :pocket-map-position) (position-of (player-of *game*))))
    (unless (eq (lockedp (get-zone new-position)) :nil)
      (format t "You unlock zone ~a~%" new-position)
      (setf (lockedp (get-zone new-position)) :nil))
    (setf (position-of (player-of *game*)) new-position)
    (when (underwaterp (get-zone (position-of (player-of *game*))))
      (swell-up-all))
    (process-potty)
    (run-equip-effects (player-of *game*))
    (iter (for i in (allies-of *game*))
      (process-potty i)
      (run-equip-effects i))
    (print-enter-text (position-of (player-of *game*)))
    (cond ((continue-battle-of (get-zone (position-of (player-of *game*))))
           (set-new-battle (getf (continue-battle-of (get-zone (position-of (player-of *game*)))) :enemies)
                           :win-events (getf (continue-battle-of (get-zone (position-of (player-of *game*)))) :win-events)
                           :continuable t
                           :enter-battle-text (getf (continue-battle-of (get-zone (position-of (player-of *game*)))) :enter-battle-text))
           (return-from move-to-pocket-map))
          ((iter (for i in (events-of (get-zone (position-of (player-of *game*)))))
             (when (trigger-event i)
               (collect i)))
           (return-from move-to-pocket-map))
          ((enemy-spawn-list-of (get-zone (position-of (player-of *game*))))
           (iter (for i in (enemy-spawn-list-of (get-zone (position-of (player-of *game*)))))
             (let ((random (if (getf i :random) (getf i :random) 1)))
               (when (< (random (getf i :max-random)) random)
                 (set-new-battle (cond ((getf i :eval)
                                        (eval (getf i :eval)))
                                       ((getf i :lambda)
                                        (funcall (coerce (getf i :lambda) 'function)))
                                       (t (getf i :enemies))))
                 (return-from move-to-pocket-map))))))))
(defunassert (wet (&key (wet-amount t) force-fill-amount pants-down accident force-wet-amount (wetter (player-of *game*)))
                  "this function is mostly for mods, doesn't print text or diaper expansion, that's handled by other functions. WETTER is the instance of BASE-CHARACTER doing the flooding. WET-AMOUNT is the amount WETTER floods but won't flood if he/she can't go, passing T to WET-AMOUNT means to use (BLADDER/CONTENTS-OF WETTER), FORCE-WET-AMOUNT causes WETTER to wet regardless. FORCE-FILL-AMOUNT will set (BLADDER/CONTENTS-OF WETTER) to that amount first. PANTS-DOWN is T if WETTER pulls his/her pants down first. ACCIDENT is T if the wetting isn't intentional and WETTER may or may not be able to stop the flow")
    (force-fill-amount (or null number)
                       force-wet-amount (or null number)
                       wet-amount (or boolean number)
                       wetter base-character)
  (let* ((return-value ())
         (affected-clothes ())
         (random (random 4))
         (amount nil))
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
           (return-from wet (list :old-bladder-contents (bladder/contents-of wetter)
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
          (t (setf amount (cond ((eq wet-amount t)
                                 (bladder/contents-of wetter))
                                ((> wet-amount (bladder/contents-of wetter))
                                 (bladder/contents-of wetter))
                                (t
                                 wet-amount)))))
    (setf (getf return-value :old-bladder-contents) (bladder/contents-of wetter))
    (let* ((amount-left amount))
      (cond ((or pants-down (not (filter-items (wear-of wetter) 'closed-bottoms)))
             (decf (bladder/contents-of wetter) amount)
             (setf amount-left 0))
            (t
             (decf (bladder/contents-of wetter) amount)
             (iter (while (> amount-left 0))
               (for i in (reverse (wear-of wetter)))
               (when (typep i 'closed-bottoms)
                 (cond ((> amount-left (- (sogginess-capacity-of i) (sogginess-of i)))
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

(defunassert (mess (&key (mess-amount t) force-fill-amount pants-down accident force-mess-amount (messer (player-of *game*)))
                   "this function is mostly for mods, doesn't print text or diaper expansion, that's handled by other functions. MESSER is the instance of BASE-CHARACTER doing the messing. MESS-AMOUNT is the amount MESSER messes but won't mess if he/she can't go, passing T to MESS-AMOUNT means to use (BOWELS/CONTENTS-OF MESSER), FORCE-MESS-AMOUNT causes MESSER to mess regardless. FORCE-FILL-AMOUNT will set (BOWELS/CONTENTS-OF MESSER) to that amount first. PANTS-DOWN is T if MESSER pulls his/her pants down first. ACCIDENT is T if the messing isn't intentional")
    (force-fill-amount (or null number)
                       force-mess-amount (or null number)
                       mess-amount (or boolean number)
                       messer base-character)
  (let* ((return-value ())
         (affected-clothes ())
         (amount nil))
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
           (return-from mess (list :old-bowels-contents (bowels/contents-of messer)
                                   :new-bowels-contents (bowels/contents-of messer)
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
      (cond ((or pants-down (not (filter-items (wear-of messer) 'closed-bottoms)))
             (decf (bowels/contents-of messer) amount)
             (setf amount-left 0))
            (t
             (decf (bowels/contents-of messer) amount)
             (iter (while (> amount-left 0))
               (for i in (reverse (wear-of messer)))
               (when (typep i 'closed-bottoms)
                 (cond ((> amount-left (- (messiness-capacity-of i) (messiness-of i)))
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
    (iter (for i in (filter-items (wear-of user) 'bottoms))
      (setf (lockedp i) nil)
      (when (typep i 'onesie/closed)
        (toggle-onesie%% i)))
    (setf (inventory-of (player-of *game*)) (append (inventory-of (player-of *game*)) (filter-items (wear-of user) 'closed-bottoms))
          (wear-of user) (remove-if (lambda (a)
                                      (typep a 'closed-bottoms))
                                    (wear-of user)))
    (if (wear-of user)
        (push b (cdr (last (wear-of user))))
        (push b (wear-of user)))
    (iter (with i = 0)
      (while (< (1+ i) (list-length (wear-of user))))
      (if (or (and (typep (nth i (wear-of user)) 'bottoms)
                   (thickness-capacity-of (nth i (wear-of user)))
                   (nthcdr (1+ i) (wear-of user))
                   (> (total-thickness (nthcdr (1+ i) (wear-of user)))
                      (thickness-capacity-of (nth i (wear-of user)))))
              (and (typep (nth i (wear-of user)) 'bottoms)
                   (or (>= (sogginess-of (nth i (wear-of user))) (/ (sogginess-capacity-of (nth i (wear-of user))) 4))
                       (>= (messiness-of (nth i (wear-of user))) (/ (messiness-capacity-of (nth i (wear-of user))) 4)))))
          (progn
            (push (nth i (wear-of user)) (inventory-of (player-of *game*)))
            (setf (wear-of user) (remove-nth i (wear-of user))))
          (incf i)))))
(defunassert (potty-on-toilet (prop &key wet mess pants-down (user (player-of *game*))))
    (prop toilet
          wet (or boolean number)
          mess (or boolean number))
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
          (push (format nil "Looks like you missed a step ~a" (random-elt names)) out)
          (push (format nil "Aww, looks like the little ~a forgot to take ~a ~a first"
                        (let ((a names))
                          (push (format nil "baby ~a" (if (malep user) "boy" "girl")) a)
                          (random-elt a))
                        (if (malep user) "his" "her")
                        (cond ((filter-items (wear-of user) 'tabbed-briefs)
                               "diapers")
                              ((filter-items (wear-of user) 'pullon)
                               "pullups")
                              (t "panties")))
                out)
          (format t "~a~%" (random-elt out))))))
(defunassert (potty-on-self-or-prop (prop &key wet mess pants-down (user (player-of *game*))))
    (wet (or boolean number)
         mess (or boolean number))
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
       (clothes (cond ((filter-items (wear-of user) 'tabbed-briefs)
                       (list "diapers" "pamps" "huggies" "pampers" "padding"))
                      ((filter-items (wear-of user) 'pullon)
                       (list "pullups" "padding"))
                      ((not (filter-items (wear-of user) 'closed-pants))
                       (list "undies" "panties"))
                      (t (list "pants")))))
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
                      (format t "~a~%" (random-elt both-list)))
                     ((and mess-return-value (> (getf mess-return-value :mess-amount) 0) mess-list)
                      (format t "~a~%" (random-elt mess-list)))
                     ((and wet-return-value (> (getf wet-return-value :wet-amount) 0) wet-list)
                      (format t "~a~%" (random-elt wet-list))))
               (setf wet-list () mess-list () both-list()))
             (format-leak-lists ()
               (cond ((and
                       wet-return-value
                       mess-return-value
                       (> (getf wet-return-value :leak-amount) 0)
                       (> (getf mess-return-value :leak-amount) 0)
                       both-leak-list)
                      (format t "~a~%" (random-elt both-leak-list)))
                     ((and mess-return-value (> (getf mess-return-value :leak-amount) 0) mess-leak-list)
                      (format t "~a~%" (random-elt mess-leak-list)))
                     ((and wet-return-value (> (getf wet-return-value :leak-amount) 0) wet-leak-list)
                      (format t "~a~%" (random-elt wet-leak-list))))
               (setf wet-leak-list ()
                     mess-leak-list ()
                     both-leak-list())))
        (cond
          ;; player pulls his pants down then potty
          ((and pants-down (filter-items (wear-of user) 'closed-bottoms))
           (do-push (format nil "~a pulled down ~a ~a and went potty on the ~a"
                            (name-of user)
                            (if (malep user) "his" "her")
                            (random-elt clothes)
                            (if prop
                                (name-of prop)
                                "floor"))
             both-list wet-list mess-list)
           (do-push (format nil "~a pulls down ~a ~a and marks ~a territory"
                            (name-of user)
                            (if (malep user) "his" "her")
                            (random-elt clothes)
                            (if (malep user) "his" "her"))
             both-list wet-list mess-list)
           (push (format nil "~a pulled down ~a ~a and peed on the ~a"
                         (name-of user)
                         (if (malep user) "his" "her")
                         (random-elt clothes)
                         (if prop
                             (name-of prop)
                             "floor"))
                 wet-list)
           (push (format nil "~a pulled down ~a ~a and squats down and mess"
                         (name-of user)
                         (if (malep user) "his" "her")
                         (random-elt clothes))
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
                         (name-of user)
                         (if prop
                             (name-of prop)
                             "floor"))
                 both-list)
           (push (format nil "~a pees on the ~a like an animal"
                         (name-of user)
                         (if prop
                             (name-of prop)
                             "floor"))
                 wet-list)
           (push (format nil "~a squats down and messes on the ~a like an animal"
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
                               (name-of user)
                               (if (malep user) "his" "her")
                               (name-of prop))
                       wet-list)
                 (push (format nil "~a squats down on all fours and mess like an animal"
                               (name-of user))
                       mess-list)
                 (push (format nil "~a lifts ~a leg and pees on the ~a, then squats down on all fours and mess"
                               (name-of user)
                               (if (malep user) "his" "her")
                               (name-of prop))
                       both-list)
                 (do-push (format nil "Bad ~a! No going potty on the ~a!"
                                  (species-of user)
                                  (name-of prop))
                   wet-list mess-list both-list))
               (do-push (format nil "~a realized ~a made a horrible mistake. ~a weren't wearing any padding!!!"
                                (name-of user)
                                (if (malep user) "he" "she")
                                (if (malep user) "He" "She"))
                 both-list wet-list mess-list))
           (format-lists))
          ;; player is using his pants like a toilet
          (t
           (cond ((and (not prop) wet-return-value (< (getf wet-return-value :wet-amount) 30))
                  (push (format nil "~a lets a little out to relieve the pressure"
                                (name-of user))
                        wet-list)
                  (push (format nil "Bad idea as ~a just made a puddle on the floor"
                                (if (malep user) "he" "she"))
                        wet-leak-list)
                  (format-lists)
                  (format-leak-lists))
                 ((filter-items (wear-of user) 'tabbed-briefs)
                  (when prop
                    (push (format nil "~a lifts ~a leg near the ~a and floods ~a pamps"
                                  (name-of user)
                                  (if (malep user) "his" "her")
                                  (name-of prop)
                                  (if (malep user) "his" "her"))
                          wet-list)
                    (push (format nil "~a lifts ~a leg near the ~a and floods ~a pamps. Looks like the little ~a isn't house-trained"
                                  (name-of user)
                                  (if (malep user) "his" "her")
                                  (name-of prop)
                                  (if (malep user) "his" "her")
                                  (species-of user))
                          wet-list)
                    (push (format nil "You lift your leg near the ~a and flood your pamps, then squat down on all fours and mess"
                                  (name-of prop))
                          both-list)
                    (push (format nil "~a squats down on all fours with ~a tail raised like an animal and messes ~a diapers"
                                  (name-of user)
                                  (if (malep user) "his" "her")
                                  (if (malep user) "his" "her"))
                          mess-list))
                  (do-push (format nil "~a goes potty in ~a diapers like a toddler"
                                   (name-of user)
                                   (if (malep user) "his" "her"))
                    wet-list mess-list both-list)
                  (when (>= (getf wet-return-value :wet-amount) (bladder/potty-desperate-limit-of user))
                    (do-push (format nil "after doing a potty dance like a 5 year old, ~a floods ~a diapers with a huge sigh of relief"
                                     (name-of user)
                                     (if (malep user) "his" "her"))
                      wet-list))
                  (when (filter-items (wear-of user) 'diaper)
                    (do-push (format nil "Aww, is the baby using ~a diapers?"
                                     (if (malep user) "his" "her"))
                      wet-list mess-list both-list))
                  (push (format nil "~a pauses and floods ~a diapers"
                                (name-of user)
                                (if (malep user) "his" "her"))
                        wet-list)
                  (push (apply #'format nil "~a squats down with ~a tail raised and fills ~a diapers"
                               (name-of user)
                               (if (malep user)
                                   '("his" "his")
                                   '("her" "her")))
                        mess-list)
                  (push (format nil "heh, the baby blorted ~a diapers" (if (malep user) "his" "her")) mess-list)
                  (push (format nil "~a diapers sprung a leak" (name-of user)) wet-leak-list)
                  (do-push (format nil
                                   "~a's diapers leak all over, there goes the carpet" (name-of user))
                    wet-leak-list mess-leak-list both-leak-list)
                  (push (format nil "Blowout!!!") mess-leak-list)
                  (push (format nil "Heh, baby made a puddle") wet-leak-list)
                  (push (format nil "~a piddles ~a pamps" (name-of user) (if (malep user) "his" "her")) wet-list))
                 ((filter-items (wear-of user) 'pullon)
                  (when prop
                    (push (format nil "~a lifts ~a leg near the ~a and floods ~a pullups"
                                  (name-of user)
                                  (if (malep user) "his" "her")
                                  (name-of prop)
                                  (if (malep user) "his" "her"))
                          wet-list)
                    (push (format nil "~a lifts ~a leg near the ~a and floods ~a pullups, then squats down on all fours and messes"
                                  (name-of user)
                                  (if (malep user) "his" "her")
                                  (name-of prop)
                                  (if (malep user) "his" "her"))
                          both-list)
                    (push (format nil "~a squats down on all fours with ~a tail raised like an animal and messes ~a pullups"
                                  (name-of user)
                                  (if (malep user) "his" "her")
                                  (if (malep user) "his" "her"))
                          mess-list))
                  (do-push (format nil
                                   "~a's pullups leak all over, there goes the carpet" (name-of user))
                    wet-leak-list mess-leak-list both-leak-list)
                  (when (filter-items (wear-of user) 'pullup)
                    (do-push
                        (format nil "Bad ~a! You know you're supposed to use the toilet like a big kid"
                                (if (malep user) "boy" "girl")
                                )
                      wet-list mess-list both-list)
                    (when (>= (getf wet-return-value :wet-amount) (bladder/potty-desperate-limit-of user))
                      (do-push (format nil "after doing a potty dance like a 5 year old, ~a floods ~a pullups with a huge sigh of relief and just hopes no one will notice"
                                       (name-of user)
                                       (if (malep user) "his" "her"))
                        wet-list))
                    (do-push (format nil "Bad ~a! Look at the mess you made leaking everywhere like that! Do we have to put you back in diapers?!"
                                     (if (malep user) "boy" "girl"))
                      wet-leak-list mess-leak-list both-leak-list))
                  (push (format nil "~a squats down and messes ~a pullups like a toddler"
                                (name-of user)
                                (if (malep user) "his" "her"))
                        mess-list)
                  (do-push (format nil "~a goes potty in ~a pullups like a toddler"
                                   (name-of user)
                                   (if (malep user) "his" "her"))
                    mess-list wet-list both-list)
                  (push (format nil "~a pauses and floods ~a pullups"
                                (name-of user)
                                (if (malep user) "his" "her"))
                        wet-list)
                  (push (format nil "~a floods ~a pullups like a toddler"
                                (name-of user)
                                (if (malep user) "his" "her"))
                        wet-list)
                  (push (format nil "~a mess falls out of ~a pullups and on the floor"
                                (name-of user)
                                (if (malep user) "his" "her"))
                        mess-leak-list)
                  (push (format nil "~a's pullups sprung a leak"
                                (name-of user))
                        wet-leak-list)
                  (push (format nil "~a makes a puddle"
                                (name-of user))
                        wet-leak-list)
                  (when (eq user (player-of *game*))
                    (push (format nil "You made a puddle on the floor. You sure you're ready for pullups?")
                          wet-leak-list)))
                 ((filter-items (wear-of user) 'incontinence-pad)
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
                  (do-push (format nil "Heh, baby made a mess on the floor")
                    wet-leak-list mess-leak-list both-leak-list))
                 (t
                  (do-push (format nil "~a realized ~a made a horrible mistake. ~a's not wearing any padding!!!"
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
                    (cons wet-return-value mess-return-value) user)))))))
;;; Wish the API I made for this wasn't so complex, but I wasn't sure how to make it simple and still retain the functionality
(defgeneric get-babyish-padding (user))
(defgeneric get-process-potty-action-type (user type had-accident))
(defgeneric output-process-potty-text (user padding type action had-accident))
(defmethod get-babyish-padding ((user team-member))
  (macro-level `(cond ,@(iter (for i in '(tabbed-briefs pullon closed-bottoms))
                          (collect `((filter-items (wear-of user) ',i)
                                     ',i)))
                      (t nil))))
(defmethod get-process-potty-action-type ((user ally-last-minute-potty-training) (type (eql :wet)) had-accident)
  (cond ((and
          (car had-accident)
          (> (getf (car had-accident) :wet-amount) 0))
         :had-accident)
        ((>=
          (bladder/contents-of user)
          (bladder/potty-desperate-limit-of user))
         :desparate)
        ((>= (bladder/contents-of user) (bladder/potty-dance-limit-of user))
         :potty-dance)))
(defmethod get-process-potty-action-type ((user ally-last-minute-potty-training) (type (eql :mess)) had-accident)
  (cond ((and
          (cdr had-accident)
          (> (getf (cdr had-accident) :mess-amount) 0))
         :had-accident)
        ((>=
          (bowels/contents-of user)
          (bowels/potty-desperate-limit-of user))
         :desparate)
        ((>= (bowels/contents-of user) (bowels/potty-dance-limit-of user))
         :potty-dance)))
(defmethod get-process-potty-action-type ((user ally) (type (eql :wet)) had-accident)
  (when (and (car had-accident) (> (getf (car had-accident) :wet-amount) 0))
    :had-accident))
(defmethod get-process-potty-action-type ((user ally) (type (eql :mess)) had-accident)
  (when (and (cdr had-accident) (> (getf (cdr had-accident) :mess-amount) 0))
    :had-accident))
(defmethod get-process-potty-action-type ((user player) (type (eql :wet)) had-accident)
  (cond ((and (car had-accident) (> (getf (car had-accident) :wet-amount) 0))
         :had-accident)
        ((>= (bladder/contents-of user) (bladder/potty-dance-limit-of user))
         :potty-dance)
        ((>= (bladder/contents-of user) (bladder/need-to-potty-limit-of user))
         :need-to-potty)))
(defmethod get-process-potty-action-type ((user player) (type (eql :mess)) had-accident)
  (cond ((and (cdr had-accident) (> (getf (cdr had-accident) :mess-amount) 0))
         :had-accident)
        ((>= (bowels/contents-of user) (bowels/potty-dance-limit-of user))
         :potty-dance)
        ((>= (bowels/contents-of user) (bowels/need-to-potty-limit-of user))
         :need-to-potty)))
(defmethod output-process-potty-text (user padding type (action (eql nil)) had-accident)
  nil)
(defmethod output-process-potty-text ((user player) padding (type (eql :wet)) (action (eql :potty-dance)) had-accident)
  (format t "~a~%"
          (random-elt (list "You feel like your bladder is going to explode"
                            "You're doing a potty dance like a 5 year old"
                            "You feel like you're going to wet yourself"
                            "You whine as you hold yourself in desperation"
                            "Aww, does the baby need to potty?"))))
(defmethod output-process-potty-text ((user player) padding (type (eql :wet)) (action (eql :desparate)) had-accident)
  (format t "~a~%"
          (random-elt (list "You feel like your bladder is going to explode"
                            "You're doing a potty dance like a 5 year old"
                            "You feel like you're going to wet yourself"
                            "You whine as you hold yourself in desperation"
                            "Aww, does the baby need to potty?"))))
(defmethod output-process-potty-text ((user player) padding (type (eql :wet)) (action (eql :need-to-potty)) had-accident)
  (format t "You need to pee~%"))
(defmethod output-process-potty-text ((user player) (padding (eql 'tabbed-briefs)) (type (eql :wet)) (action (eql :had-accident)) had-accident)
  (format t "~a~%"
          (let ((j (cond ((<= (getf (car had-accident) :wet-amount) 10)
                          (list "You ghasp in horror as a little leaks out"
                                "You think you just leaked a little"
                                (format nil "A little squirts out. You quickly grab yourself with a ~a, but manage to stop the flood"
                                        (random-elt '("groan" "whine")))))
                         ((and (<= (getf (car had-accident) :wet-amount) 300) (> (getf (car had-accident) :wet-amount) 10))
                          (list "You gasp in  horror as you flood yourself, but manage to stop yourself"))
                         ((> (getf (car had-accident) :wet-amount) 300)
                          (let ((a (list "After doing a potty dance like a 5 year old, you freeze and pee yourself"
                                         "Grabbing your crotch you pause and blush as you flood yourself like an infant"
                                         "You cross your legs in a vain attempt to hold it in but fail miserably"
                                         "You gasp in embaressment as you flood yourself like a toddler"
                                         "You let out a groan as your bladder empties itself"
                                         "You fall to your knees clutching the front of your diapers struggling to keep your diapers dry and flood yourself"
                                         (format nil "LOOK EVERYBODY!!!! ~A IS WETTING ~A DIAPERS!!!!~%~%*~a eeps and hides ~a soggy padding in embarrassment*"
                                                 (string-upcase (name-of user))
                                                 (if (malep user)
                                                     "HIS"
                                                     "HER")
                                                 (name-of user)
                                                 (if (malep user)
                                                     "his"
                                                     "her")))))
                            (unless (malep user)
                              (push "You press your legs together while fidgeting and squirming until your flood your pamps like the baby girl you are" a))
                            a)))))
            (when (>= (getf (car had-accident) :wet-amount) 300)
              (push (format nil "Aww, the baby is using ~a diapers?" (if (malep user) "his" "her")) j))
            (random-elt j)))
  (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
    (format t "~a~%"
            (random-elt (list "Your face turns red as you leak everywhere"
                              "Your diaper leaks all over the place, this is why you're supposed to change it"
                              "What's the point in having a diaper if you're just going to leak everywhere like you're doing now."
                              "Your diaper leaks. There goes the carpet."
                              "Heh, baby made a puddle"
                              "Your diapers sprung a leak")))))
(defmethod output-process-potty-text ((user player) (padding (eql 'pullon)) (type (eql :wet)) (action (eql :had-accident)) had-accident)
  (format t "~a~%"
          (random-elt (cond ((<= (getf (car had-accident) :wet-amount) 10)
                             (list "You ghasp in horror as a little leaks out"
                                   "You think you just leaked a little"
                                   (format nil "A little squirts out. You quickly grab yourself with a ~a, but manage to stop the flood"
                                           (random-elt '("groan" "whine")))))
                            ((and (<= (getf (car had-accident) :wet-amount) 300) (> (getf (car had-accident) :wet-amount) 10))
                             (list "You gasp in  horror as you flood yourself, but manage to stop yourself"))
                            ((> (getf (car had-accident) :wet-amount) 300)
                             (list "After doing a potty dance like a 5 year old, you freeze and pee yourself"
                                   "Grabbing your crotch you pause and blush as you flood yourself like an infant"
                                   "You cross your legs in a vain attempt to hold it in but fail miserably"
                                   "You gasp in embaressment as you flood yourself like a toddler"
                                   "You let out a groan as your bladder empties itself"
                                   "You fall to your knees clutching the front of your pullups struggling to keep them dry and flood yourself"
                                   "The little pictures on the front of your pullups fade showing everyone what you did"
                                   (format nil "Naughty ~a wetting your pullups. You know you're supposed to use the toilet like a big kid."
                                           (if (malep user) "boy" "girl"))
                                   (format nil "LOOK EVERYBODY!!!! ~a IS WETTING ~A PULLUPS!!!!!!~%~%*~a eeps and hides ~a soggy pullups in embarrassment*"
                                           (string-upcase (name-of user))
                                           (if (malep user) "HIS" "HER")
                                           (name-of user)
                                           (if (malep user) "his" "her")))))))
  (format t "~a~%"
          (let ((out (list "Your face turns red as you leak everywhere"
                           "Your pullups leak. There goes the carpet."
                           "Heh, baby made a puddle"
                           "Your pullups sprung a leak")))
            (when (filter-items (wear-of user) 'pullup)
              (push "Your pullups leaks all over the place, You sure you're ready for those?" out))
            (random-elt out))))
(defmethod output-process-potty-text ((user player) (padding (eql 'closed-bottoms)) (type (eql :wet)) (action (eql :had-accident)) had-accident)
  (format t "~a~%"
          (random-elt (cond ((<= (getf (car had-accident) :wet-amount) 10)
                             (list "You ghasp in horror as a little leaks out"
                                   "You think you just leaked a little"
                                   (format nil
                                           "A little squirts out. You quickly grab yourself with a ~a, but manage to stop the flood"
                                           (random-elt '("groan" "whine")))))
                            ((and (<= (getf (car had-accident) :wet-amount) 300) (> (getf (car had-accident) :wet-amount) 10))
                             (list "You gasp in  horror as you flood yourself, but manage to stop yourself"))
                            ((> (getf (car had-accident) :wet-amount) 300)
                             (list "After doing a potty dance like a 5 year old, you freeze and pee yourself"
                                   "Grabbing your crotch you pause and blush as you flood yourself like an infant"
                                   "You cross your legs in a vain attempt to hold it in but fail miserably"
                                   "You gasp in embaressment as you flood yourself like a toddler"
                                   "You let out a groan as your bladder empties itself"
                                   "You fall to your knees holding your crotch struggling to keep your pants dry and flood yourself")))))
  (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
    (format t "~a~%"
            (random-elt (list "Maybe you should start wearing diapers"
                              (format nil "Bad ~a! No going potty in the house!" (if (random 2) (species-of user) (name-of user)))
                              "A puddle appears on the floor"
                              "There goes the carpet"
                              "Heh, baby made a puddle"
                              "Your pants are ruined"
                              (format nil "Heh, baby wet ~a pants" (if (malep user) "his" "her"))
                              (format nil "Bad ~a! Look what you did to your pants!"
                                      (if (random 2) (species-of user) (name-of user))))))))
(defmethod output-process-potty-text ((user player) (padding (eql nil)) (type (eql :wet)) (action (eql :had-accident)) had-accident)
  (format t "~a~%"
          (let
              ((j (cond ((<= (getf (car had-accident) :wet-amount) 10)
                         (list "You ghasp in horror as a little leaks out"
                               "You think you just leaked a little"
                               (format nil "A little squirts out. You quickly grab yourself with a ~a, but manage to stop the flood"
                                       (random-elt '("groan" "whine")))))
                        ((and (<= (getf (car had-accident) :wet-amount) 300) (> (getf (car had-accident) :wet-amount) 10))
                         (list "You gasp in  horror as you flood yourself, but manage to stop yourself"))
                        ((> (getf (car had-accident) :wet-amount) 300)
                         (list "After doing a potty dance like a 5 year old, you freeze and pee yourself"
                               "Grabbing your crotch you pause and blush as you flood yourself like an infant"
                               "You cross your legs in a vain attempt to hold it in but fail miserably"
                               "You gasp in embaressment as you flood yourself like a toddler"
                               "You let out a groan as your bladder empties itself")))))
            (random-elt j)))
  (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
    (format t "~a~%"
            (random-elt (list "Maybe you should start wearing diapers"
                              (format nil "Bad ~a! No going potty in the house!" (if (random 2) (species-of user) (name-of user)))
                              "A puddle appears on the floor"
                              "There goes the carpet"
                              "Heh, baby made a puddle")))))
(defmethod output-process-potty-text ((user player) padding (type (eql :mess)) (action (eql :potty-dance)) had-accident)
  (format t "~a~%"
          (random-elt (list "You feel like you're gonna mess yourself"
                            "You clench hard trying to avoid messing"
                            "You fart a little due to the pressure"
                            "Aww, does the baby need to potty?"))))
(defmethod output-process-potty-text ((user player) padding (type (eql :mess)) (action (eql :desparate)) had-accident)
  (format t "~a~%"
          (random-elt (list "You feel like you're gonna mess yourself"
                            "You clench hard trying to avoid messing"
                            "You fart a little due to the pressure"
                            "Aww, does the baby need to potty?"))))
(defmethod output-process-potty-text ((user player) padding (type (eql :mess)) (action (eql :need-to-potty)) had-accident)
  (format t "You need to poo~%"))
(defmethod output-process-potty-text ((user player) (padding (eql 'tabbed-briefs)) (type (eql :mess)) (action (eql :had-accident)) had-accident)
  (format t "~a~%"
          (let ((j (list "Reaching the breaking point, you instinctively squat down with your tail up and mess"
                         "Your struggle to hold it in, but your bowels decide to empty themselves anyway"
                         "You try to fart to relieve the pressure, except it wasn't a fart"
                         "You end up messing your self"
                         "The back of your diaper expands as you accidentally mess yourself"
                         "You instinctively squat down with your tail up and mess your diapers, then hold the back of your diapers checking your load in embarrassment"
                         (format nil "Heh, the baby blorted ~a pamps." (if (malep user) "his" "her")))))
            (when (filter-items (wear-of user) 'diaper)
              (push (format nil "Aww, is the baby messing ~a diapers" (if (malep user) "his" "her")) j))
            (random-elt j)))
  (when (and (cdr had-accident) (> (getf (cdr had-accident) :leak-amount) 0))
    (format t "~a~%"
            (random-elt (list "Your face turns red as your mess falls out the leg guards"
                              "Your diaper leaks all over the place, this is why you're supposed to change it"
                              "What's the point in having a diaper if you're just going to leak everywhere like you're doing now."
                              "Your diaper leaks. There goes the carpet."
                              "Not on the carpet!!!"
                              "Blowout!!!!")))))
(defmethod output-process-potty-text ((user player) (padding (eql 'pullon)) (type (eql :mess)) (action (eql :had-accident)) had-accident)
  (format t "~a~%"
          (let ((j (list "Reaching the breaking point, you instinctively squat down with your tail up and mess"
                         "Your struggle to hold it in, but your bowels decide to empty themselves anyway"
                         "You try to fart to relieve the pressure, except it wasn't a fart"
                         "You end up messing your self"
                         "The back of your pullups expands as you accidentally mess yourself")))
            (when (filter-items (wear-of user) 'pullup)
              (push (format nil "Naughty ~a messing your pullups. You know you're supposed to use the toilet like a big kid"
                            (if (malep user) "boy" "girl"))
                    j))
            (random-elt j)))
  (when (and (cdr had-accident) (> (getf (cdr had-accident) :leak-amount) 0))
    (format t "~a~%"
            (random-elt (list "Your face turns red as your mess falls out the leg guards"
                              "Your pullups leaks all over the place, You sure you're ready for those?"
                              "Your pullups leak. There goes the carpet."
                              "Not on the carpet!!!")))))
(defmethod output-process-potty-text ((user player) (padding (eql 'closed-bottoms)) (type (eql :mess)) (action (eql :had-accident)) had-accident)
  (format t "~a~%"
          (random-elt (list "Reaching the breaking point, you instinctively squat down with your tail up and mess"
                            "Your struggle to hold it in, but your bowels decide to empty themselves anyway"
                            "You try to fart to relieve the pressure, except it wasn't a fart"
                            "You end up messing your self"
                            "a lump forms at the seat of your pants")))
  (when (and (cdr had-accident) (> (getf (cdr had-accident) :leak-amount) 0))
    (format t "~a~%"
            (random-elt (list "Maybe you should start wearing diapers"
                              (format nil "Bad ~a! No going potty in the house!"
                                      (if (random 2) (species-of user) (name-of user)))
                              "There goes the carpet"
                              "Heh, baby made a mess"
                              "Your pants are ruined"
                              (format nil "Heh, baby messed ~a pants" (if (malep user) "his" "her"))
                              (format nil "Bad ~a! Look what you did to your pants!" (if (random 2) (species-of user) (name-of user))))))))
(defmethod output-process-potty-text ((user player) (padding (eql nil)) (type (eql :mess)) (action (eql :had-accident)) had-accident)
  (format t "~a~%"
          (random-elt (list "Reaching the breaking point, you instinctively squat down with your tail up and mess"
                            "Your struggle to hold it in, but your bowels decide to empty themselves anyway"
                            "You try to fart to relieve the pressure, except it wasn't a fart"
                            "You end up messing your self")))
  (when (and (cdr had-accident) (> (getf (cdr had-accident) :leak-amount) 0))
    (format t "~a~%"
            (random-elt (list "Maybe you should start wearing diapers"
                              (format nil "Bad ~a! No going potty in the house!"
                                      (if (random 2) (species-of user) (name-of user)))
                              "There goes the carpet"
                              "Heh, baby made a mess")))))
(defmethod output-process-potty-text ((user ally-last-minute-potty-training) (padding (eql 'tabbed-briefs)) (type (eql :wet)) (action (eql :had-accident)) had-accident)
  (let ((normal ())
        (leak ()))
    (if (and (car had-accident) (= (getf (car had-accident) :wet-amount) 10))
        (progn
          (do-push (with-output-to-string (s)
                     (format s "~a: *gasps in horror* I think a little just came out!!!!~%~%" (name-of user))
                     (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
                       (format s "~a: You're making a puddle~%~%" (name-of (player-of *game*)))
                       (format s "~a: NUUUUU!!!!~%~%" (name-of user))))
            normal leak)
          (do-push (with-output-to-string (s)
                     (format s "*~a quietly moans at a little squirts out~%~%" (name-of user))
                     (format s "~a: Did you wet yourself?~%~%" (name-of (player-of *game*)))
                     (format s "~a: *quietly* No ~%~%" (name-of user))
                     (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
                       (format s "~a Your diaper's leaking~%~%" (name-of (player-of *game*)))
                       (format s "~a: GAH!!!!~%~%" (name-of (player-of *game*)))))
            normal leak))
        (progn
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
            normal)
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
            leak)
          (do-push (with-output-to-string (s)
                     (format s "*~a bounces up and down with ~a knees pressed together and paws pressed against ~a crotch, pauses when ~a bladder gives out looks down and notices "
                             (name-of user)
                             (if (malep user) "his" "her")
                             (if (malep user) "his" "her")
                             (if (malep user) "his" "her"))
                     (format s "~a soggy padding, blushes heavily and quickly covers ~a soggy padding with ~a paws and tail hoping no one will notice*~%~%"
                             (if (malep user) "his" "her")
                             (if (malep user) "his" "her")
                             (if (malep user) "his" "her"))
                     )
            normal)
          (do-push (with-output-to-string (s)
                     (format s "*~a bounces up and down with ~a knees pressed together and paws pressed against ~a crotch, pauses when ~a bladder gives out looks down and notices "
                             (name-of user)
                             (if (malep user) "his" "her")
                             (if (malep user) "his" "her")
                             (if (malep user) "his" "her"))
                     (format s "~a padding is leaking, blushes heavily and quickly covers ~a soggy padding with ~a paws and tail hoping no one will notice*~%~%"
                             (if (malep user) "his" "her")
                             (if (malep user) "his" "her")
                             (if (malep user) "his" "her"))
                     )
            leak)))
    (if (> (getf (car had-accident) :leak-amount) 0)
        (format t "~a" (random-elt leak))
        (format t "~a" (random-elt normal)))))
(defmethod output-process-potty-text ((user ally-last-minute-potty-training) (padding (eql 'pullon)) (type (eql :wet)) (action (eql :had-accident)) had-accident)
  (let ((normal ())
        (leak ()))
    (if (and (car had-accident) (= (getf (car had-accident) :wet-amount) 10))
        (progn
          (do-push (with-output-to-string (s)
                     (format s "~a: *gasps in horror* I think a little just came out!!!!~%~%" (name-of user))
                     (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
                       (format s "~a: You're making a puddle~%~%" (name-of (player-of *game*)))
                       (format s "~a: NUUUUU!!!!~%~%" (name-of user))))
            normal leak)
          (do-push (with-output-to-string (s)
                     (format s "*~a quietly moans at a little squirts out~%~%" (name-of user))
                     (format s "~a: Did you wet yourself?~%~%" (name-of (player-of *game*)))
                     (format s "~a: *quietly* No ~%~%" (name-of user))
                     (cond
                       ((filter-items (wear-of user) 'pullup)
                        (format s "~a: Those pictures on the front of your pullups better not fade~%~%"
                                (name-of (player-of *game*)))
                        (format s "~a: They're not, honest ~%~%" (name-of user))
                        (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
                          (format s "*~a checks ~a's pullups. Notices they're drenched~%~%" (name-of (player-of *game*)) (name-of user))
                          (format s "~a: Uh huh, sure~%~%" (name-of (player-of *game*)))))))
            normal leak))
        (progn
          (do-push (with-output-to-string (s)
                     (format s "*~a bounces up and down with ~a knees pressed together and paws pressed against ~a crotch, pauses when ~a bladder gives out looks down and notices "
                             (name-of user)
                             (if (malep user) "his" "her")
                             (if (malep user) "his" "her")
                             (if (malep user) "his" "her"))
                     (if (filter-items (wear-of user) 'pullup)
                         (format s "the pictures on ~a pullups have faded, blushes heavily and quickly covers ~a soggy pullups with ~a paws and tail hoping no one will notice*~%~%"
                                 (if (malep user) "his" "her")
                                 (if (malep user) "his" "her")
                                 (if (malep user) "his" "her"))
                         (format s "that ~a wetted ~a pullups, blushes heavily and quickly covers ~a soggy pullups with ~a paws and tail hoping no one will notice*~%~%"
                                 (if (malep user) "he" "she")
                                 (if (malep user) "his" "her")
                                 (if (malep user) "his" "her")
                                 (if (malep user) "his" "her"))))
            normal leak)
          (do-push (with-output-to-string (s)
                     (format s "*~a has an accident*~%~%"
                             (name-of user))
                     (format s "~a: Bad ~a. You know you're supposed to use the toilet like a big ~a. Just look what you did to your pullups~%~%"
                             (name-of (player-of *game*))
                             (name-of user)
                             (if (malep user) "boy" "girl")))
            normal)
          (when (filter-items (wear-of user) 'pullup)
            (do-push (with-output-to-string (s)
                       (format s "*~a has an accident and leaks*~%~%"
                               (name-of user))
                       (format s "~a: Bad ~a. You know you're supposed to use the toilet like a big ~a. Just look at the mess you made on the floor~%~%"
                               (name-of (player-of *game*))
                               (name-of user)
                               (if (malep user) "boy" "girl")))
              leak))))
    (if (> (getf (car had-accident) :leak-amount) 0)
        (format t "~a" (random-elt leak))
        (format t "~a" (random-elt normal)))))
(defmethod output-process-potty-text ((user ally-last-minute-potty-training) (padding (eql 'closed-bottoms)) (type (eql :wet)) (action (eql :had-accident)) had-accident)
  (let ((normal ())
        (leak ()))
    (if (and (car had-accident) (= (getf (car had-accident) :wet-amount) 10))
        (progn
          (do-push (with-output-to-string (s)
                     (format s "~a: *gasps in horror* I think a little just came out!!!!~%~%" (name-of user))
                     (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
                       (format s "~a: You're making a puddle~%~%" (name-of (player-of *game*)))
                       (format s "~a: NUUUUU!!!!~%~%" (name-of user))))
            normal leak)
          (do-push (with-output-to-string (s)
                     (format s "*~a quietly moans at a little squirts out~%~%" (name-of user))
                     (format s "~a: Did you wet yourself?~%~%" (name-of (player-of *game*)))
                     (format s "~a: *quietly* No ~%~%" (name-of user)))
            normal leak))
        (progn
          (do-push (with-output-to-string (s)
                     (format s "*~a bounces up and down with ~a knees pressed together and paws pressed against ~a crotch, pauses when ~a bladder gives out looks down and notices "
                             (name-of user)
                             (if (malep user) "his" "her")
                             (if (malep user) "his" "her")
                             (if (malep user) "his" "her"))
                     (format s "~a flooded ~aself, blushes heavily and quickly covers the front of ~a pants with ~a paws and tail hoping no one will notice*~%~%"
                             (if (malep user) "his" "her")
                             (if (malep user) "him" "her")
                             (if (malep user) "his" "her")
                             (if (malep user) "his" "her")))
            normal leak)))
    (if (> (getf (car had-accident) :leak-amount) 0)
        (format t "~a" (random-elt leak))
        (format t "~a" (random-elt normal)))))
(defmethod output-process-potty-text ((user ally-last-minute-potty-training) (padding (eql nil)) (type (eql :wet)) (action (eql :had-accident)) had-accident)
  (let ((normal ())
        (leak ()))
    (if (and (car had-accident) (= (getf (car had-accident) :wet-amount) 10))
        (progn
          (do-push (with-output-to-string (s)
                     (format s "~a: *gasps in horror* I think a little just came out!!!!~%~%" (name-of user))
                     (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
                       (format s "~a: You're making a puddle~%~%" (name-of (player-of *game*)))
                       (format s "~a: NUUUUU!!!!~%~%" (name-of user))))
            normal leak)
          (do-push (with-output-to-string (s)
                     (format s "*~a quietly moans at a little squirts out~%~%" (name-of user))
                     (format s "~a: Did you wet yourself?~%~%" (name-of (player-of *game*)))
                     (format s "~a: *quietly* No ~%~%" (name-of user)))
            normal leak))
        (progn
          (do-push
              (with-output-to-string (s)
                (format s "*~a crosses ~a legs pressing ~a paws against ~a crotch as a puddle forms beneath ~a feet*~%~%"
                        (name-of user)
                        (if (malep user) "his" "her")
                        (if (malep user) "his" "her")
                        (if (malep user) "his" "her")
                        (if (malep user) "his" "her")))
            normal leak)
          (do-push
              (with-output-to-string (s)
                (format s "*~a has an accident and makes a mess on the floor. " (name-of user))
                (format s "Then walks away heavily blushing hoping no one will notice*~%~%"))
            normal leak)))
    (if (> (getf (car had-accident) :leak-amount) 0)
        (format t "~a" (random-elt leak))
        (format t "~a" (random-elt normal)))))
(defmethod output-process-potty-text ((user ally-last-minute-potty-training) padding (type (eql :wet)) (action (eql :potty-dance)) had-accident)
  (format t "~a"
          (random-elt (if (= (random 5) 0)
                          (list (with-output-to-string (s)
                                  (format s "~a: ~a, do you need to potty?~%~%"
                                          (name-of (player-of *game*))
                                          (name-of user))
                                  (format s "~a: No, I'm fine *bounces up and down holding ~aself*~%~%"
                                          (name-of user)
                                          (if (malep user) "him" "her")))
                                (with-output-to-string (s)
                                  (format s "~a: ~a, do you need to potty?~%~%"
                                          (name-of (player-of *game*))
                                          (name-of user))
                                  (format s "~a: No, I'm ok *hops from foot to foot holding ~a crotch*~%~%"
                                          (name-of user)
                                          (if (malep user) "his" "her")))
                                (with-output-to-string (s)
                                  (format s "~a: ~a, do you need to potty?~%~%"
                                          (name-of (player-of *game*))
                                          (name-of user))
                                  (format s "~a: No, I'm alright *moans with ~a legs twisted holding ~a crotch*~%~%"
                                          (name-of user)
                                          (if (malep user) "his" "her")
                                          (if (malep user) "his" "her"))))
                          (list (with-output-to-string (s)
                                  (format s "*~a is doing a potty dance like a 5 year old*~%~%"
                                          (name-of user)))
                                (with-output-to-string (s)
                                  (format s "*~a is bouncing up and down with ~a knees knocked together holding ~aself*~%~%"
                                          (name-of user)
                                          (if (malep user) "his" "her")
                                          (if (malep user) "him" "her")))
                                (with-output-to-string (s)
                                  (format s "*~a is hopping from foot to foot*~%~%"
                                          (name-of user)))
                                (with-output-to-string (s)
                                  (format s "*~a starts moaning with ~a legs crossed*~%~%"
                                          (name-of user)
                                          (if (malep user) "his" "her"))))))))
(defmethod output-process-potty-text ((user ally-last-minute-potty-training) padding (type (eql :wet)) (action (eql :desparate)) had-accident)
  (format t "~a"
          (random-elt
           (if (= (random 5) 0)
               (list (with-output-to-string (s)
                       (format s "~a: ~a!!! I GOTTY POTTY!!! *bounces up and down holding ~aself*~%~%"
                               (name-of user)
                               (name-of (player-of *game*))
                               (if (malep user) "him" "her")))
                     (with-output-to-string (s)
                       (format s "~a: ~a!!! HURRY!!! I CAN'T HOLD IT MUCH LONGER!!! *hops from foot to foot holding ~a crotch*~%~%"
                               (name-of user)
                               (name-of (player-of *game*))
                               (if (malep user) "his" "her")))
                     (with-output-to-string (s)
                       (format s "~a: ~a!!! PLEASE TAKE ME TO THE POTTY!!! I'M ABOUT TO WET MYSELF!!! *bounces up and down holding ~aself*~%~%"
                               (name-of user)
                               (name-of (player-of *game*))
                               (if (malep user) "him" "her"))))
               (list (with-output-to-string (s)
                       (format s "*~a is doing a potty dance like a 5 year old*~%~%"
                               (name-of user)))
                     (with-output-to-string (s)
                       (format s "*~a is bouncing up and down with ~a knees pressed together holding ~aself*~%~%"
                               (name-of user)
                               (if (malep user) "his" "her")
                               (if (malep user) "him" "her")))
                     (with-output-to-string (s)
                       (format s "*~a is hopping from foot to foot*~%~%"
                               (name-of user)))
                     (with-output-to-string (s)
                       (format s "*~a starts moaning with ~a legs crossed*~%~%"
                               (name-of user)
                               (if (malep user) "his" "her"))))))))
(defmethod output-process-potty-text ((user ally-rebel-potty-training) padding (type (eql :wet)) (action (eql :had-accident)) had-accident)
  (cond ((and
          (car had-accident)
          (> (getf (car had-accident) :leak-amount) 0))
         (format t "~a"
                 (random-elt
                  (list (with-output-to-string (s)
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
                        (with-output-to-string (s)
                          (format s "*~a floods ~a nappies, then leaks and leaves puddles*~%~%"
                                  (name-of user)
                                  (if (malep user) "his" "her")))
                        (with-output-to-string (s)
                          (format s "*~a floods his nappies, then gets an expression of horror on ~a face when ~a diaper leaks and a puddle forms, then starts waddling with ~a legs spread apart*~%~%"
                                  (name-of user)
                                  (if (malep user) "his" "her")
                                  (if (malep user) "his" "her")
                                  (if (malep user) "his" "her")))
                        (with-output-to-string (s)
                          (format s "*~a decides to flood ~a already waterlogged diaper, then acts all surprised when it leaks*~%~%"
                                  (name-of user)
                                  (if (malep user) "his" "her")))
                        (with-output-to-string (s)
                          (format s "*~a floods his diapers and starts leaving a puddle, then freaks and waddles towards ~a with ~a legs spread apart like a 5 year old who didn't make it*~%~%"
                                  (name-of user)
                                  (name-of (player-of *game*))
                                  (if (malep user) "his" "her"))
                          (format s "~a: Umm ~a, I think I need a change.~%~%"
                                  (name-of user)
                                  (name-of (player-of *game*)))
                          (format s "~a: No shit~%~%"
                                  (name-of (player-of *game*))))))))
        ((and (car had-accident)
              (> (getf (car had-accident) :wet-amount) 0))
         (format t "~a"
                 (random-elt (list (with-output-to-string (s)
                                     (format s "*~a stops in his tracks*~%~%"
                                             (name-of user))
                                     (format s "~a: Is something the matter?~%~%"
                                             (name-of (player-of *game*)))
                                     (format s "~a: what do you mean? *a soft hiss can be heard coming from the front of ~a diaper*~%~%"
                                             (name-of user)
                                             (if (malep user) "his" "her"))
                                     (format s "~a: Oh, never mind~%~%"
                                             (name-of (player-of *game*))))
                                   (with-output-to-string (s)
                                     (format s "*~a pauses and floods ~a diapers*~%~%"
                                             (name-of user)
                                             (if (malep user) "his" "her")))
                                   (with-output-to-string (s)
                                     (format s "*~a floods ~a diapers*~%~%"
                                             (name-of user)
                                             (if (malep user) "his" "her")))))))))
(defmethod output-process-potty-text ((user ally-no-potty-training) padding (type (eql :wet)) (action (eql :had-accident)) had-accident)
  (cond ((and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
         (format t "~a"
                 (random-elt (list (with-output-to-string (s)
                                     (format s "*~a floods ~a nappies, then leaks and leaves puddles*~%~%"
                                             (name-of user)
                                             (if (malep user) "his" "her")))
                                   (with-output-to-string (s)
                                     (format s "*~a floods his nappies, then gets an expression of horror on ~a face when ~a diaper leaks and a puddle forms, then starts waddling with ~a legs spread apart*~%~%"
                                             (name-of user)
                                             (if (malep user) "his" "her")
                                             (if (malep user) "his" "her")
                                             (if (malep user) "his" "her")))
                                   (with-output-to-string (s)
                                     (format s "*~a decides to flood ~a already waterlogged diaper, then acts all surprised when it leaks*~%~%"
                                             (name-of user)
                                             (if (malep user) "his" "her")))
                                   (with-output-to-string (s)
                                     (format s "*~a floods his diapers and starts leaving a puddle, then freaks and waddles towards ~a with ~a legs spread apart like a 5 year old who didn't make it*~%~%"
                                             (name-of user)
                                             (name-of (player-of *game*))
                                             (if (malep user) "his" "her"))
                                     (format s "~a: Umm ~a, I think I need a change.~%~%"
                                             (name-of user)
                                             (name-of (player-of *game*)))
                                     (format s "~a: No shit~%~%"
                                             (name-of (player-of *game*))))))))
        ((and (car had-accident) (> (getf (car had-accident) :wet-amount) 0))
         (format t "~a"
                 (random-elt (list (with-output-to-string (s)
                                     (format s "*~a floods ~a diapers*~%~%"
                                             (name-of user)
                                             (if (malep user) "his" "her")))))))))
(defmethod output-process-potty-text ((user ally-last-minute-potty-training) (padding (eql :diaper)) (type (eql :mess)) (action (eql :had-accident)) had-accident)
  (let ((normal ())
        (leak ()))
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
      normal)
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
               (apply #'format s "*~a gasps with a horrified look on ~a face when ~a notices it.~%~%"
                      (name-of user)
                      (if (malep user)
                          '("his" "he")
                          '("her" "she"))))
      leak)
    (do-push (with-output-to-string (s)
               (apply #'format s "*~a instinctively squats down and accidentally messes ~a diapers then gasps in horror when ~a realized what ~a did*~%~%"
                      (name-of user)
                      (if (malep user)
                          '("his" "he" "he")
                          '("her" "she" "she"))))
      normal)
    (do-push (with-output-to-string (s)
               (apply #'format s "*~a instinctively squats down and accidentally messes ~a diapers then holds the back of ~a diapers checking ~a load in embarrassment*~%~%"
                      (name-of user)
                      (if (malep user)
                          '("his" "his" "his" "his")
                          '("her" "her" "her" "her"))))
      normal)
    (do-push (with-output-to-string (s)
               (apply #'format s "*~a instinctively squats down and accidentally messes ~a diapers then gasps in horror when ~a notices the poo falling down the leg guards*~%~%"
                      (name-of user)
                      (if (malep user)
                          '("his" "he")
                          '("her" "she"))))
      leak)
    (if (> (getf (cdr had-accident) :leak-amount) 0)
        (format t "~a" (random-elt leak))
        (format t "~a" (random-elt normal)))))
(defmethod output-process-potty-text ((user ally-last-minute-potty-training) (padding (eql :pullup)) (type (eql :mess)) (action (eql :had-accident)) had-accident)
  (let ((normal ())
        (leak ()))
    (do-push (with-output-to-string (s)
               (apply #'format s "*~a instinctively squats down and accidentally messes ~a pullups then gasps in horror when ~a realized what ~a did*~%~%"
                      (name-of user)
                      (if (malep user)
                          '("his" "he" "he")
                          '("her" "she" "she"))))
      normal)
    (do-push (with-output-to-string (s)
               (apply #'format s "*~a instinctively squats down and accidentally messes ~a pullups then gasps in horror when ~a notices the poo falling down the leg guards*~%~%"
                      (name-of user)
                      (if (malep user)
                          '("his" "he")
                          '("her" "she"))))
      leak)
    (when (filter-items (wear-of user) 'pullup)
      (do-push (with-output-to-string (s)
                 (format s "*~a has an accident and leaks*~%~%"
                         (name-of user))
                 (format s "~a: Bad ~a. You know you're supposed to use the toilet like a big ~a. Just look at the mess you made on the floor~%~%"
                         (name-of (player-of *game*))
                         (name-of user)
                         (if (malep user) "boy" "girl")))
        leak))
    (if (> (getf (cdr had-accident) :leak-amount) 0)
        (format t "~a" (random-elt leak))
        (format t "~a" (random-elt normal)))))
(defmethod output-process-potty-text ((user ally-last-minute-potty-training) (padding (eql 'closed-bottoms)) (type (eql :mess)) (action (eql :had-accident)) had-accident)
  (let ((normal ())
        (leak ()))
    (do-push (with-output-to-string (s)
               (apply #'format s "*~a instinctively squats down and accidentally messes ~a pants then gasps in horror when ~a realized what ~a did*~%~%"
                      (name-of user)
                      (if (malep user)
                          '("his" "he" "he")
                          '("her" "she" "she"))))
      normal leak)
    (if (> (getf (cdr had-accident) :leak-amount) 0)
        (format t "~a" (random-elt leak))
        (format t "~a" (random-elt normal)))))
(defmethod output-process-potty-text ((user ally-last-minute-potty-training) (padding (eql nil)) (type (eql :mess)) (action (eql :had-accident)) had-accident)
  (let ((normal ())
        (leak ()))
    (do-push
        (with-output-to-string (s)
          (format s "*~a accidentally messes ~aself as it falls on the floor*~%~%"
                  (name-of user)
                  (if (malep user) "his" "her"))
          (format s "~a: Bad ~a!!! No going potty on the floor!!!~%~%"
                  (name-of (player-of *game*))
                  (name-of user))
          (format s "~a: I didn't mean to!!!~%~%"
                  (name-of user))
          (format s "~a: A likely story~%~%"
                  (name-of (player-of *game*))))
      normal leak)
    (do-push (with-output-to-string (s)
               (format s "*~a has an accident and makes a mess on the floor. " (name-of user))
               (format s "Then walks away heavily blushing hoping no one will notice*~%~%"))
      normal leak)
    (if (> (getf (cdr had-accident) :leak-amount) 0)
        (format t "~a" (random-elt leak))
        (format t "~a" (random-elt normal)))))
(defmethod output-process-potty-text ((user ally-last-minute-potty-training) padding (type (eql :mess)) (action (eql :potty-dance)) had-accident)
  (format t "~a"
          (random-elt (if (= (random 5) 0)
                          (list (with-output-to-string (s)
                                  (format s "~a: ~a, do you need to potty?~%~%"
                                          (name-of (player-of *game*))
                                          (name-of user))
                                  (format s "~a: No, I'm fine *bounces up and down holding ~aself*~%~%"
                                          (name-of user)
                                          (if (malep user) "him" "her")))
                                (with-output-to-string (s)
                                  (format s "~a: ~a, do you need to potty?~%~%"
                                          (name-of (player-of *game*))
                                          (name-of user))
                                  (format s "~a: No, I'm ok *hops from foot to foot*~%~%"
                                          (name-of user)))
                                (with-output-to-string (s)
                                  (format s "~a: ~a, do you need to potty?~%~%"
                                          (name-of (player-of *game*))
                                          (name-of user))
                                  (format s "~a: No, I'm alright *moans with ~a legs twisted*~%~%"
                                          (name-of user)
                                          (if (malep user) "his" "her"))))
                          (list (with-output-to-string (s)
                                  (format s "*~a is doing a potty dance like a 5 year old*~%~%"
                                          (name-of user)))
                                (with-output-to-string (s)
                                  (format s "*~a is bouncing up and down with ~a knees pressed together holding ~aself*~%~%"
                                          (name-of user)
                                          (if (malep user) "his" "her")
                                          (if (malep user) "him" "her")))
                                (with-output-to-string (s)
                                  (format s "*~a is hopping from foot to foot*~%~%"
                                          (name-of user)))
                                (with-output-to-string (s)
                                  (format s "*~a starts moaning with ~a legs crossed*~%~%"
                                          (name-of user)
                                          (if (malep user) "his" "her"))))))))
(defmethod output-process-potty-text ((user ally-last-minute-potty-training) padding (type (eql :mess)) (action (eql :desparate)) had-accident)
  (format t "~a"
          (random-elt (if (= (random 5) 0)
                          (list (with-output-to-string (s)
                                  (format s "~a: ~a!!! I GOTTY POTTY!!! *bounces up and down holding ~aself*~%~%"
                                          (name-of user)
                                          (name-of (player-of *game*))
                                          (if (malep user) "him" "her")))
                                (with-output-to-string (s)
                                  (format s "~a: ~a!!! HURRY!!! I CAN'T HOLD IT MUCH LONGER!!! *hops from foot to foot holding ~a crotch*~%~%"
                                          (name-of user)
                                          (name-of (player-of *game*))
                                          (if (malep user) "his" "her"))))
                          (progn (with-output-to-string (s)
                                   (format s "*~a is doing a potty dance like a 5 year old*~%~%"
                                           (name-of user)))
                                 (with-output-to-string (s)
                                   (format s "*~a farts to relieve the pressure*~%~%"
                                           (name-of user)))
                                 (with-output-to-string (s)
                                   (format s "*~a is bouncing up and down with ~a knees pressed together holding ~aself*~%~%"
                                           (name-of user)
                                           (if (malep user) "his" "her")
                                           (if (malep user) "him" "her")))
                                 (with-output-to-string (s)
                                   (format s "*~a is hopping from foot to foot*~%~%"
                                           (name-of user)))
                                 (with-output-to-string (s)
                                   (format s "*~a starts moaning with ~a legs crossed*~%~%"
                                           (name-of user)
                                           (if (malep user) "his" "her"))))))))
(defmethod output-process-potty-text ((user ally-rebel-potty-training) padding (type (eql :mess)) (action (eql :had-accident)) had-accident)
  (cond ((and (cdr had-accident) (> (getf (cdr had-accident) :leak-amount) 0))
         (format t "~a"
                 (random-elt (list (with-output-to-string (s)
                                     (format s "*~a squats down and pushes a big load into ~a already loaded diaper, then predictably has a blowout*~%~%"
                                             (name-of user)
                                             (if (malep user) "his" "her")))))))
        ((and (cdr had-accident) (> (getf (cdr had-accident) :mess-amount) 0))
         (format t "~a"
                 (random-elt (list (with-output-to-string (s)
                                     (format s "*~a squats down and pushes a big load into ~a diaper like an infant*~%~%"
                                             (name-of user)
                                             (if (malep user) "his" "her")))
                                   (with-output-to-string (s)
                                     (apply #'format s "*~a squats down and pushes a big load into ~a diaper then holds the back of ~a diaper checking ~a new load as if giving ~aself a diaper check*~%~%"
                                            (name-of user)
                                            (if (malep user)
                                                '("his" "his" "his" "him")
                                                '("her" "her" "her" "her"))))))))))
(defmethod output-process-potty-text ((user ally-no-potty-training) padding (type (eql :mess)) (action (eql :had-accident)) had-accident)
  (cond ((and (cdr had-accident) (> (getf (cdr had-accident) :leak-amount) 0))
         (format t "~a"
                 (random-elt (list (with-output-to-string (s)
                                     (format s "*~a squats down and pushes a big load into ~a already loaded diaper, then predictably has a blowout*~%~%"
                                             (name-of user)
                                             (if (malep user) "his" "her")))))))
        ((and (cdr had-accident) (> (getf (cdr had-accident) :mess-amount) 0))
         (format t "~a"
                 (random-elt (list (with-output-to-string (s)
                                     (format s "*~a squats down and pushes a big load into ~a diaper like an infant*~%~%"
                                             (name-of user)
                                             (if (malep user) "his" "her")))))))))
(defmethod output-process-potty-text ((user ally-silent-potty-training) padding (type (eql :wet)) (action (eql :potty-dance)) had-accident)
  (format t "~a~%"
          (random-elt (let ((a (list (format nil "*~a is doing a potty dance like a 5 year old*"
                                             (name-of user))
                                     (format nil "*~a hops from foot to foot holding ~a crotch*"
                                             (name-of user)
                                             (if (malep user)
                                                 "his" "her"))
                                     (format nil "*~a bounces up and down holding ~aself*"
                                             (name-of user)
                                             (if (malep user)
                                                 "his" "her")))))
                        (unless (malep user)
                          (push (format nil "~a fidgets and squirms while pressing her legs together" (name-of user)) a))
                        a))))
(defmethod output-process-potty-text ((user ally-silent-potty-training) padding (type (eql :wet)) (action (eql :desparate)) had-accident)
  (format t "~a~%"
          (random-elt (let ((a (list (format nil "*~a is doing a potty dance like a 5 year old*"
                                             (name-of user))
                                     (format nil "*~a hops from foot to foot holding ~a crotch*"
                                             (name-of user)
                                             (if (malep user)
                                                 "his" "her"))
                                     (format nil "*~a bounces up and down holding ~aself*"
                                             (name-of user)
                                             (if (malep user)
                                                 "him" "her"))
                                     (apply #'format nil "*~a whines as ~a hold ~aself in desperation*"
                                            (name-of user)
                                            (if (malep user)
                                                '("he" "him")
                                                '("she" "her"))))))
                        (unless (malep user)
                          (push (format nil "~a fidgets, squirms, and bounces while pressing her legs together" (name-of user))
                                a))
                        a))))
(defmethod output-process-potty-text ((user ally-silent-potty-training) (padding (eql 'tabbed-briefs)) (type (eql :wet)) (action (eql :had-accident)) had-accident)
  (format t "~a~%"
          (let ((j (cond ((<= (getf (car had-accident) :wet-amount) 10)
                          (list (format nil "*~a ghasp in horror as a little leaks out*" (name-of user))
                                (format nil "*~a's bladder just leaked a little*" (name-of user))))
                         ((and (<= (getf (car had-accident) :wet-amount) 300) (> (getf (car had-accident) :wet-amount) 10))
                          (list (apply #'format nil "*~a gasps in horror as ~a floods ~aself, but manages to stop ~aself*"
                                       (if (malep user)
                                           '("he" "him" "him")
                                           '("she" "her" "he")))))
                         ((> (getf (car had-accident) :wet-amount) 300)
                          (list (format nil "*After doing a potty dance like a 5 year old, ~a freezes and pee ~aself*"
                                        (name-of user)
                                        (if (malep user)
                                            "him"
                                            "her"))
                                (apply #'format nil "*~a Grabs ~a crotch, pauses and blushes as ~a flood ~a diapers like an infant*"
                                       (name-of user)
                                       (if (malep user)
                                           '("his" "he" "his")
                                           '("her" "she" "her")))
                                (format nil "*~a cross ~a legs in a vain attempt to hold it in but fails miserably*"
                                        (name-of user)
                                        (if (malep user)
                                            "his"
                                            "her"))
                                (apply #'format nil "~a gasps in embaressment as ~a floods ~aself like a toddler"
                                       (name-of user)
                                       (if (malep user)
                                           '("he" "him")
                                           '("she" "her")))
                                (apply #'format nil "~a falls to ~a knees clutching the front of ~a diaper in a desparate attempt to keep ~a diapers dry but ends up flooding ~a diapers"
                                       (name-of user)
                                       (iter (for i from 1 to 4)
                                         (collect (if (malep user)
                                                      "his"
                                                      "her")))))))))
            (random-elt j)))
  (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
    (format t "~a~%"
            (random-elt (list (format nil "*~a's face turns red as ~a leak everywhere*"
                                      (name-of user)
                                      (if (malep user) "he" "she"))
                              (format nil "*~a leaves a puddle then starts waddling around with ~a legs spread apart leaving a trail like a 5 year old who didn't make it*"
                                      (name-of user)
                                      (if (malep user) "he" "she"))
                              (format nil "*~a's diapers sprung a leak*"
                                      (name-of user))
                              (format nil "~a: Aww, looks like ~a's diapers sprung a leak~%~%*~ablushes heavily at the embarrassing comment*"
                                      (name-of (player-of *game*))
                                      (name-of user)
                                      (name-of user)))))))
(defmethod output-process-potty-text ((user ally-silent-potty-training) (padding (eql 'pullon)) (type (eql :wet)) (action (eql :had-accident)) had-accident)
  (format t "~a~%"
          (let ((j (cond ((<= (getf (car had-accident) :wet-amount) 10)
                          (list (format nil "*~a ghasp in horror as a little leaks out*" (name-of user))
                                (format nil "*~a's bladder just leaked a little*" (name-of user))))
                         ((and (<= (getf (car had-accident) :wet-amount) 300) (> (getf (car had-accident) :wet-amount) 10))
                          (list (apply #'format nil "*~a gasps in horror as ~a floods ~aself, but manages to stop ~aself*"
                                       (if (malep user)
                                           '("he" "him" "him")
                                           '("she" "her" "he")))))
                         ((> (getf (car had-accident) :wet-amount) 300)
                          (list (format nil "*After doing a potty dance like a 5 year old, ~a freezes and pee ~aself*"
                                        (name-of user)
                                        (if (malep user)
                                            "him"
                                            "her"))
                                (apply #'format nil "*~a Grabs ~a crotch, pauses and blushes as ~a flood ~aself like an infant*"
                                       (name-of user)
                                       (if (malep user)
                                           '("his" "he" "him")
                                           '("her" "she" "her")))
                                (format nil "*~a cross ~a legs in a vain attempt to hold it in but fails miserably*"
                                        (name-of user)
                                        (if (malep user)
                                            "his"
                                            "her"))
                                (apply #'format nil "~a gasps in embaressment as ~a floods ~aself like a toddler"
                                       (name-of user)
                                       (if (malep user)
                                           '("he" "him")
                                           '("she" "her")))
                                (apply #'format nil "~a falls to ~a knees clutching the front of ~a pullups in a desparate attempt to keep the pictures on the front of ~a pullups from fading but ends up flooding ~a pullups"
                                       (name-of user)
                                       (iter (for i from 1 to 4)
                                         (collect (if (malep user)
                                                      "his"
                                                      "her")))))))))
            (when (>= (getf (car had-accident) :wet-amount) 300)
              (push (format nil "*The little pictures on the front of ~a's pullups fade showing everyone what ~a did*"
                            (name-of user)
                            (if (malep user) "he" "she"))
                    j))
            (random-elt j)))
  (format t "~a~%"
          (random-elt (list (format nil "*~a's face turns red as ~a leak everywhere*"
                                    (name-of user)
                                    (if (malep user) "he" "she"))
                            (format nil "*~a leaves a puddle then starts waddling around with ~a legs spread apart leaving a trail like a 5 year old who didn't make it*"
                                    (name-of user)
                                    (if (malep user) "he" "she"))
                            (format nil "*~a's pullups sprung a leak*"
                                    (name-of user))))))
(defmethod output-process-potty-text ((user ally-silent-potty-training) (padding (eql 'closed-bottoms)) (type (eql :wet)) (action (eql :had-accident)) had-accident)
  (format t "~a~%"
          (let ((j (cond ((<= (getf (car had-accident) :wet-amount) 10)
                          (list (format nil "*~a ghasp in horror as a little leaks out*" (name-of user))
                                (format nil "*~a's bladder just leaked a little*" (name-of user))))
                         ((and (<= (getf (car had-accident) :wet-amount) 300) (> (getf (car had-accident) :wet-amount) 10))
                          (list (apply #'format nil "*~a gasps in horror as ~a floods ~aself, but manages to stop ~aself*"
                                       (if (malep user)
                                           '("he" "him" "him")
                                           '("she" "her" "he")))))
                         ((> (getf (car had-accident) :wet-amount) 300)
                          (let ((a (list (format nil "*After doing a potty dance like a 5 year old, ~a freezes and pee ~aself*"
                                                 (name-of user)
                                                 (if (malep user)
                                                     "him"
                                                     "her"))
                                         (apply #'format nil "*~a Grabs ~a crotch, pauses and blushes as ~a flood ~aself like an infant*"
                                                (name-of user)
                                                (if (malep user)
                                                    '("his" "he" "him")
                                                    '("her" "she" "her")))
                                         (format nil "*~a cross ~a legs in a vain attempt to hold it in but fails miserably*"
                                                 (name-of user)
                                                 (if (malep user)
                                                     "his"
                                                     "her"))
                                         (apply #'format nil "~a gasps in embaressment as ~a floods ~aself like a toddler"
                                                (name-of user)
                                                (if (malep user)
                                                    '("he" "him")
                                                    '("she" "her")))
                                         (apply #'format nil "~a falls to ~a knees holding ~a crotch in a desparate attempt to keep ~a pants dry but ends up flooding ~a pants"
                                                (name-of user)
                                                (iter (for i from 1 to 4)
                                                  (collect (if (malep user)
                                                               "his"
                                                               "her")))))))
                            (unless (malep user)
                              (push (format nil "~a struggles to hold it in while pressing her legs together before wetting her pants"
                                            (name-of user))
                                    a))
                            a)))))
            (random-elt j)))
  (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
    (format t "~a~%"
            (random-elt (list (format nil "~a: Bad ~a! No going potty in the house!~%~%*~a baps ~a's on the nose with a newspaper*"
                                      (name-of (player-of *game*))
                                      (name-of user)
                                      (name-of (player-of *game*))
                                      (name-of user))
                              "A puddle appears on the floor"
                              "There goes the carpet"
                              (format nil "~a: Heh, baby ~a made a puddle"
                                      (name-of (player-of *game*))
                                      (name-of user))
                              (format nil "~a's pants are ruined"
                                      (name-of user))
                              (format nil "~a: Heh, baby ~a wet ~a pants"
                                      (name-of (player-of *game*))
                                      (name-of user)
                                      (if (malep user) "his" "her"))
                              (format nil "~a: Bad ~a! Look what you did to your pants!"
                                      (name-of (player-of *game*))
                                      (name-of user)))))))
(defmethod output-process-potty-text ((user ally-silent-potty-training) (padding (eql nil)) (type (eql :wet)) (action (eql :had-accident)) had-accident)
  (format t "~a~%"
          (let ((j (cond ((<= (getf (car had-accident) :wet-amount) 10)
                          (list (format nil "*~a ghasp in horror as a little leaks out*" (name-of user))
                                (format nil "*~a's bladder just leaked a little*" (name-of user))))
                         ((and (<= (getf (car had-accident) :wet-amount) 300) (> (getf (car had-accident) :wet-amount) 10))
                          (list (apply #'format nil "*~a gasps in horror as ~a floods ~aself, but manages to stop ~aself*"
                                       (if (malep user)
                                           '("he" "him" "him")
                                           '("she" "her" "he")))))
                         ((> (getf (car had-accident) :wet-amount) 300)
                          (let ((a (list (format nil "*After doing a potty dance like a 5 year old, ~a freezes and pee ~aself*"
                                                 (name-of user)
                                                 (if (malep user)
                                                     "him"
                                                     "her"))
                                         (apply #'format nil "*~a Grabs ~a crotch, pauses and blushes as ~a flood ~aself like an infant*"
                                                (name-of user)
                                                (if (malep user)
                                                    '("his" "he" "him")
                                                    '("her" "she" "her")))
                                         (format nil "*~a cross ~a legs in a vain attempt to hold it in but fails miserably*"
                                                 (name-of user)
                                                 (if (malep user)
                                                     "his"
                                                     "her"))
                                         (apply #'format nil "~a gasps in embaressment as ~a floods ~aself like a toddler"
                                                (name-of user)
                                                (if (malep user)
                                                    '("he" "him")
                                                    '("she" "her")))
                                         (apply #'format nil "~a falls to ~a knees holding ~a crotch in a desparate attempt to keep from wetting ~aself but ends up wetting ~a pants anyway"
                                                (name-of user)
                                                (if (malep user)
                                                    '("his" "his" "him" "his")
                                                    '("her" "her" "her" "her"))))))
                            (unless (malep user)
                              (push
                               (format nil "~a struggles to hold it in while pressing her legs together until urine starts flowing down her legs"
                                       (name-of user))
                               a))
                            a)))))
            (random-elt j)))
  (when (and (car had-accident) (> (getf (car had-accident) :leak-amount) 0))
    (format t "~a~%"
            (random-elt (list (format nil "~a: Bad ~a! No going potty in the house!~%~%*~a baps ~a's on the nose with a newspaper*"
                                      (name-of (player-of *game*))
                                      (name-of user)
                                      (name-of (player-of *game*))
                                      (name-of user))
                              "A puddle appears on the floor"
                              "There goes the carpet"
                              (format nil "~a: Heh, baby ~a made a puddle"
                                      (name-of (player-of *game*))
                                      (name-of user)))))))
(defmethod output-process-potty-text ((user ally-silent-potty-training) padding (type (eql :mess)) (action (eql :potty-dance)) had-accident)
  (format t "~a~%"
          (random-elt (list (format nil "*~a is doing a potty dance like a 5 year old*" (name-of user))
                            (apply #'format nil "*~a crosses ~a legs in an attempt to avoid messing ~aself*"
                                   (name-of user)
                                   (if (malep user)
                                       '("his" "him")
                                       '("her" "her")))
                            (format nil "*~a is hopping from foot to foot holding the ~a*"
                                    (name-of user)
                                    (funcall (if (malep user)
                                                 #'car
                                                 #'cdr)
                                             (getf '(tabbed-briefs ("seat of his diapers" . "seat of her diapers")
                                                     pullon ("seat of his pullups" . "seat of her pullups")
                                                     closed-bottoms ("seat of his pants" . "seat of her pants")
                                                     nil ("back of himself" . "back of herself"))
                                                   padding)))))))
(defmethod output-process-potty-text ((user ally-silent-potty-training) padding (type (eql :mess)) (action (eql :desparate)) had-accident)
  (format t "~a~%"
          (random-elt (list (format nil "*~a is doing a potty dance like a 5 year old*" (name-of user))
                            (apply #'format nil "*~a crosses ~a legs in an attempt to avoid messing ~aself*"
                                   (name-of user)
                                   (if (malep user)
                                       '("his" "him")
                                       '("her" "her")))
                            (format nil "*~a is hopping from foot to foot holding the ~a*"
                                    (name-of user)
                                    (funcall
                                     (if (malep user)
                                         #'car
                                         #'cdr)
                                     (getf '(tabbed-briefs ("seat of his diapers" . "seat of her diapers")
                                             pullon ("seat of his pullups" . "seat of her pullups")
                                             closed-bottoms ("seat of his pants" . "seat of her pants")
                                             nil ("back of himself" . "back of herself"))
                                           padding)))))))
(defmethod output-process-potty-text ((user ally-silent-potty-training) (padding (eql 'tabbed-briefs)) (type (eql :mess)) (action (eql :had-accident)) had-accident)
  (format t "~{~a~}~%"
          (let ((a (list (random-elt (list (apply #'format nil "*~a instinctively squats down with ~a tail up and mess ~a diapers*"
                                                  (name-of user)
                                                  (if (malep user)
                                                      '("his" "his")
                                                      '("her" "her")))
                                           (apply #'format nil
                                                  "*The back of ~a's diaper expands as ~a accidentally messes ~aself*"
                                                  (name-of user)
                                                  (if (malep user)
                                                      '("he" "him")
                                                      '("she" "her")))
                                           (apply #'format nil "*~a instinctively squats down with ~a tail up and messes ~a diapers then holds the back of ~a diapers checking ~a load in embarrassment*~%~%"
                                                  (name-of user)
                                                  (if (malep user)
                                                      '("his" "his" "his" "his")
                                                      '("her" "her" "her" "her")))))))
                (b (random-elt (list (format nil "~%~%~a: Heh, baby ~a blorted ~a pamps."
                                             (name-of (player-of *game*))
                                             (name-of user)
                                             (if (malep user) "his" "her"))
                                     nil))))
            (when b (push b (cdr (last a))))))
  (when (and (cdr had-accident) (> (getf (cdr had-accident) :leak-amount) 0))
    (format t "*~a*~%"
            (random-elt (list (format nil "~a face turns red as ~a mess falls out the leg guards"
                                      (name-of user)
                                      (if (malep user)
                                          "his"
                                          "her"))
                              "Blowout!!!!")))))
(defmethod output-process-potty-text ((user ally-silent-potty-training) (padding (eql 'pullon)) (type (eql :mess)) (action (eql :had-accident)) had-accident)
  (format t "~{~a~}~%"
          (let ((a (list (random-elt (list (apply #'format nil "*~a instinctively squats down with ~a tail up and mess ~a pullups*"
                                                  (name-of user)
                                                  (if (malep user)
                                                      '("his" "his")
                                                      '("her" "her")))
                                           (apply #'format nil "*The back of ~a's pullups expands as ~a accidentally messes ~aself*"
                                                  (name-of user)
                                                  (if (malep user)
                                                      '("he" "him")
                                                      '("she" "her")))))))
                (b (random-elt (list (format nil "~%~%~a: Bad ~a!!! You know you're supposed to use the toilet like a big kid"
                                             (name-of (player-of *game*))
                                             (name-of user))
                                     nil))))
            (when b (push b (cdr (last a))))))
  (when (and (cdr had-accident) (> (getf (cdr had-accident) :leak-amount) 0))
    (format t "*~a*~%"
            (random-elt (list (format nil "~a face turns red as ~a mess falls out the leg guards"
                                      (name-of user)
                                      (if (malep user)
                                          "his"
                                          "her"))
                              (format nil "~a pullups leak all over the place" (name-of user)))))))
(defmethod output-process-potty-text ((user ally-silent-potty-training) (padding (eql 'closed-bottoms)) (type (eql :mess)) (action (eql :had-accident)) had-accident)
  (format t "*~a*~%"
          (random-elt (list (apply #'format nil "~a instinctively squats down with ~a tail up and messes ~a pants"
                                   (name-of user)
                                   (if (malep user)
                                       '("his" "his")
                                       '("her" "her")))
                            (apply #'format nil "a lump forms at the seat of ~a's pants"
                                   (name-of user)))))
  (when (and (cdr had-accident) (> (getf (cdr had-accident) :leak-amount) 0))
    (format t "~a~%"
            (random-elt (list (format nil "~a: Bad ~a! No going potty in the house!~%~%*~a baps ~a on the nose with a newspaper*"
                                      (name-of (player-of *game*))
                                      (name-of user)
                                      (name-of (player-of *game*))
                                      (name-of user))
                              (format nil "*~a's pants are ruined*" (name-of user))
                              (format nil "*~a makes a mess on the floor*" (name-of user))
                              (format nil "~a: Heh, baby ~a messed ~a pants"
                                      (name-of (player-of *game*))
                                      (name-of user)
                                      (if (malep user) "his" "her"))
                              (format nil "~a: Bad ~a! Look what you did to your pants!" (name-of (player-of *game*)) (name-of user)))))))
(defmethod output-process-potty-text ((user ally-silent-potty-training) (padding (eql nil)) (type (eql :mess)) (action (eql :had-accident)) had-accident)
  (format t "*~a*~%"
          (random-elt (list (format nil "Reaching the breaking point, ~a instinctively squats down with ~a tail up and messes"
                                    (name-of user)
                                    (if (malep user)
                                        "his"
                                        "her"))
                            (format nil "~a has an accident and makes a mess on the floor" (name-of user)))))
  (let ((a (random-elt (list (format nil "~a: Bad ~a! No going potty in the house!~%~%*~a baps ~a on the nose with a newspaper*"
                                     (name-of (player-of *game*))
                                     (name-of user)
                                     (name-of (player-of *game*))
                                     (name-of user))
                             nil))))
    (when a
      (format t "~a~%" a))))
(defunassert (process-potty (&optional (user (player-of *game*))))
    (user (or player ally))
  (let ((time-difference (- (time-of *game*) (last-process-potty-time-of user))))
    (incf (bladder/contents-of user) (* (bladder/fill-rate-of user) time-difference))
    (incf (bowels/contents-of user) (* (bowels/fill-rate-of user) time-difference)))
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
(defunassert (calculate-diaper-usage (user))
    (user base-character)
  (iter (with j = (list :sogginess 0 :sogginess-capacity 0 :messiness 0 :messiness-capacity 0))
    (for i in (wear-of user))
    (when (typep i 'closed-bottoms)
      (incf (getf j :sogginess) (sogginess-of i))
      (incf (getf j :sogginess-capacity) (sogginess-capacity-of i))
      (incf (getf j :messiness) (messiness-of i))
      (incf (getf j :messiness-capacity) (messiness-capacity-of i)))
    (finally (return j))))
(defunassert (calculate-diaper-usage* (clothes))
    (clothes list)
  (iter (with j = (list :sogginess 0 :sogginess-capacity 0 :messiness 0 :messiness-capacity 0))
    (for i in clothes)
    (when (typep i 'closed-bottoms)
      (incf (getf j :sogginess) (sogginess-of i))
      (incf (getf j :sogginess-capacity) (sogginess-capacity-of i))
      (incf (getf j :messiness) (messiness-of i))
      (incf (getf j :messiness-capacity) (messiness-capacity-of i)))
    (finally (return j))))
(defunassert (calculate-level-to-exp (level))
    (level number)
  (floor (/ (* 4 (expt level 3)) 5)))
(defunassert (calculate-exp-yield (target))
    (target enemy)
  ($ (exp-yield-of target) * (level-of target) / 7))
(defunassert (calculate-wear-stats (user))
    (user base-character)
  (iter
    (with j = (list :health 0 :attack 0 :defense 0 :energy 0 :speed 0))
    (for i in (wear-of user))
    (iter
      (for (a b) on (wear-stats-of i) by #'cddr)
      (incf (getf j a) b))
    (finally (return j))))
(defunassert (calculate-wield-stats (user))
    (user base-character)
  (iter
    (with j = (list :health 0 :attack 0 :defense 0 :energy 0 :speed 0))
    (for (a b) on (if (wield-of user) (wield-stats-of (wield-of user)) ()) by #'cddr)
    (incf (getf j a) b)
    (finally (return j))))
(defunassert (calculate-stat-delta (user))
    (user base-character)
  (iter
    (with j = (list :health 0 :attack 0 :defense 0 :energy 0 :speed 0))
    (for i in (when *battle* (getf (status-conditions-of *battle*) user)))
    (iter
      (for (a b) on (stat-delta-of i) by #'cddr)
      (incf (getf j a) b))
    (finally (return j))))
(defunassert (calculate-stat-multiplier (user))
    (user base-character)
  (iter
    (with j = (list :health 1 :attack 1 :defense 1 :energy 1 :speed 1))
    (for i in (when *battle* (getf (status-conditions-of *battle*) user)))
    (iter
      (for (a b) on (stat-multiplier-of i) by #'cddr)
      (declare (ignorable b))
      (setf (getf j a) (* (getf j a))))
    (finally (return j))))
(defunassert (calculate-stat (user stat-key))
    (user base-character)
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

(defunassert (calculate-damage (target user attack-base))
    (user base-character
          target base-character
          attack-base number)
  (round ($ ($ ($ ($ ($ ($ ($ 2 * (level-of user)) / 5) + 2) * attack-base * ($ (calculate-stat user :attack) / (calculate-stat target :defense)))
                  / 50)
               + 2)
            * ($ (random-from-range 85 100) / 100))))
(defmacro draw-bar (stat &rest colors)
  `(if clim-listener::*application-frame*
       (multiple-value-bind (x y) (clim:stream-cursor-position *standard-output*)
         (clim:draw-rectangle* *standard-output* x y (+ x (* ,stat 400)) (+ y 15)
                               :ink (cond ,@(iter (for i in colors)
                                              (collect `(,(car i) ,(intern (format nil "+~a+"
                                                                                   (if (typep (car (last i)) 'cons)
                                                                                       (caar (last i))
                                                                                       (car (last i))))
                                                                           "CLIM"))))))
         (clim:draw-rectangle* *standard-output* x y (+ x 400) (+ y 15)
                               :filled nil)
         (clim:stream-set-cursor-position *standard-output* (+ x 400) y))
       (cl-ansi-text:with-color ((cond ,@(iter (for i in colors)
                                           (collect `(,(car i)
                                                      ,(if (typep (car (last i)) 'cons)
                                                           (cadar (last i))
                                                           (car (last i))))))))
         (color-format (cond ,@(iter (for i in colors)
                                 (collect (let ((col (last i)))
                                            (if (typep (car col) 'cons)
                                                (append (butlast i)
                                                        (cdar col))
                                                i)))))
                       "[~{~a~}]"
                       (iter (for i from 0 below 40)
                         (collect (if (< i (* ,stat 40)) "#" " ")))))))
(defun format-stats (user)
  (format t "Name: ~a~%" (name-of user))
  (format t "Species: ~a~%" (species-of user))
  (format t "Description: ~a~%" (description-of user))
  (format t "Health: ")
  (draw-bar (/ (health-of user) (calculate-stat user :health))
            ((> (health-of user) (* (calculate-stat user :health) 1/2)) :green)
            ((> (health-of user) (* (calculate-stat user :health) 1/4)) :yellow)
            (t :red))
  (format t " ~a~%Energy: "
          (if (member user (cons (player-of *game*) (allies-of *game*)))
              (format nil "(~a/~a)"
                      (health-of user)
                      (calculate-stat user :health))
              ""))
  (draw-bar (/ (energy-of user) (calculate-stat user :energy))
            ((> (energy-of user) (* (calculate-stat user :energy) 1/2)) :green)
            ((> (energy-of user) (* (calculate-stat user :energy) 1/4)) :yellow)
            (t :red))
  (format t " ~a~%"
          (if (member user (cons (player-of *game*) (allies-of *game*)))
              (format nil "(~a/~a)"
                      (energy-of user)
                      (calculate-stat user :energy))
              ""))
  (when *battle*
    (write-string "Conditions: ")
    (iter (for i in (getf (status-conditions-of *battle*) user))
      (format t "`~a' " (name-of i)))
    (write-char #\Newline))
  (format t "Stats: ~a~%Base-Stats: ~a~%"
          (let ((wield-stats (calculate-wield-stats user))
                (wear-stats (calculate-wear-stats user)))
            (iter (for (a b) on (base-stats-of user) by #'cddr)
              (collect a)
              (collect (+ b (getf wield-stats a) (getf wear-stats a)))))
          (base-stats-of user))
  (let* ((c (filter-items (wear-of user) 'closed-bottoms))
         (b (calculate-diaper-usage* c)))
    (cond ((find '(or tabbed-briefs pullon incontinence-pad) c :test (lambda (o e) (typep e o)))
           (format t "Diaper State: ~{~a~}~%"
                   (let ((a ()))
                     (cond ((>= (getf b :sogginess) (getf b :sogginess-capacity))
                            (push "Leaking" a)
                            (push " " a))
                           ((>= (getf b :sogginess) 100)
                            (push "Soggy" a)
                            (push " " a)))
                     (cond ((>= (getf b :messiness) (getf b :messiness-capacity))
                            (push "Blowout" a))
                           ((>= (getf b :messiness) 100)
                            (push "Messy" a)))
                     (unless a (push "Clean" a))
                     a)))
          (c
           (format t "Pants State: ~{~a~}~%"
                   (let ((a ()))
                     (cond ((>= (getf b :sogginess) (getf b :sogginess-capacity))
                            (push "Wet" a)
                            (push " " a)))
                     (cond ((>= (getf b :messiness) (getf b :messiness-capacity))
                            (push "Messy" a)))
                     (unless a (push "Clean" a))
                     a))))
    (when c
      (write-string "Sogginess: ")
      (draw-bar (/ (getf b :sogginess) (getf b :sogginess-capacity))
                ((>= (getf b :sogginess) (- (getf b :sogginess-capacity) (/ (getf b :sogginess-capacity) 4))) :red)
                ((>= (getf b :sogginess) (/ (getf b :sogginess-capacity) 2)) :yellow)
                (t :green))
      (terpri)
      (write-string "Messiness: ")
      (draw-bar (/ (getf b :messiness) (getf b :messiness-capacity))
                ((>= (getf b :messiness) (- (getf b :messiness-capacity) (/ (getf b :messiness-capacity) 4))) :red)
                ((>= (getf b :messiness) (/ (getf b :messiness-capacity) 2)) :yellow)
                (t :green))
      (terpri)))
  (write-string "Bladder State: ")
  (draw-bar (/ (bladder/contents-of user) (bladder/maximum-limit-of user))
            ((>= (bladder/contents-of user) (bladder/potty-desperate-limit-of user)) :red)
            ((>= (bladder/contents-of user) (bladder/potty-dance-limit-of user)) (:orange :red))
            ((>= (bladder/contents-of user) (bladder/need-to-potty-limit-of user)) :yellow)
            (t :green))
  (terpri)
  (write-string "Bowels State: ")
  (draw-bar (/ (bowels/contents-of user) (bowels/maximum-limit-of user))
            ((>= (bowels/contents-of user) (bowels/potty-desperate-limit-of user)) :red)
            ((>= (bowels/contents-of user) (bowels/potty-dance-limit-of user)) (:orange :red))
            ((>= (bowels/contents-of user) (bowels/need-to-potty-limit-of user)) :yellow)
            (t :green))
  (terpri))
(defun present-stats (user)
  (cond #+(or slynk swank) ((not clim-listener::*application-frame*)
                            (format-stats user))
        (clim-listener::*application-frame* (push (clim:updating-output (clim-listener::*standard-output*)
                                                    (clim:with-output-as-presentation (t user 'yadfa-class :single-box t)
                                                      (format-stats user)))
                                                  *records*))))

(defun finish-battle (&optional lose)
  (if lose
      (progn (format t "~a was defeated~%" (name-of (player-of *game*)))
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
             (iter (for i in (cons (player-of *game*) (allies-of *game*)))
               (setf (health-of i) (calculate-stat i :health))
               (setf (energy-of i) (calculate-stat i :energy))
               (let ((exp-gained (/ (iter (for i in (enemies-of *battle*))
                                      (with j = 0)
                                      (incf j (calculate-exp-yield i))
                                      (finally (return j)))
                                    2)))
                 (iter (for k in (team-of *game*))
                   (incf (exp-of k) exp-gained)
                   (let ((old-level (level-of k)))
                     (iter (while (>= (exp-of k) (calculate-level-to-exp (+ (level-of k) 1))))
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
             (iter (for i in (team-of *game*))
               (wet :force-fill-amount (bladder/maximum-limit-of i))
               (mess :force-fill-amount (bowels/maximum-limit-of i))))
      (progn (format t "~a won the battle~%~%" (name-of (player-of *game*)))
             (let ((items-looted (iter (for i in (enemies-of *battle*))
                                   (with j = ())
                                   (setf j (append j (inventory-of i) (wear-of i)))
                                   (finally (return j))))
                   (bitcoins-looted (iter (for i in (enemies-of *battle*))
                                      (with j = 0)
                                      (incf j (if (bitcoins-of i) (bitcoins-of i) (* (bitcoins-per-level-of i) (level-of i))))
                                      (finally (return j))))
                   (exp-gained (iter (for i in (enemies-of *battle*))
                                 (with j = 0)
                                 (incf j (calculate-exp-yield i))
                                 (finally (return j))))
                   (win-events (win-events-of *battle*)))
               (iter (for k in (team-of *game*))
                 (incf (exp-of k) exp-gained)
                 (let ((old-level (level-of k)))
                   (iter (while (>= (exp-of k) (calculate-level-to-exp (+ (level-of k) 1))))
                     (incf (level-of k)))
                   (when (> (level-of k) old-level)
                     (format t "~a level-uped to ~d~%" (name-of k) (level-of k))
                     (iter (for i from (1+ old-level) to (level-of k))
                       (iter (for j in (learned-moves-of k))
                         (when (= (car j) i)
                           (unless (get-move (cdr j) k)
                             (pushnewmove (cdr j) k)
                             (format t "~a learned ~a~%" (name-of k) (name-of (get-move (cdr j) k))))))))))
               (cond ((and items-looted (> bitcoins-looted 0))
                      (format t "~a loots ~d bitcoins and ~d ~a from the enemies~%"
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
                 (when (or (not (member i (finished-events-of *game*)))
                           (event-repeatable (get-event i)))
                   (funcall (coerce (event-lambda (get-event i)) 'function) i)
                   (pushnew i (finished-events-of *game*))))
               (when *battle* (return-from finish-battle)))))
  (unuse-package :yadfa-battle :yadfa-user)
  (use-package :yadfa-world :yadfa-user))
(defun wash (clothing)
  (loop for i in (filter-items clothing 'bottoms) do (when (not (disposablep i)) (setf (sogginess-of i) 0 (messiness-of i) 0))))
(defun go-to-sleep% (user)
  (incf (time-of *game*) 60)
  (let ((time-difference (- (time-of *game*) (last-process-potty-time-of user))))
    (incf (bladder/contents-of user) (* (bladder/fill-rate-of user) time-difference))
    (incf (bowels/contents-of user) (* (bowels/fill-rate-of user) time-difference)))
  (setf (health-of user) (calculate-stat user :health)
        (last-process-potty-time-of user) (time-of *game*)
        (energy-of user) (calculate-stat user :energy))
  (cons (wet :wetter user) (mess :messer user)))
(defun go-to-sleep ()
  (iter (for i in (cons (player-of *game*) (allies-of *game*)))
    (let ((return-value (go-to-sleep% i))
          (out ()))
      (multiple-value-bind (value key)
          (pop-from-expansion i return-value)
        (when (eq key :wet/mess)
          (setf return-value value)))
      (format t "~a wakes up " (name-of i))
      (when (> (getf (car return-value) :wet-amount) 0)
        (cond ((filter-items (wear-of i) 'tabbed-briefs)
               (if (> (getf (car return-value) :leak-amount) 0)
                   (progn (push (format nil "feeling all cold and soggy. ~a checks ~a diaper and to ~a embarrassment finds out it's leaking profusely. Seems ~a wet the bed.~%"
                                        (if (malep i) "He" "She")
                                        (if (malep i) "his" "her")
                                        (if (malep i) "his" "her")
                                        (name-of i))
                                out)
                          (format t "~a" (random-elt out))
                          (setf out ()))
                   (progn (push (format nil "and hears a squish . ~a looks down at ~a diaper, notices that it's soggy and then folds ~a ears back and blushes. Looks like ~a wet the bed~%"
                                        (if (malep i) "He" "She")
                                        (if (malep i) "his" "her")
                                        (if (malep i) "his" "her")
                                        (name-of i))
                                out)
                          (push (format nil "and looks down and pokes ~a diaper, then gets all blushy when it squishes. Seems ~a wet the bed~%"
                                        (if (malep i) "his" "her")
                                        (name-of i))
                                out)
                          (format t "~a" (random-elt out))
                          (setf out ()))))
              ((filter-items (wear-of i) 'pullon)
               (if (> (getf (car return-value) :leak-amount) 0)
                   (progn (push (format nil "feeling all cold and soggy. ~a checks ~a pullups and to ~a embarrassment finds out it's leaking profusely. Seems ~a wet the bed.~%"
                                        (if (malep i) "He" "She")
                                        (if (malep i) "his" "her")
                                        (if (malep i) "his" "her")
                                        (name-of i))
                                out)
                          (format t "~a" (random-elt out))
                          (setf out ()))
                   (progn (push (format nil "and hears a squish. ~a looks down at ~a pullups, notices that ~a and then folds ~a ears back and blushes. Looks like ~a wet the bed~%"
                                        (if (malep i) "He" "She")
                                        (if (filter-items (wear-of i) 'pullup)
                                            "the little pictures have faded"
                                            "it's soggy")
                                        (if (malep i) "his" "her")
                                        (if (malep i) "his" "her")
                                        (name-of i))
                                out)
                          (format t "~a" (random-elt out))
                          (setf out ()))))
              ((filter-items (wear-of i) 'incontinence-pad)
               (if (> (getf (car return-value) :leak-amount) 0)
                   (progn (push (format nil "feeling all cold and soggy. ~a notices ~a PJs, the padding under ~a PJs, and bed are soaked. Seems ~a wet the bed~%"
                                        (if (malep i) "He" "She")
                                        (if (malep i) "his" "her")
                                        (if (malep i) "his" "her")
                                        (name-of i))
                                out)
                          (format t "~a" (random-elt out))
                          (setf out ()))
                   (progn (push (format nil "and hears a squish from under ~a PJs. ~a checks the incontinence pad under them and notices that they're soaked and then folds ~a ears back and blushes. Looks like ~a wet the bed~%"
                                        (if (malep i) "his" "her")
                                        (if (malep i) "He" "She")
                                        (if (malep i) "his" "her")
                                        (name-of i))
                                out)
                          (format t "~a" (random-elt out))
                          (setf out ()))))
              ((filter-items (wear-of i) 'closed-bottoms)
               (push (format nil "feeling all cold and soggy. ~a notices ~a PJs and bed are soaked then folds ~a ears back and blushes. Seems ~a wet the bed~%"
                             (if (malep i) "He" "She")
                             (if (malep i) "his" "her")
                             (if (malep i) "his" "her")
                             (name-of i))
                     out)
               (format t "~a" (random-elt out))
               (setf out ()))
              (t
               (push (format nil "feeling all cold and soggy. ~a notices the bed is soaked then folds ~a ears back and blushes. Seems ~a wet the bed~%"
                             (if (malep i) "He" "She")
                             (if (malep i) "his" "her")
                             (name-of i))
                     out)
               (format t "~a" (random-elt out))
               (setf out ()))))
      (when (and (> (getf (cdr return-value) :mess-amount) 0) (> (getf (car return-value) :wet-amount) 0))
        (format t "~a is also " (name-of i)))
      (when (> (getf (cdr return-value) :mess-amount) 0)
        (cond
          ((filter-items (wear-of i) 'tabbed-briefs)
           (if (> (getf (cdr return-value) :leak-amount) 0)
               (progn
                 (push (format nil
                               "feeling all mushy. ~a notices to ~a embarrassment that ~a diaper is leaking poo all over the bed. Seems ~a messed the bed~%"
                               (if (malep i) "He" "She")
                               (if (malep i) "his" "her")
                               (if (malep i) "his" "her")
                               (name-of i))
                       out)
                 (format t "~a" (random-elt out))
                 (setf out ()))
               (progn
                 (push (format nil
                               "feeling all mushy. ~a notices to ~a embarrassment that ~a diaper is filled with poo. Seems ~a messed the bed~%"
                               (if (malep i) "He" "She")
                               (if (malep i) "his" "her")
                               (if (malep i) "his" "her")
                               (name-of i))
                       out)
                 (format t "~a" (random-elt out))
                 (setf out ()))))
          ((filter-items (wear-of i) 'pullon)
           (if (> (getf (cdr return-value) :leak-amount) 0)
               (progn
                 (push (format nil
                               "feeling all mushy. ~a notices to ~a embarrassment that ~a pullups is leaking poo all over the bed. Seems ~a messed the bed~%"
                               (if (malep i) "He" "She")
                               (if (malep i) "his" "her")
                               (if (malep i) "his" "her")
                               (name-of i))
                       out)
                 (format t "~a" (random-elt out))
                 (setf out ()))
               (progn
                 (push (format nil
                               "feeling all mushy. ~a notices to ~a embarrassment that ~a pullup is filled with poo. Seems ~a messed the bed~%"
                               (if (malep i) "He" "She")
                               (if (malep i) "his" "her")
                               (if (malep i) "his" "her")
                               (name-of i))
                       out)
                 (format t "~a" (random-elt out))
                 (setf out ()))))
          ((filter-items (wear-of i) 'incontinence-pad)
           (if (> (getf (cdr return-value) :leak-amount) 0)
               (progn
                 (push (format nil
                               "feeling all mushy. ~a notices to ~a embarrassment that ~a incontinence pad is leaking poo all over the bed and PJs. Seems ~a messed the bed~%"
                               (if (malep i) "He" "She")
                               (if (malep i) "his" "her")
                               (if (malep i) "his" "her")
                               (name-of i))
                       out)
                 (format t "~a" (random-elt out))
                 (setf out ()))
               (progn
                 (push (format nil
                               "feeling all mushy. ~a notices to ~a embarrassment that ~a incontinence pad is filled with poo. Seems ~a messed the bed~%"
                               (if (malep i) "He" "She")
                               (if (malep i) "his" "her")
                               (if (malep i) "his" "her")
                               (name-of i))
                       out)
                 (format t "~a" (random-elt out))
                 (setf out ()))))
          ((filter-items (wear-of i) 'closed-bottoms)
           (push (format nil
                         "feeling all mushy. ~a notices to ~a embarrassment that ~a PJs have poo in them and is getting on the bed. Seems ~a messed the bed~%"
                         (if (malep i) "He" "She")
                         (if (malep i) "his" "her")
                         (if (malep i) "his" "her")
                         (name-of i))
                 out)
           (format t "~a" (random-elt out))
           (setf out ()))
          (t
           (push (format nil
                         "feeling all mushy. ~a notices to ~a embarrassment that ~a bed has poo on it. Seems ~a messed the bed~%"
                         (if (malep i) "He" "She")
                         (if (malep i) "his" "her")
                         (if (malep i) "his" "her")
                         (name-of i))
                 out)
           (format t "~a" (random-elt out))
           (setf out ())))))))
(defunassert (shopfun (items-for-sale &key items-to-buy items-to-sell user format-items))
    (user (or base-character null)
          items-to-buy (or list null)
          items-to-sell (or list null)
          items-for-sale list)
  (when items-to-buy
    (iter (for i in items-to-buy)
      (let ((item (when (< (car i) (list-length items-for-sale))
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
              (t (iter (for j from 1 to (cdr i))
                   (push (apply #'make-instance
                                (car (nth (car i) items-for-sale))
                                (eval (cdr (nth (car i) items-for-sale))))
                         (inventory-of user)))
                 (decf (bitcoins-of user) (* (value-of item) (cdr i)))
                 (format t "You buy ~d ~a for ~f bitcoins~%"
                         (cdr i)
                         (if (plural-name-of item)
                             (plural-name-of item)
                             (format nil "~as" (name-of item)))
                         (* (value-of item) (cdr i))))))))
  (when items-to-sell
    (let ((items (sort (remove-duplicates items-to-sell) #'<)))
      (setf items (iter (generate i in items)
                    (for j in (inventory-of user))
                    (for k upfrom 0)
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
      (removef (inventory-of user) items
               :test (lambda (o e)
                       (member e o)))))
  (when format-items
    (format t "~10a~40a~10@a~%" "Index" "Item" "Price")
    (iter (for i in items-for-sale)
      (for j from 0)
      (let ((item (apply #'make-instance (car i) (eval (cdr i)))))
        (format t "~10a~40a~10@a~%" j (name-of item) (value-of item))))))
(defun getf-action-from-prop (position prop action)
  (getf (actions-of (getf (get-props-from-zone position) prop)) action))
(defun (setf getf-action-from-prop) (new-value position prop action)
  (setf (getf (actions-of (getf (get-props-from-zone position) prop)) action) new-value))
(defunassert (wash-in-washer (washer)
                             "washes your dirty diapers and all the clothes you've ruined. WASHER is an instance of a washer you want to put your clothes in."
                             (declare (ignorable washer)))
    (washer (or washer null))
  (wash (inventory-of (player-of *game*)))
  (write-line "You washed all your soggy and messy clothing. Try not to wet and mess them next time"))
(defmethod process-battle-turn ((character enemy) attack item reload selected-target)
  (iter (for i in (getf (status-conditions-of *battle*) character))
    (when (or (eq (duration-of i) t) (> (duration-of i) 0))
      (funcall (coerce (battle-script-of i) 'function) (target-of i) character i)
      (when (typep (duration-of i) 'number)
        (decf (duration-of i))))
    (removef-if (getf (status-conditions-of *battle*) character)
                (lambda (a) (and (not (eq a t)) (<= a 0)))
                :key #'duration-of))
  (run-equip-effects character)
  (when (<= (health-of character) 0)
    (unless (member character (fainted-of *battle*))
      (format t "~a has fainted~%~%" (name-of character))
      (pushnew character (fainted-of *battle*)))
    (setf (health-of character) 0)
    (removef (turn-queue-of *battle*) character)
    (return-from process-battle-turn))
  (when (> (health-of character) (calculate-stat character :health))
    (setf (health-of character) (calculate-stat character :health)))
  (when (> (energy-of character) (calculate-stat character :energy))
    (setf (energy-of character) (calculate-stat character :energy)))
  (let ((time-passed (- (time-of *game*) (last-process-potty-time-of character))))
    (incf (bladder/contents-of character) (* (bladder/fill-rate-of character) time-passed))
    (incf (bowels/contents-of character) (* (bowels/fill-rate-of character) time-passed)))
  (setf (last-process-potty-time-of character) (time-of *game*))
  (cond ((funcall (coerce (process-battle-accident-of character) 'function) character attack item reload selected-target)
         nil)
        ((iter (for j in (getf (status-conditions-of *battle*) character))
           (when (blocks-turn-of j)
             (leave t))))
        ((and (or (>= (bladder/contents-of character) (bladder/potty-dance-limit-of character))
                  (>= (bowels/contents-of character) (bowels/potty-dance-limit-of character)))
              (< (car (sort (let* ((a ())
                                   (x (- (bladder/maximum-limit-of character) (bladder/contents-of character)))
                                   (y (- (bladder/maximum-limit-of character) (bladder/potty-dance-limit-of character))))
                              (when (>= (bladder/contents-of character) (bladder/potty-dance-limit-of character))
                                (push (random (expt ($ x / y * (expt 5 1.3)) (/ 1 1.3))) a))
                              (setf x (- (bowels/maximum-limit-of character) (bowels/contents-of character))
                                    y (- (bowels/maximum-limit-of character) (bowels/potty-dance-limit-of character)))
                              (when (>= (bowels/contents-of character) (bowels/potty-dance-limit-of character))
                                (push (random (expt ($ x / y * (expt 5 2)) 0.5)) a))
                              a)
                            '<))
                 1))
         (format t "~a is too busy doing a potty dance to fight~%" (name-of character)))
        ((and (wield-of character)
              (ammo-type-of (wield-of character))
              (<= (list-length (ammo-of (wield-of character))) 0)
              (> (ammo-capacity-of (wield-of character)) 0)
              (ammo-type-of (wield-of character))
              (iter (for i in (inventory-of character))
                (when (typep i (ammo-type-of (wield-of character)))
                  (leave t))))
         (format t "~a reloaded ~a ~a"
                 (name-of character)
                 (if (malep character)
                     "his"
                     "her")
                 (name-of (wield-of character)))
         (iter (with count = 0)
           (for item in (inventory-of character))
           (when (or (>= (list-length (ammo-of (wield-of character))) (ammo-capacity-of (wield-of character)))
                     (and (reload-count-of (wield-of character)) (>= count (reload-count-of (wield-of character)))))
             (leave t))
           (when (typep item (ammo-type-of (wield-of character)))
             (incf count)
             (push item (ammo-of (wield-of character)))
             (removef item (inventory-of character) :count 1))))
        (t
         (funcall (coerce (battle-script-of character) 'function) character (random-elt (team-of *game*))))))
(defmethod process-battle-turn ((character team-member) attack item reload selected-target)
  (iter (for i in (getf (status-conditions-of *battle*) character))
    (when (or (eq (duration-of i) t) (> (duration-of i) 0))
      (funcall (coerce (battle-script-of i) 'function) (target-of i) character i)
      (when (typep (duration-of i) 'number)
        (decf (duration-of i))))
    (removef-if (getf (status-conditions-of *battle*) character)
                (lambda (a) (and (not (eq a t)) (<= a 0)))
                :key #'duration-of))
  (run-equip-effects character)
  (when (<= (health-of character) 0)
    (setf (health-of character) 0)
    (unless (member character (fainted-of *battle*))
      (format t "~a has fainted~%~%" (name-of character))
      (pushnew character (fainted-of *battle*)))
    (removef (turn-queue-of *battle*) character)
    (return-from process-battle-turn))
  (when (> (health-of character) (calculate-stat character :health))
    (setf (health-of character) (calculate-stat character :health)))
  (when (> (energy-of character) (calculate-stat character :energy))
    (setf (energy-of character) (calculate-stat character :energy)))
  (let ((time-passed (- (time-of *game*) (last-process-potty-time-of character))))
    (incf (bladder/contents-of character) (* (bladder/fill-rate-of character) time-passed))
    (incf (bowels/contents-of character) (* (bowels/fill-rate-of character) time-passed)))
  (setf (last-process-potty-time-of character) (time-of *game*))
  (cond ((funcall (coerce (process-battle-accident-of character) 'function) character attack item reload selected-target)
         nil)
        ((iter (for j in (getf (status-conditions-of *battle*) character))
           (when (blocks-turn-of j)
             (leave t))))
        ((and (or
               (>= (bladder/contents-of character) (bladder/potty-dance-limit-of character))
               (>= (bowels/contents-of character) (bowels/potty-dance-limit-of character)))
              (< (car (sort (let ((a ())
                                  (x (- (bladder/maximum-limit-of character) (bladder/contents-of character)))
                                  (y (- (bladder/maximum-limit-of character) (bladder/potty-dance-limit-of character))))
                              (when (>= (bladder/contents-of character) (bladder/potty-dance-limit-of character))
                                (push (random (expt ($ x / y * (expt 5 1.3)) (/ 1 1.3))) a))
                              (setf x (- (bowels/maximum-limit-of character) (bowels/contents-of character))
                                    y (- (bowels/maximum-limit-of character) (bowels/potty-dance-limit-of character)))
                              (when (>= (bowels/contents-of character) (bowels/potty-dance-limit-of character))
                                (push (random (expt ($ x / y * (expt 5 2)) 0.5)) a))
                              a)
                            '<))
                 1)
              (not (or (typep (get-move attack character) 'yadfa-moves:watersport) (typep (get-move attack character) 'yadfa-moves:mudsport))))
         (format t "~a is too busy doing a potty dance to fight~%" (name-of character)))
        (item
         (format t "~a used ~a ~a on ~a~%"
                 (name-of character)
                 (if (malep character) "his" "her")
                 (name-of (nth item (inventory-of (player-of *game*))))
                 (name-of selected-target))
         (use-item% (nth item (inventory-of (player-of *game*))) (player-of *game*)
                    :target selected-target))
        (reload (format t "~a reloaded ~a ~a"
                        (name-of character)
                        (if (malep character)
                            "his"
                            "her")
                        (name-of (wield-of character)))
                (iter (with count = 0)
                  (for item in (inventory-of (player-of *game*)))
                  (when (or
                         (>=
                          (list-length (ammo-of (wield-of character)))
                          (ammo-capacity-of (wield-of character)))
                         (and
                          (reload-count-of (wield-of character))
                          (>=
                           count
                           (reload-count-of (wield-of character)))))
                    (leave t))
                  (when (and (typep item reload) (typep item (ammo-type-of (wield-of character))))
                    (incf count)
                    (push item (ammo-of (wield-of character)))
                    (removef item (inventory-of (player-of *game*)) :count 1))))
        ((eq attack t)
         (if (wield-of character)
             (progn (funcall (coerce (attack-script-of (wield-of character)) 'function) selected-target character (wield-of character))
                    (when (ammo-of (wield-of character))
                      (pop (ammo-of (wield-of character)))))
             (funcall (coerce (default-attack-of character) 'function) selected-target character)))
        (attack
         (funcall (coerce (attack-of (get-move attack character)) 'function) selected-target character (get-move attack character)))))
(defunassert (process-battle (&key attack item reload target friendly-target no-team-attack selected-target))
    (target (or null integer)
            attack (or symbol boolean)
            reload (or list (and symbol (not keyword))))
  (when (and (not attack) (not item))
    (write-line "You need to either specify an attack or an item to use")
    (return-from process-battle))
  (let* ((selected-target (cond (selected-target selected-target)
                                ((and target (>= target (list-length (enemies-of *battle*))))
                                 (write-line "That target doesn't exist")
                                 (return-from process-battle))
                                ((and friendly-target (>= friendly-target (list-length (team-of *game*))))
                                 (write-line "That target doesn't exist")
                                 (return-from process-battle))
                                (target (nth target (enemies-of *battle*)))
                                (friendly-target (nth friendly-target (team-of *game*)))
                                (t (iter (for i in (enemies-of *battle*))
                                     (when (>= (health-of i) 0)
                                       (leave i))))))
         (ret nil)
         (team-attacked no-team-attack))
    (flet ((check-if-done ()
             (iter (for i in (append (enemies-of *battle*) (team-of *game*)))
               (if (<= (health-of i) 0)
                   (progn (setf (health-of i) 0)
                          (unless (member i (fainted-of *battle*))
                            (format t "~a has fainted~%~%" (name-of i))
                            (pushnew i (fainted-of *battle*)))
                          (removef (turn-queue-of *battle*) i))
                   (removef (fainted-of *battle*) i :count 1))
               (when (> (health-of i) (calculate-stat i :health))
                 (setf (health-of i) (calculate-stat i :health)))
               (when (> (energy-of i) (calculate-stat i :energy))
                 (setf (energy-of i) (calculate-stat i :energy))))
             (unless (iter (for i in (team-of *game*)) (when (> (health-of i) 0) (leave t)))
               (finish-battle t)
               (return-from process-battle t))
             (unless (iter (for i in (enemies-of *battle*)) (when (> (health-of i) 0) (leave t)))
               (finish-battle)
               (return-from process-battle t))))
      (check-if-done)
      (unless (or (eq attack t) (get-move attack (first (turn-queue-of *battle*))))
        (format t "~a doesn't know ~a~%" (name-of (first (turn-queue-of *battle*))) attack)
        (return-from process-battle))
      (iter (until (and team-attacked (typep (first (turn-queue-of *battle*)) 'team-member)))
        (check-if-done)
        (let* ((current-character (pop (turn-queue-of *battle*)))
               (new-ret (process-battle-turn current-character attack item reload selected-target)))
          (pop-from-expansion current-character)
          (when (typep current-character 'team-member)
            (setf team-attacked t
                  ret new-ret)))
        (check-if-done)
        (unless (turn-queue-of *battle*)
          (incf (time-of *game*) 5)
          (appendf (turn-queue-of *battle*)
                   (sort (iter (for i in (append (enemies-of *battle*) (team-of *game*)))
                           (when (> (health-of i) 0)
                             (collect i)))
                         '>
                         :key #'(lambda (a) (calculate-stat a :speed))))))
      (format t "~a is next in battle~%" (name-of (first (turn-queue-of *battle*))))
      ret)))
(defun ally-join (ally)
  (format t "~a Joins the team~%" (name-of ally))
  (when (> (bitcoins-of ally) 0)
    (format t "~a gets ~f bitcoins from ~a~%" (name-of (player-of *game*)) (bitcoins-of ally) (name-of ally)))
  (when (inventory-of ally)
    (format t "~a gets some loot from ~a~%" (name-of (player-of *game*)) (name-of ally))
    (pushnew ally (allies-of *game*)))
  (incf (bitcoins-of (player-of *game*)) (bitcoins-of ally))
  (setf (inventory-of (player-of *game*)) (append (inventory-of (player-of *game*)) (inventory-of ally))
        (inventory-of ally) ()
        (bitcoins-of ally) 0))
(defun use-item% (item user &rest keys &key target action &allow-other-keys)
  (let ((script (if action (action-lambda (getf (special-actions-of item) action)) (use-script-of item)))
        (ret nil))
    (unless (apply (coerce (cant-use-predicate-of item) 'function) item user keys)
      (if script
          (progn (setf ret (apply (coerce script 'function) item target (when action keys)))
                 (when (consumablep item)
                   (removef (inventory-of user) item)))
          (write-line "You can't do that with that item")))
    (when (> (health-of target) (calculate-stat target :health))
      (setf (health-of target) (calculate-stat target :health)))
    (when (> (energy-of target) (calculate-stat target :energy))
      (setf (energy-of target) (calculate-stat target :energy)))
    ret))

(defunassert (set-player (name malep species)
                         "Sets the name, gender, and species of the player")
    (malep boolean
           name simple-string
           species simple-string)
  (setf (name-of (player-of *game*)) name)
  (setf (species-of (player-of *game*)) species)
  (setf (malep (player-of *game*)) malep))
(defun intro-function (query-io)
  "This function sets up the player and prints the back story. If you're trying to create your own game with a different storyline using a mod, you can replace this function. Be careful when enabling mods that change the story line this significantly as they can overwrite each other"
  (setf (clim:stream-end-of-line-action query-io) :wrap*)
  (write-line "Enter your character's name, gender, and species" query-io)
  (let* ((default (make-instance 'player))
         (values (prompt-for-values (string
                                     :prompt "Name"
                                     :default (name-of default) :view clim:+text-field-view+)
                                    (boolean
                                     :prompt "Is Male"
                                     :default (malep default) :view clim:+toggle-button-view+)
                                    (string
                                     :prompt "Species"
                                     :default (species-of default) :view clim:+text-field-view+)
                                    ((clim:completion (yadfa-items:tshirt yadfa-items:short-dress nil))
                                     :prompt "Top clothes"
                                     :default 'yadfa-items:tshirt :view clim:+option-pane-view+)
                                    ((clim:completion (yadfa-items:bra nil))
                                     :prompt "Top Undies"
                                     :default 'nil :view clim:+option-pane-view+)
                                    ((clim:completion (yadfa-items:jeans nil))
                                     :prompt "Bottom Clothes"
                                     :default 'nil :view clim:+option-pane-view+)
                                    ((clim:completion (yadfa-items:diaper yadfa-items:pullups yadfa-items:boxers yadfa-items:panties nil))
                                     :prompt "Bottom Undies"
                                     :default 'yadfa-items:diaper :view clim:+option-pane-view+)
                                    ((clim:completion (:normal :low :overactive))
                                     :prompt "Bladder capacity"
                                     :default :normal :view clim:+option-pane-view+))))
    (setf (player-of *game*) (make-instance 'player
                                            :position '(0 0 0 yadfa-zones:home)
                                            :name (first values)
                                            :male (second values)
                                            :species (third values)
                                            :bladder/need-to-potty-limit (getf '(:normal 300 :low 200 :overactive 149) (eighth values))
                                            :bladder/potty-dance-limit (getf '(:normal 450 :low 300 :overactive 150) (eighth values))
                                            :bladder/potty-desperate-limit (getf '(:normal 525 :low 350 :overactive 160) (eighth values))
                                            :bladder/maximum-limit (getf '(:normal 600 :low 400 :overactive 200) (eighth values))
                                            :bladder/contents (getf '(:normal 450 :low 300 :overactive 150) (eighth values))
                                            :wear (iter (for i in (cdddr (butlast values)))
                                                    (unless (eq i 'nil) (collect (make-instance i))))))
    (setf (team-of *game*) (list (player-of *game*)))
    (when (member (seventh values) '(yadfa-items:diaper yadfa-items:pullups))
      (iter (for i from 1 to (random 20))
        (push (make-instance (seventh values))
              (get-items-from-prop :dresser (position-of default))))))
  (write-line "You wake up from sleeping, the good news is that you managed to stay dry throughout the night. Bad news is your bladder filled up during the night. You would get up and head to the toilet, but the bed is too comfy, so you just lay there holding it until the discomfort of your bladder exceeds the comfort of your bed. Then eventually get up while holding yourself and hopping from foot to foot hoping you can make it to a bathroom in time" query-io))
