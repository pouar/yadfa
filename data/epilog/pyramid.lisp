;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-pyramid"; coding: utf-8-unix; -*-
(in-package :yadfa-pyramid)
(defvar *maze*)
(defvar *position*)
(defvar *pattern-cache*)
(defvar *width*)
(defvar *height*)
(defvar *objects*)
(defvar *potty*)
(defvar *result*)
(defvar *positions*)
(defvar *finished-puzzles*)
(defvar *wear*)
(declaim (type fixnum *width* *height*)
         (type list *position* *positions* *wear* *objects*)
         (type hash-table *pattern-cache* *maze*)
         (type cons *potty*)
         (type (or cons boolean) *result*))
(define-command-table puzzle-commands)
(define-command-table game-commands)
(defclass area ()
  ((north
    :initform nil
    :initarg :north
    :accessor northp)
   (south
    :initform nil
    :initarg :south
    :accessor southp)
   (east
    :initform nil
    :initarg :east
    :accessor eastp)
   (west
    :initform nil
    :initarg :west
    :accessor westp)
   (objects
    :initform nil
    :initarg :objects
    :accessor objects-of)
   (puzzle
    :initform nil
    :initarg :puzzle
    :accessor puzzle-of)))
(defclass object ()
  ((name
   :initform ""
   :initarg :name
   :accessor name-of)))
(defclass puzzle ()
  ((name
    :initform ""
    :initarg :name
    :accessor name-of)
   (key-of
    :initform nil
    :initarg :key
    :accessor key-of)
   (needed-objects
    :initform nil
    :initarg :needed-objects
    :accessor needed-objects-of)))
(defmethod print-object ((object object) stream)
  (print-unreadable-object (object stream :type t)
    (if (slot-boundp object 'name)
        (write (slot-value object 'name) :stream stream)
        (write-string "#<unbound>" stream))))
(defmethod print-object ((object puzzle) stream)
  (print-unreadable-object (object stream :type t)
    (if (slot-boundp object 'name)
        (write (slot-value object 'name) :stream stream)
        (write-string "#<unbound>" stream))))
(serapeum:eval-always (define-presentation-type object (&optional place)))
(define-conditional-application-frame game-frame
    ()
  (:enable-commands (puzzle-commands))
  ()
  (:command-table (game-frame :inherit-from (puzzle-commands game-commands)))
  (:pane (horizontally ()
           (make-clim-stream-pane :name 'maze :incremental-redisplay t :scroll-bars nil
                                    :display-time :command-loop :display-function 'draw-maze :width 500 :height 600)
           (make-clim-interactor-pane :name 'int :display-time :command-loop :width 300 :height 600 :end-of-line-action :wrap*))))
(define-presentation-to-command-translator describe-area
    (area climi::com-describe game-frame
     :gesture :select
     :documentation "Describe"
     :pointer-documentation "Describe")
    (object)
  (list object))
(declaim (ftype (function () (values cons &optional)) process-potty))
(defun process-potty ()
  (let ((user (player-of *game*)))
    (incf (bladder/contents-of user)
          (bladder/fill-rate-of user))
    (incf (bowels/contents-of user)
          (bowels/fill-rate-of user))
    (cons (when (>= (bladder/contents-of user) (bladder/maximum-limit-of user))
            (apply 'wet :wetter user (when (boundp '*wear*) `(:clothes ,*wear*))))
          (when (>= (bowels/contents-of user) (bowels/maximum-limit-of user))
            (apply 'mess :messer user (when (boundp '*wear*) `(:clothes ,*wear*)))))))
(define-conditional-command (com-end-puzzle :name t)
    (game-frame :enable-commands (end-puzzle-commands)
                :disable-commands (puzzle-commands))
    ())
(define-game-frame-command (com-exit-game :name t)
    ()
  (frame-exit *application-frame*))
(defun set-mode (key)
  (declare (type (or boolean cons) key))
  (change-entity-enabledness 'com-end-puzzle)
  (setf *result* key)
  (when (consp key)
    (format t "you dance around holding the front of your ~a like a 5 year old until you get an expression of horror and embarrassment on your face as you ~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~} your pamps~%"
            (typecase (car *wear*)
              (diaper "diapers")
              (pullup "pullups"))
            `(,@(when (car *potty*)
                  '("flood"))
              ,@(when (cdr *potty*)
                  '("mess"))))))
(define-command (com-move :name t :command-table puzzle-commands)
    ((direction '(member-alist (("North" :north)
                                ("South" :south)
                                ("West" :west)
                                ("East" :east)))
                :prompt "[[North | South | East | West]]"))
  (locally (declare (type keyword direction))
    (if (funcall (case direction
                   (:north 'northp)
                   (:south 'southp)
                   (:west 'westp)
                   (:east 'eastp))
                 (gethash *position* *maze*))
        (progn (setf *position* (destructuring-bind (x y) *position*
                                  (case direction
                                    (:north `(,x ,(1- y)))
                                    (:south `(,x ,(1+ y)))
                                    (:west `(,(1- x) ,y))
                                    (:east `(,(1+ x) ,y)))))
               (setf *potty* (process-potty))
               (when (or (car *potty*) (cdr (car *potty*)))
                 (set-mode *potty*)))
        (format t "Can't move ~a" (case direction
                                    (:north "North")
                                    (:south "South")
                                    (:west "West")
                                    (:east "East"))))))
(define-command (com-eval :name t :command-table game-commands)
    ((form form))
  (present (eval form) 'expression))
(define-command (com-check :name t :command-table puzzle-commands)
    ()
  (when (eq (case (and (puzzle-of (gethash *position* *maze*))
                       (not (set-exclusive-or (objects-of (gethash *position* *maze*)) (needed-objects-of (puzzle-of (gethash *position* *maze*)))))
                       (key-of (puzzle-of (gethash *position* *maze*))))
              (:puzzle (let ((result (yadfa-puzzle:run-game)))
                         (case result
                           (nil nil)
                           ((t) t)
                           (t (set-mode *potty*)))))
              (:potty
               (wet :pants-down t :force-wet-amount t)
               (mess :pants-down t :force-mess-amount t)
               (set-mode t)
               (write-line "Desperate for a bathroom You quickly pull your pullups down and reluctantly sit on the training potty and with a look of relief on your face as you use the training potty like a toddler.")))
            t)
    (unless (set-exclusive-or '(:puzzle) (pushnew (key-of (puzzle-of (gethash *position* *maze*))) *finished-puzzles*))
      (setf (first *wear*) (make-instance 'yadfa-items:temple-pullups))
      (setf (puzzle-of (gethash (pop *positions*) *maze*)) (make-instance 'puzzle :name "Training Potty" :key :potty))
      (write-line "You hear something move. The locked shape glow on the tapes of your diaper disappears and your diaper somehow transforms into a pullup. QUICK!!!! FIND A BATHROOM!!!!"))))
(define-command (com-place :name t :command-table puzzle-commands)
    ((object object))
  (cond ((not (member object *objects*))
         (format t "~a isn't in your inventory~%" (name-of object)))
        ((and (puzzle-of (gethash *position* *maze*)) (not (member object (needed-objects-of (puzzle-of (gethash *position* *maze*))))))
         (format t "~a doesn't belong there~%" (name-of object)))
        (t (push object (objects-of (gethash *position* *maze*)))
           (alexandria:deletef *objects* object))))
(define-command (com-excavate :name t :command-table puzzle-commands)
    ((object object))
  (cond ((not (member object (objects-of (gethash *position* *maze*))))
         (format t "~a isn't in there~%" (name-of object)))
        ((and (puzzle-of (gethash *position* *maze*)) (member object (needed-objects-of (puzzle-of (gethash *position* *maze*)))))
         (format t "~a can't be removed~%" (name-of object)))
        (t (push object *objects*)
           (alexandria:deletef (objects-of (gethash *position* *maze*)) object))))
(defclass stat-view (view) ())
(defclass object-view (view) ())
(defconstant +stat-view+ (make-instance 'stat-view))
(defconstant +object-view+ (make-instance 'object-view))
(define-presentation-to-command-translator excavate-object
    (object com-excavate game-frame
     :tester ((object presentation)
              (when (listp (presentation-type presentation))
                (destructuring-bind (object &optional place) (presentation-type presentation)
                  (declare (ignore object))
                  (eq place :area)))))
    (object)
  (list object))
(define-presentation-to-command-translator place-object
    (object com-place game-frame
     :tester ((object presentation)
              (when (listp (presentation-type presentation))
                (destructuring-bind (object &optional place) (presentation-type presentation)
                  (declare (ignore object))
                  (eq place :inventory)))))
    (object)
  (list object))
(define-presentation-to-command-translator check-puzzle
    (puzzle com-check game-frame
     :tester ((object) object))
    (object)
  '())
(define-presentation-method present (user (type base-character) medium (view stat-view) &key)
  (format medium "Name: ~a~%" (name-of user))
  (format medium "Bladder: ")
  (yadfa-clim:draw-bar medium
                       (/ (bladder/contents-of user) (bladder/maximum-limit-of user))
                       ((>= (bladder/contents-of user) (bladder/potty-desperate-limit-of user)) :red)
                       ((>= (bladder/contents-of user) (bladder/potty-dance-limit-of user)) (:orange :red))
                       ((>= (bladder/contents-of user) (bladder/need-to-potty-limit-of user)) :yellow)
                       (t :green))
  (terpri medium)
  (format medium "Bowels: ")
  (yadfa-clim:draw-bar medium
                       (/ (bowels/contents-of user) (bowels/maximum-limit-of user))
                       ((>= (bowels/contents-of user) (bowels/potty-desperate-limit-of user)) :red)
                       ((>= (bowels/contents-of user) (bowels/potty-dance-limit-of user)) (:orange :red))
                       ((>= (bowels/contents-of user) (bowels/need-to-potty-limit-of user)) :yellow)
                       (t :green))
  (terpri medium)
  (format medium "Diaper State: ~{~a~}~%"
          (let ((a ()))
            (destructuring-bind (&key (sogginess 0) (sogginess-capacity 0) (messiness 0) (messiness-capacity 0))
                (calculate-diaper-usage* (filter-items (if (boundp '*wear*) *wear* (wear-of user)) 'closed-bottoms))
              (declare (type real sogginess sogginess-capacity messiness messiness-capacity))
              (cond ((>= sogginess sogginess-capacity)
                     (push "Leaking" a)
                     (push " " a))
                    ((>= sogginess 100)
                     (push "Soggy" a)
                     (push " " a)))
              (cond ((>= messiness messiness-capacity)
                     (push "Blowout" a))
                    ((>= messiness 100)
                     (push "Messy" a)))
              (unless a (push "Clean" a))
              a))))
(defmethod run-frame-top-level ((frame game-frame) &key)
  (let* ((*maze* (make-hash-table :test 'equal))
         (*width* 10)
         (*height* 10)
         (*position* `(,(random *width*) ,(random *height*)))
         (*pattern-cache* (make-hash-table :test 'equal))
         *objects*
         *result*
         *finished-puzzles*
         (*potty* '(nil))
         (*wear* (list (make-instance 'yadfa-items:cursed-diaper)))
         (*positions* (alexandria:shuffle (iter (for x from 0 to (1- *width*))
                                           (appending (iter (for y from 0 to (1- *height*))
                                                        (collect `(,x ,y))))))))
    (declare (special *maze* *position* *width* *height* *pattern-cache* *objects* *result* *positions* *finished-puzzles* *wear* *potty*)
             (type fixnum *width* *height*)
             (type list *position* *positions* *wear* *objects*)
             (type hash-table *pattern-cache* *maze*)
             (type cons *potty*)
             (type (or cons boolean) *result*))
    (handler-case (progn (labels ((neighbors (x y width height)
                                    (declare (type fixnum x y width height))
                                    (remove-if
                                     (lambda (x-y)
                                       (declare (type list x-y))
                                       (not (and (< -1 (first x-y) width)
                                                 (< -1 (second x-y) height))))
                                     `((,x ,(1+ y) southp) (,(1- x) ,y westp) (,x ,(1- y) northp) (,(1+ x) ,y eastp))))
                                  (remove-wall (width height &optional visited)
                                    (labels ((walk (x y width height)
                                               (push (list x y) visited)
                                               (iter (for (u v w) in (alexandria:shuffle (neighbors x y width height)))
                                                 (unless (member (list u v) visited :test #'equal)
                                                   (eval `(setf (,w ,(gethash `(,x ,y) *maze*)) t))
                                                   (eval `(setf (,(getf '(northp southp
                                                                          southp northp
                                                                          westp eastp
                                                                          eastp westp)
                                                                        w)
                                                                 ,(gethash `(,u ,v) *maze*))
                                                                t))
                                                   (walk u v width height)))))
                                      (walk (random width) (random height) width height))))
                           (iter (for x from 0 to (1- *width*))
                             (iter (for y from 0 to (1- *height*))
                               (setf (gethash `(,x ,y) *maze*) (make-instance 'area))))
                           (remove-wall *width* *height*))
                         (let ((emblem (make-instance 'object :name "Emblem"))
                               (puzzle (make-instance 'puzzle :name "Puzzle" :key :puzzle)))
                           (setf (puzzle-of (gethash (pop *positions*) *maze*)) puzzle)
                           (push emblem (needed-objects-of puzzle))
                           (push emblem (objects-of (gethash (pop *positions*) *maze*))))
                         (call-next-method))
      (frame-exit () (values *result* *wear*)))))
;;; yadfa-pyramid::game-frame writes some text to its standard output when it first starts up, but the default frame top level function
;; calls (redisplay-frame-panes frame :force-p t) when it first starts which ends up clearing this, so I copy pasted the original here
;;; and modified it so it prints the text after it gets redisplayed
(defmethod default-frame-top-level
    ((frame game-frame)
     &key (command-parser 'command-line-command-parser)
          (command-unparser 'command-line-command-unparser)
          (partial-command-parser
           'command-line-read-remaining-arguments-for-partial-command)
          (prompt "Command: "))
  ;; Give each pane a fresh start first time through.
  (let ((needs-redisplay t)
        (first-time t))
    (loop
      ;; The variables are rebound each time through the loop because the
      ;; values of frame-standard-input et al. might be changed by a command.
      ;;
      ;; We rebind *QUERY-IO* ensuring variable is always a stream,
      ;; but we use FRAME-QUERY-IO for our own actions and to decide
      ;; whenever frame has the query IO stream associated with it..
      (let* ((frame-query-io (frame-query-io frame))
             (interactorp (typep frame-query-io 'interactor-pane))
             (*standard-input*  (or (frame-standard-input frame)  *standard-input*))
             (*standard-output* (or (frame-standard-output frame) *standard-output*))
             (*query-io* (or frame-query-io *query-io*))
             ;; during development, don't alter *error-output*
             ;; (*error-output* (frame-error-output frame))
             (*pointer-documentation-output* (frame-pointer-documentation-output frame))
             (*command-parser* command-parser)
             (*command-unparser* command-unparser)
             (*partial-command-parser* partial-command-parser))
        (restart-case
            (flet ((execute-command ()
                     (alexandria:when-let ((command (read-frame-command frame :stream frame-query-io)))
                       (setq needs-redisplay t)
                       (execute-frame-command frame command))))
              (when needs-redisplay
                (redisplay-frame-panes frame :force-p first-time)
                (when first-time
                  (write-line "You enter the temple and a locked shaped light emits from the tapes of your diapers. You struggle with the tapes but they won't come off. Better do something before you end up flooding and/or messing your pamps." frame-query-io))
                (setq first-time nil
                      needs-redisplay nil))
              (when interactorp
                (setf (cursor-visibility (stream-text-cursor frame-query-io)) nil)
                (when prompt
                  (with-text-style (frame-query-io (make-text-style :sans-serif :bold :normal))
                    (if (stringp prompt)
                        (write-string prompt frame-query-io)
                        (funcall prompt frame-query-io frame))
                    (force-output frame-query-io))))
              (execute-command)
              (when interactorp
                (fresh-line frame-query-io)))
          (abort ()
            :report "Return to application command loop."
            (if interactorp
                (format frame-query-io "~&Command aborted.~&")
                (beep))))))))
(defun draw-maze (frame pane)
  (declare (ignore frame))
  (labels ((bitmap (position)
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
               (iter (for direction in '(eastp westp southp northp))
                 (for byte-position upfrom 0)
                 (unless (funcall direction (gethash position *maze*))
                   (setf (ldb (byte 1 byte-position) b) 1)))
               (aref array b)))
           (pattern-cache (path designs)
             (declare (type pathname path)
                      (type list designs))
             (or (gethash `(,path ,designs) *pattern-cache*)
                 (setf (gethash `(,path ,designs) *pattern-cache*)
                       (clim:make-pattern-from-bitmap-file
                        (uiop:merge-pathnames*
                         path
                         #P"yadfa:home;pixmaps;map-patterns;")
                        :format :xpm
                        :designs designs)))))
    (updating-output (pane :unique-id 'map :cache-test 'equal :cache-value (sxhash *position*))
      (iter (for x from 0 to (1- *width*))
        (iter (for y from 0 to (1- *height*))
          (updating-output (pane :unique-id `(,x ,y) :id-test 'equal :cache-value (equal *position* `(,x ,y)))
            (with-output-as-presentation (pane (gethash `(,x ,y) *maze*) 'area)
              (draw-pattern* pane (pattern-cache (bitmap `(,x ,y))
                                                 `(,clim:+background-ink+ ,(clim:make-rgb-color (if (equal `(,x ,y) *position*) 1 0) 0 0)))
                             (* x 16) (* y 16)))))))
    (setf (stream-cursor-position pane) (values 0 (* 16 *height*)))
    (updating-output (pane :unique-id 'inventory :cache-value (sxhash *objects*))
      (formatting-table (pane)
        (formatting-row (pane)
          (formatting-cell (pane) (write-string "Inventory: (" pane))
          (iter (for object in *objects*)
            (formatting-cell (pane) (present object '(object :inventory) :stream pane)))
          (formatting-cell (pane) (write-string ")" pane))))
      (terpri pane))
    (updating-output (pane :unique-id 'objects :cache-value (sxhash (objects-of (gethash *position* *maze*))))
      (formatting-table (pane)
        (formatting-row (pane)
          (formatting-cell (pane) (write-string "Objects: (" pane))
          (iter (for object in (objects-of (gethash *position* *maze*)))
            (formatting-cell (pane) (present object '(object :area) :stream pane)))
          (formatting-cell (pane) (write-string ")" pane))))
      (terpri pane))
    (updating-output (pane :unique-id 'puzzle :cache-value (puzzle-of (gethash *position* *maze*)))
      (formatting-table (pane)
        (formatting-row (pane)
          (formatting-cell (pane) (write-string "Puzzle: " pane))
          (formatting-cell (pane) (present (puzzle-of (gethash *position* *maze*)) 'puzzle :stream pane))))
      (terpri pane))
    (updating-output (pane :unique-id 'player :cache-value (sxhash `(,(bladder/contents-of (player-of *game*))
                                                                     ,(bowels/contents-of (player-of *game*)))))
      (present (player-of *game*) (type-of (player-of *game*)) :view +stat-view+ :stream pane)
      (terpri pane))
    (updating-output (pane :unique-id 'controls :cache-value *result*)
      (typecase *result*
        (null (formatting-table (pane)
                (macrolet ((thunk (direction text)
                             `(with-output-as-presentation (pane '(com-move ,direction) '(command :command-table game-frame))
                                (surrounding-output-with-border
                                    (pane :shape :rounded :radius 6
                                          :background +gray80+ :highlight-background +gray90+)
                                  (format pane ,text)))))
                  (formatting-row (pane)
                    (formatting-cell (pane) pane)
                    (formatting-cell (pane) (thunk :north "North"))
                    (formatting-cell (pane) pane))
                  (formatting-row (pane)
                    (formatting-cell (pane) (thunk :west "West"))
                    (formatting-cell (pane) pane)
                    (formatting-cell (pane) (thunk :east "East")))
                  (formatting-row (pane)
                    (formatting-cell (pane) pane)
                    (formatting-cell (pane) (thunk :south "South"))
                    (formatting-cell (pane) pane)))))
        (t (formatting-table (pane)
             (formatting-row (pane)
               (formatting-cell (pane)
                 (with-output-as-presentation (pane '(com-exit-game) '(command :command-table game-frame))
                   (surrounding-output-with-border
                       (pane :shape :rounded :radius 6
                             :background +gray80+ :highlight-background +gray90+)
                     (format pane "Exit Minigame")))))))))))
(defun run-game ()
  (run-frame-top-level (make-application-frame 'game-frame)))
