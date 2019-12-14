;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-clim"; coding: utf-8-unix; -*-
(in-package :yadfa-clim)
(define-command-table yadfa-world-commands)
(define-command-table yadfa-battle-commands)
(define-command-table yadfa-bin-commands)
(define-command-table yadfa-menu-commands)

(define-command (yadfa-set-eol-action :command-table yadfa-menu-commands :menu "Set EOL Action")
    ((keyword '(member :scroll :allow :wrap :wrap*)
              :prompt "Keyword"))
  (setf (stream-end-of-line-action clim-listener::*query-io*) keyword))
(define-command (yadfa-gc :command-table yadfa-menu-commands :menu "GC")
    ()
  (trivial-garbage:gc :full t))
(unless
    (find-menu-item "Yadfa" (find-command-table 'clim-listener::listener) :errorp nil)
  (add-menu-item-to-command-table (find-command-table 'clim-listener::listener) "Yadfa" :menu (find-command-table 'yadfa-menu-commands)))
(pushnew (find-command-table 'yadfa-menu-commands) (command-table-inherit-from (find-command-table 'clim-listener::listener)))
(pushnew (find-command-table 'yadfa-world-commands) (command-table-inherit-from (find-command-table 'clim-listener::listener)))
(pushnew (find-command-table 'yadfa-battle-commands) (command-table-inherit-from (find-command-table 'clim-listener::listener)))
(pushnew (find-command-table 'yadfa-bin-commands) (command-table-inherit-from (find-command-table 'clim-listener::listener)))
(conditional-commands:define-conditional-command (com-enable-world)
    (clim-listener::listener :enable-commands (yadfa-world-commands yadfa-bin-commands)
                             :disable-commands (yadfa-battle-commands))
    ())
(conditional-commands:define-conditional-command (com-enable-battle)
    (clim-listener::listener :enable-commands (yadfa-battle-commands yadfa-bin-commands)
                             :disable-commands (yadfa-world-commands))
    ())
(define-command
    (com-inspect :command-table global-command-table :name "Inspect")
    ((obj 'expression
          :prompt "object"
          :gesture :inspect))
  (clouseau:inspect obj :new-process t))
(conditional-commands:add-entity-enabledness-change 'listener-start 'clim-listener::listener
                                                    :enable-commands '(yadfa-bin-commands yadfa-world-commands)
                                                    :disable-commands '(yadfa-world-commands)
                                                    :change-status nil)
(defclass stat-view (view) ())
(defconstant +stat-view+ (make-instance 'stat-view))
(defmacro draw-bar (medium stat &rest colors)
  `(multiple-value-bind (x y) (stream-cursor-position ,medium)
     (draw-rectangle* ,medium x y (+ x (* ,stat 400)) (+ y 15)
                           :ink (cond ,@(iter (for i in colors)
                                          (collect `(,(car i) ,(intern (format nil "+~a+"
                                                                               (if (typep (car (last i)) 'cons)
                                                                                   (caar (last i))
                                                                                   (car (last i))))
                                                                       "CLIM"))))))
     (draw-rectangle* ,medium x y (+ x 400) (+ y 15)
                           :filled nil)
     (stream-set-cursor-position ,medium (+ x 400) y)))
(define-presentation-method present (object (type base-character) stream (view stat-view) &key)
  (format stream "Name: ~a~%" (name-of object))
  (out "Sex: " (if (malep object) "Male" "Female") :%)
  (format stream "Species: ~a~%" (species-of object))
  (format stream "Description: ~a~%" (description-of object))
  (when (typep object 'team-member)
    (out "Tail: " (tail-of object) :%
         "Wings: " (wings-of object) :%
         "Skin: " (skin-of object) :%))
  (format stream "Health: ")
  (draw-bar stream (/ (health-of object) (calculate-stat object :health))
            ((> (health-of object) (* (calculate-stat object :health) 1/2)) :green)
            ((> (health-of object) (* (calculate-stat object :health) 1/4)) :yellow)
            (t :red))
  (format stream " ~a~%Energy: "
          (if (member object (cons (player-of *game*) (allies-of *game*)))
              (format nil "(~a/~a)"
                      (health-of object)
                      (calculate-stat object :health))
              ""))
  (draw-bar stream (/ (energy-of object) (calculate-stat object :energy))
            ((> (energy-of object) (* (calculate-stat object :energy) 1/2)) :green)
            ((> (energy-of object) (* (calculate-stat object :energy) 1/4)) :yellow)
            (t :red))
  (format stream " ~a~%"
          (if (member object (cons (player-of *game*) (allies-of *game*)))
              (format nil "(~a/~a)"
                      (energy-of object)
                      (calculate-stat object :energy))
              ""))
  (when *battle*
    (write-string "Conditions: " stream)
    (iter (for i in (getf (status-conditions-of *battle*) object))
      (format stream "`~a' " (name-of i)))
    (write-char #\Newline stream))
  (format stream "Stats: ~a~%Base-Stats: ~a~%"
          (let ((wield-stats (calculate-wield-stats object))
                (wear-stats (calculate-wear-stats object)))
            (iter (for (a b) on (base-stats-of object) by #'cddr)
              (collect a)
              (collect (+ b (getf wield-stats a) (getf wear-stats a)))))
          (base-stats-of object))
  (let* ((c (filter-items (wear-of object) 'closed-bottoms))
         (b (calculate-diaper-usage* c)))
    (cond ((find '(or tabbed-briefs pullon incontinence-pad) c :test (lambda (o e) (typep e o)))
           (format stream "Diaper State: ~{~a~}~%"
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
           (format stream "Pants State: ~{~a~}~%"
                   (let ((a ()))
                     (cond ((>= (getf b :sogginess) (getf b :sogginess-capacity))
                            (push "Wet" a)
                            (push " " a)))
                     (cond ((>= (getf b :messiness) (getf b :messiness-capacity))
                            (push "Messy" a)))
                     (unless a (push "Clean" a))
                     a))))
    (when c
      (write-string "Sogginess: " stream)
      (draw-bar stream (/ (getf b :sogginess) (getf b :sogginess-capacity))
                ((>= (getf b :sogginess) (- (getf b :sogginess-capacity) (/ (getf b :sogginess-capacity) 4))) :red)
                ((>= (getf b :sogginess) (/ (getf b :sogginess-capacity) 2)) :yellow)
                (t :green))
      (terpri stream)
      (write-string "Messiness: " stream)
      (draw-bar stream (/ (getf b :messiness) (getf b :messiness-capacity))
                ((>= (getf b :messiness) (- (getf b :messiness-capacity) (/ (getf b :messiness-capacity) 4))) :red)
                ((>= (getf b :messiness) (/ (getf b :messiness-capacity) 2)) :yellow)
                (t :green))
      (terpri stream)))
  (write-string "Bladder State: " stream)
  (draw-bar stream (/ (bladder/contents-of object) (bladder/maximum-limit-of object))
            ((>= (bladder/contents-of object) (bladder/potty-desperate-limit-of object)) :red)
            ((>= (bladder/contents-of object) (bladder/potty-dance-limit-of object)) (:orange :red))
            ((>= (bladder/contents-of object) (bladder/need-to-potty-limit-of object)) :yellow)
            (t :green))
  (terpri stream)
  (write-string "Bowels State: " stream)
  (draw-bar stream (/ (bowels/contents-of object) (bowels/maximum-limit-of object))
            ((>= (bowels/contents-of object) (bowels/potty-desperate-limit-of object)) :red)
            ((>= (bowels/contents-of object) (bowels/potty-dance-limit-of object)) (:orange :red))
            ((>= (bowels/contents-of object) (bowels/need-to-potty-limit-of object)) :yellow)
            (t :green))
  (terpri stream))
(define-command (com-yadfa-move :command-table yadfa-world-commands :menu t :name "Move")
    ((zone zone))
  (block nil
    (apply #'yadfa-world:move
           (cond
             (*battle*
              (format t "You can't do this in battle~%")
              (return))
             ((and
               (<
                (first (position-of (player-of *game*)))
                (first (position-of zone)))
               (=
                (second (position-of (player-of *game*)))
                (second (position-of zone)))
               (=
                (third (position-of (player-of *game*)))
                (third (position-of zone)))
               (equal
                (fourth (position-of (player-of *game*)))
                (fourth (position-of zone))))
              (iter (for i
                         from (1+ (first (position-of (player-of *game*))))
                         to (first (position-of zone)))
                (collect :east)))
             ((and
               (>
                (first (position-of (player-of *game*)))
                (first (position-of zone)))
               (=
                (second (position-of (player-of *game*)))
                (second (position-of zone)))
               (=
                (third (position-of (player-of *game*)))
                (third (position-of zone)))
               (equal
                (fourth (position-of (player-of *game*)))
                (fourth (position-of zone))))
              (iter (for i
                         from (1- (first (position-of (player-of *game*))))
                         downto (first (position-of zone)))
                (collect :west)))
             ((and
               (=
                (first (position-of (player-of *game*)))
                (first (position-of zone)))
               (<
                (second (position-of (player-of *game*)))
                (second (position-of zone)))
               (=
                (third (position-of (player-of *game*)))
                (third (position-of zone)))
               (equal
                (fourth (position-of (player-of *game*)))
                (fourth (position-of zone))))
              (iter (for i
                         from (1+ (second (position-of (player-of *game*))))
                         to (second (position-of zone)))
                (collect :south)))
             ((and
               (=
                (first (position-of (player-of *game*)))
                (first (position-of zone)))
               (>
                (second (position-of (player-of *game*)))
                (second (position-of zone)))
               (=
                (third (position-of (player-of *game*)))
                (third (position-of zone)))
               (equal
                (fourth (position-of (player-of *game*)))
                (fourth (position-of zone))))
              (iter (for i
                         from (1- (second (position-of (player-of *game*))))
                         downto (second (position-of zone)))
                (collect :north)))
             (t
              (format t "You're either already on that zone or you tried specifying a path that involves turning (which this interface can't do because Pouar sucks at writing code that generates paths)~%")
              (return))))))
(define-command (com-yadfa-describe-zone :command-table yadfa-bin-commands :menu t :name "Describe Zone")
    ((zone zone))
  (yadfa-bin:lst :describe-zone zone))
(define-presentation-to-command-translator com-yadfa-move-translator
    (zone com-yadfa-move yadfa-world-commands
     :documentation "Move"
     :pointer-documentation "Move Here"
     :gesture nil
     :menu t)
    (object)
  (list object))
(define-presentation-to-command-translator com-yadfa-describe-zone-translator
    (zone com-yadfa-describe-zone yadfa-bin-commands
     :documentation "Describe Zone"
     :pointer-documentation "Print Zone Description"
     :gesture nil
     :menu t)
    (object)
  (list object))
(define-application-frame emacs-frame (standard-application-frame)
  ((lambda :accessor emacs-frame-lambda
     :initarg :emacs-frame-lambda
     :initform (lambda (frame)
                 (declare (ignore frame))
                 t)))
  (:panes (int :interactor :height 400 :width 600))
  (:layouts
   (default int)))
(defmethod default-frame-top-level :around ((frame emacs-frame)
                                            &key (command-parser 'command-line-command-parser)
                                                 (command-unparser 'command-line-command-unparser)
                                                 (partial-command-parser
                                                  'command-line-read-remaining-arguments-for-partial-command)
                                                 (prompt "Command: "))
  (declare (ignore prompt))
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
        (progn
          (redisplay-frame-panes frame :force-p t)
          (when interactorp
            (setf (cursor-visibility (stream-text-cursor frame-query-io)) nil))
          (funcall (emacs-frame-lambda frame) frame))
      (abort ()
        :report "Return to application command loop."
        (if interactorp
            (format frame-query-io "~&Command aborted.~&")
            (beep))))))
