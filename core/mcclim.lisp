;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-clim"; coding: utf-8-unix; -*-
(in-package :yadfa-clim)
(define-command-table yadfa-menu-commands)
(define-command-table yadfa-presentation-commands)
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
(pushnew (find-command-table 'yadfa-presentation-commands) (command-table-inherit-from (find-command-table 'clim-listener::listener)))
(define-command (com-yadfa-move :command-table yadfa-presentation-commands :menu t :name "Move Here")
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
(define-command (com-yadfa-describe-zone :command-table yadfa-presentation-commands :menu t :name "Print Zone Description")
    ((zone zone))
  (yadfa-bin:lst :describe-zone zone))
(define-presentation-to-command-translator com-yadfa-move-translator
    (zone com-yadfa-move yadfa-presentation-commands
     :documentation "Move"
     :pointer-documentation "Move Here"
     :gesture nil
     :menu t)
    (object)
  (list object))
(define-presentation-to-command-translator com-yadfa-describe-zone-translator
    (zone com-yadfa-describe-zone yadfa-presentation-commands
     :documentation "Print Zone Description"
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
