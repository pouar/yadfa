(in-package :yadfa)
(clim:define-command-table yadfa-menu-commands)
(clim:define-command (yadfa-set-eol-action :command-table yadfa-menu-commands :menu "Set EOL Action")
    ((keyword '(member :scroll :allow :wrap :wrap*)
         :prompt "Keyword"))
    (setf (clim:stream-end-of-line-action clim-listener::*query-io*) keyword))
(clim:define-command (yadfa-gc :command-table yadfa-menu-commands :menu "GC")
    ()
    (trivial-garbage:gc :full t))
(clim:define-command (yadfa-about :command-table yadfa-menu-commands :menu "About Yadfa")
    ()
    (dolist (i '("README" "AUTHORS"))
        (format t "~a:~%" i)
        (with-open-file (s (uiop:merge-pathnames*
                               i
                               (if uiop:*image-dumped-p*
                                   (pathname (directory-namestring (truename (uiop:argv0))))
                                   (asdf:system-source-directory :yadfa)))
                            :direction :input)
            (loop until (let ((ret (multiple-value-list (read-line s nil))))
                            (format t "~a~%" (if (first ret) (first ret) ""))
                            (second ret))))))
(unless
    (clim:find-menu-item "Yadfa" (clim:find-command-table 'clim-listener::listener) :errorp nil)
    (clim:add-menu-item-to-command-table (clim:find-command-table 'clim-listener::listener) "Yadfa" :menu (clim:find-command-table 'yadfa-menu-commands)))
(pushnew (clim:find-command-table 'yadfa-menu-commands) (clim:command-table-inherit-from (clim:find-command-table 'clim-listener::listener)))
(clim:define-presentation-action zone-presentation-menu
    (zone nil clim:global-command-table
        :documentation "Menu"
        :menu nil
        :gesture :menu)
    (object presentation frame window x y)
    (declare (ignore object))
    (clim:call-presentation-menu presentation clim:*input-context*
        frame window x y
        :for-menu t
        :label (format nil "Operation on ~A"
                   (clim:presentation-type presentation))))
(clim:define-command (com-yadfa-move :command-table clim:global-command-table :menu t :name "Move Here")
    ((zone zone))
    (apply #'yadfa/world:move
        (cond
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
                (format t "You're either already on that zone or you tried specifying a path that involves turning (which this interface can't do because Pouar sucks at writing code that generates paths)~%")))))
(clim:define-presentation-to-command-translator com-describe-object-translator
    (yadfa-class climi::com-describe clim:global-command-table
        :gesture :describe
        :documentation "Describe"
        :pointer-documentation "Describe"
        :tester ((object presentation)
              (declare (ignore object))
              (not (eq presentation climi::*null-presentation*)))
        :menu nil)
    (object)
    (list object))
(clim:define-presentation-to-command-translator com-yadfa-move-translator
    (zone com-yadfa-move clim:global-command-table
        :documentation "Move"
        :pointer-documentation "Move Here"
        :menu t)
    (object)
    (list object))
