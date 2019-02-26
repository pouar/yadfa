(in-package :climi)

(in-package :clim-listener)
;;;; because it was quicker and easier than trying to write one of these myself from scratch
(macro-level:macro-level
    `(setf *default-text-style*
         (make-text-style ,@(if (member :mcclim-ffi-freetype *features*) '("DejaVu Sans Mono" "Book") '(:fix :roman)) :normal)))

;;; The CLIM Listener has the fonts hardcoded, the following 8 forms change them
(defmethod read-frame-command ((frame listener) &key (stream *standard-input*))
    "Specialized for the listener, read a lisp form to eval, or a command."
    (multiple-value-bind (object type)
        (let ((*command-dispatchers* '(#\,)))
            (with-text-style (stream *default-text-style*)
                (accept 'command-or-form :stream stream :prompt nil
                    :default "hello" :default-type 'empty-input)))
        (cond
            ((presentation-subtypep type 'empty-input)
                ;; Do nothing.
                `(com-eval (values)))
            ((presentation-subtypep type 'command) object)
            (t `(com-eval ,object)))))
(defun apropos-present-symbol (symbol &optional (stream *standard-output*) show-package)
    (multiple-value-bind (style ink)
        (values
            (if (or (fboundp symbol)
                    (boundp  symbol)
                    (find-class symbol nil))
                (make-text-style *apropos-symbol-bound-family*
                    *apropos-symbol-unbound-face*
                    :normal)
                (make-text-style *apropos-symbol-unbound-family*
                    *apropos-symbol-bound-face*
                    :normal))
            (cond ((eql (symbol-package symbol)
                       (find-package "KEYWORD"))
                      (make-rgb-color 0.46 0.0 0.0))
                ((fboundp symbol)        (make-rgb-color 0.0  0.0  0.3))
                ((find-class symbol nil) (make-rgb-color 0.03 0.35 0.48))
                ((boundp symbol)         (make-rgb-color 0.0  0.0  0.0))
                (t                       (make-rgb-color 0.6  0.6  0.6))))
        (with-drawing-options (stream :ink ink :text-style style)
            (with-output-as-presentation (stream symbol 'clim:symbol)
                (if show-package
                    (let ((*package* (find-package :common-lisp-user)))
                        (format stream "~W" symbol))
                    (princ (symbol-name symbol) stream)))
            (when (boundp symbol)
                (format stream " = ")
                (with-drawing-options (stream :ink +olivedrab+ ;; XXX
                                          :text-style (make-text-style
                                                          (text-style-family *default-text-style*)
                                                          (text-style-face *default-text-style*)
                                                          :small))
                    (let ((object (symbol-value symbol)))
                        (present object (presentation-type-of object) :stream stream)))))))
(defun package-grapher (stream package inferior-fun)
    "Draw package hierarchy graphs for `Show Package Users' and `Show Used Packages'."
    (let ((normal-ink +foreground-ink+)
             (arrow-ink  (make-rgb-color 0.72 0.72 0.72))
             (text-style (make-text-style (text-style-family *default-text-style*)
                             (text-style-face *default-text-style*)
                             :normal)))
        (with-drawing-options (stream :text-style text-style)
            (format-graph-from-roots (list package)
                #'(lambda (package stream)
                      (let ((internal (count-internal-symbols package))
                               (external (count-external-symbols package)))
                          (with-drawing-options (stream :ink (if (plusp external)
                                                                 normal-ink
                                                                 (make-rgb-color 0.4 0.4 0.4))
                                                    :text-style text-style)
                              (with-output-as-presentation (stream package 'package
                                                               :single-box t)
                                  (format stream "~A (~D/~D)" (package-name package) internal external)))))
                inferior-fun
                :stream stream
                :merge-duplicates t
                :graph-type :tree
                :orientation :horizontal
                :arc-drawer
                #'(lambda (stream foo bar x1 y1 x2 y2)
                      (declare (ignore foo bar))
                      (draw-arrow* stream x1 y1 x2 y2 :ink arrow-ink))))))
(setf *apropos-symbol-unbound-family* (text-style-family *default-text-style*))
(setf *apropos-symbol-unbound-face*   (text-style-face *default-text-style*))
(setf *apropos-symbol-bound-family*   (text-style-family *default-text-style*))
(setf *apropos-symbol-bound-face*     (text-style-face *default-text-style*))
(setf *graph-text-style* *default-text-style*)

;;; add init function
(defmethod default-frame-top-level
    ((frame listener)
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
                                (yadfa:intro-function frame-query-io))
                            (setq first-time nil
                                needs-redisplay nil))
                        (when interactorp
                            (setf (cursor-visibility (stream-text-cursor frame-query-io)) nil)
                            (when prompt
                                (with-text-style (frame-query-io climi::+default-prompt-style+)
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
(clim:define-command-table yadfa-commands)
(clim:define-command (yadfa-set-eol-action :command-table yadfa-commands :menu "Set EOL Action")
    ((keyword '(member :scroll :allow :wrap :wrap*)
         :prompt "Keyword"))
    (setf (stream-end-of-line-action *query-io*) keyword))
(clim:define-command (yadfa-gc :command-table yadfa-commands :menu "GC")
    ()
    (trivial-garbage:gc :full t))
(clim:define-command (yadfa-about :command-table yadfa-commands :menu "About Yadfa")
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
    (find-menu-item "Yadfa" (clim:find-command-table 'listener) :errorp nil)
    (clim:add-menu-item-to-command-table (clim:find-command-table 'listener) "Yadfa" :menu (clim:find-command-table 'yadfa-commands)))
(pushnew (clim:find-command-table 'yadfa-commands) (clim:command-table-inherit-from (clim:find-command-table 'listener)))

(in-package :yadfa)
(defclass stats-view (clim:view) ())
(clim:define-presentation-type base-character ())
(clim:define-presentation-method clim:present
    (base-character (type base-character) stream
        (view stats-view) &key)
    (format-stats base-character))
