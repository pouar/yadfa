(in-package :clim-listener)
;;;; because it was quicker and easier than trying to write one of these myself from scratch

(setf *default-text-style* (make-text-style :fix :roman :normal))
(defmethod default-frame-top-level
    ((frame listener)
        &key (command-parser 'command-line-command-parser)
        (command-unparser 'command-line-command-unparser)
        (partial-command-parser
            'command-line-read-remaining-arguments-for-partial-command)
        (prompt "Command: "))
    ;; Give each pane a fresh start first time through.
    (let ((first-time t))
        (loop
            ;; The variables are rebound each time through the loop because the
            ;; values of frame-standard-input et al. might be changed by a command.
            (let* ((*standard-input*  (or (frame-standard-input frame)
                                          *standard-input*))
                      (*standard-output* (or (frame-standard-output frame)
                                             *standard-output*))
                      (query-io  (frame-query-io frame))
                      (*query-io* (or query-io *query-io*))
                      (*pointer-documentation-output*
                          (frame-pointer-documentation-output frame))
                      ;; during development, don't alter *error-output*
                      ;; (*error-output* (frame-error-output frame))
                      (*command-parser* command-parser)
                      (*command-unparser* command-unparser)
                      (*partial-command-parser* partial-command-parser)
                      (interactorp (typep *query-io* 'interactor-pane)))
                (restart-case
                    (progn
                        (redisplay-frame-panes frame :force-p first-time)
                        (when first-time
                            (yadfa::intro-function query-io))
                        (setq first-time nil)
                        (if query-io
                            ;; For frames with an interactor:
                            (progn
                                ;; Hide cursor, so we don't need to toggle it during
                                ;; command output.
                                (setf (cursor-visibility (stream-text-cursor *query-io*))
                                    nil)
                                (when (and prompt interactorp)
                                    (with-text-style (*query-io* *default-text-style*)
                                        (if (stringp prompt)
                                            (write-string prompt *query-io*)
                                            (funcall prompt *query-io* frame))
                                        (force-output *query-io*)))
                                (let ((command (read-frame-command frame
                                                   :stream *query-io*)))
                                    (when interactorp
                                        (fresh-line *query-io*))
                                    (when command
                                        (execute-frame-command frame command))
                                    (when interactorp
                                        (fresh-line *query-io*))))
                            ;; Frames without an interactor:
                            (let ((command (read-frame-command frame :stream nil)))
                                (when command (execute-frame-command frame command)))))
                    (abort ()
                        :report "Return to application command loop"
                        (if interactorp
                            (format *query-io* "~&Command aborted.~&")
                            (beep))))))))
(clim:define-command-table yadfa-commands)
(clim:define-command (yadfa-set-eol-action :command-table yadfa-commands :menu "Set EOL Action")
    ((keyword '(member :scroll :allow :wrap)
         :prompt "Keyword"))
    (setf (stream-end-of-line-action *query-io*) keyword))
(clim:add-menu-item-to-command-table (clim:find-command-table 'listener) "Yadfa" :menu (clim:find-command-table 'yadfa-commands))
(pushnew (clim:find-command-table 'yadfa-commands) (clim:command-table-inherit-from (clim:find-command-table 'listener)))
