(in-package :climi)
;;; McCLIM is missing support for :exit-boxes in invoke-accepting-values, which depends on the accept-values application frame, which McCLIM is also missing, the following three forms implement just enough of it for :exit-boxes to work in my game
(define-application-frame accept-values ()
    ((stream :initarg :stream)
        (exit-boxes :initform '((:exit "OK") (:abort "Cancel")) :initarg :exit-boxes)))
(defun invoke-accepting-values
    (stream body
        &key own-window (exit-boxes '((:exit "OK") (:abort "Cancel")))
        (initially-select-query-identifier nil initially-select-p)
        select-first-query
        modify-initial-query resynchronize-every-pass resize-frame
        align-prompts label scroll-bars
        x-position y-position width height
        (command-table 'accept-values)
        (frame-class 'accept-values))
    (declare (ignore own-window exit-boxes modify-initial-query
                 resize-frame scroll-bars x-position y-position width height frame-class))
    (when (and align-prompts ;; t means the same as :right
              (not (eq align-prompts :left)))
        (setf align-prompts :right))
    (multiple-value-bind (cx cy) (stream-cursor-position stream)
        (let* ((return-values nil)
                  (frame (make-application-frame frame-class
                             :calling-frame *application-frame*
                             :stream stream
                             :exit-boxes exit-boxes
                             :top-level-sheet (frame-top-level-sheet
                                                  *application-frame*)))
                  (*accepting-values-stream*
                      (make-instance 'accepting-values-stream
                          :stream stream
                          :align-prompts align-prompts))
                  (arecord (updating-output (stream :record-type 'accepting-values-record)
                               (when label
                                   (format stream label)
                                   (terpri stream))
                               (if align-prompts
                                   (formatting-table (stream)
                                       #1=(setf return-values
                                              (multiple-value-list
                                                  (funcall body *accepting-values-stream*))))
                                   #1#)
                               (unless (queries *accepting-values-stream*)
                                   (cerror "Exit returning body values."
                                       "~s must contain at least one call to ~s."
                                       'accepting-values 'accept)
                                   (return-from invoke-accepting-values return-values))
                               (display-exit-boxes frame
                                   stream
                                   (stream-default-view
                                       *accepting-values-stream*))))
                  (first-time t)
                  (current-command (if initially-select-p
                                       `(com-select-query
                                            ,initially-select-query-identifier)
                                       `(com-select-query
                                            ,(query-identifier 
                                                 (first
                                                     (queries *accepting-values-stream*))))))
                  (*accelerator-gestures* (compute-inherited-keystrokes command-table)))
            (letf (((frame-command-table *application-frame*)
                       (find-command-table command-table)))
                (unwind-protect
                    (handler-case
                        (loop
                            (if first-time
                                (setq first-time nil)
                                (when resynchronize-every-pass
                                    (redisplay arecord stream)))
                            (with-input-context
                                ('(command :command-table accept-values))
                                (object)
                                (progn
                                    (when (and select-first-query
                                              (not initially-select-p))
                                        (setf current-command 
                                            `(com-select-query
                                                 ,(query-identifier 
                                                      (first
                                                          (queries *accepting-values-stream*))))
                                            select-first-query nil))
                                    (handler-case
                                        (progn
                                            (apply (command-name current-command)
                                                (command-arguments current-command))
                                            ;; If current command returns without throwing a
                                            ;; command, go back to the default command
                                            (setq current-command *default-command*))
                                        (accelerator-gesture (c)
                                            (let ((command (lookup-keystroke-command-item
                                                               (accelerator-gesture-event c) command-table)))
                                                (if (listp command)
                                                    (setq current-command
                                                        (if (clim:partial-command-p command)
                                                            (funcall clim:*partial-command-parser*
                                                                command-table stream command
                                                                (position clim:*unsupplied-argument-marker* command))
                                                            command))
                                                    ;; may be it is a gesture of the frame's command-table
                                                    (signal c))))))
                                (t (setq current-command object)))
                            (redisplay arecord stream))
                        (av-exit ()
                            (finalize-query-records *accepting-values-stream*)
                            (setf (last-pass *accepting-values-stream*) t)
                            (redisplay arecord stream)))
                    (dolist (query (queries *accepting-values-stream*))
                        (finalize (editing-stream (record query)) nil))
                    (erase-output-record arecord stream)
                    (setf (stream-cursor-position stream)
                        (values cx cy))))
            (apply 'values return-values))))
(defmethod display-exit-boxes ((frame accept-values) stream (view textual-dialog-view))
    (declare (ignorable frame))
    (updating-output (stream :unique-id 'buttons :cache-value t)
        (fresh-line stream)
        (formatting-table (stream)
            (formatting-row (stream)
                (dolist (i (slot-value frame 'exit-boxes))
                    (formatting-cell (stream)
                        (with-output-as-presentation (stream nil (cond ((eql (car i) :exit)
                                                                           'exit-button)
                                                                     ((eql (car i) :abort)
                                                                         'abort-button)))
                            (surrounding-output-with-border
                                (stream :shape :rounded :radius 6
                                    :background +gray80+ :highlight-background +gray90+)
                                (format stream (cadr i))))))))
        (terpri stream)))


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
                            (yadfa:intro-function query-io))
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
