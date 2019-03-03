(in-package :marshal)
(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
                            (type (eql (coding-idiom :object))) token &optional (circle-hash nil))
    (let* ((package    (find-package (fmt:object-package-name token)))
              (values     (fmt:class-slots-values  token))
              (class-out  (find-class (intern (symbol-name (fmt:object-class-name token))
                                          package)))
              (out        (allocate-instance class-out))
              (slots      (class-persistent-slots  out)))

        (setf (gethash (fmt:id token) circle-hash) out)

        (loop
            for (slot value) on values by #'cddr
            do (when (member slot slots)
                   (if (listp value)
                       (setf (slot-value out slot)
                           (unmarshal-fn version
                               (fmt:data-type value)
                               value
                               circle-hash))
                       (setf (slot-value out slot) (unmarshal-fn version t value circle-hash)))))
        (initialize-unmarshalled-instance out)))
(defmethod marshal ((object standard-object) &optional (circle-hash nil))
    (let* ((class   (class-of object))
              (pslots  (class-persistent-slots object))
              (dummy   nil)
              (outlist nil))
        (setq dummy (getvalue circle-hash object))
        (if dummy
            (setq outlist (list (coding-idiom :reference) dummy))
            (progn
                (when pslots
                    (setq dummy (genkey circle-hash))
                    (setvalue circle-hash object dummy)
                    (setf outlist (list (coding-idiom :object)
                                      dummy
                                      (class-name class)
                                      (intern (package-name (symbol-package (class-name class)))
                                          :keyword)))
                    (dolist (walker pslots)
                        (setq outlist
                            (nconc outlist
                                (list walker (marshal (slot-value object walker)
                                                 circle-hash))))))))
        outlist))

(in-package :climi)

(in-package :clim-listener)
;;;; because it was quicker and easier than trying to write one of these myself from scratch
(macro-level:macro-level
    `(setf *default-text-style*
         (make-text-style ,@(if (member :mcclim-ffi-freetype *features*) '("DejaVu Sans Mono" "Book") '(:fix :roman)) :normal)))

(define-command (com-clear-output :name "Clear Output History"
                    :command-table application-commands
                    :menu t
                    :provide-output-destination-keyword nil)
    ()
    (window-clear *standard-output*)
    (setf yadfa::*records* ()))

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
                            (loop for i in yadfa:*records* do (redisplay i *standard-output*))
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
