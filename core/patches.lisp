;;;; -*- mode: Common-Lisp; coding: utf-8-unix; -*-
(in-package :serialization-format)
(defun array-values (expr)
  (elt expr 6))
(defun array-fill-pointer (expr)
  (elt expr 4))
(defun array-adjustable (expr)
  (elt expr 5))
(in-package :marshal)
(defmethod marshal ((array array) &optional (circle-hash nil))
  (let* ((ckey nil)
         (output nil)
         (dummy nil))
    (setf ckey (getvalue circle-hash array))
    (if ckey
        (setq output (list (coding-idiom :reference) ckey))
        (progn
          (setq ckey (genkey circle-hash))
          (setvalue circle-hash array ckey)
          (setq output (list (coding-idiom :array) ckey
                             (array-dimensions array) (array-element-type array)
                             (when (array-has-fill-pointer-p array)
                               (fill-pointer array))
                             (adjustable-array-p array)))
          (dotimes (walker (array-total-size array))
            (push (marshal (row-major-aref array walker) circle-hash) dummy))
          (setq output (nconc output (list (nreverse dummy))))))
    output))
(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
                         (type (eql (coding-idiom :array))) token &optional (circle-hash nil))
  (let ((out (make-array (fmt:array-sizes token)
                         :element-type (fmt:array-elements-type token)
                         :fill-pointer (fmt::array-fill-pointer token)
                         :adjustable (fmt::array-adjustable token)))
        (elements (fmt:array-values token)))

    (setf (gethash (fmt:id token) circle-hash) out)

    (loop
      for walker in elements
      for e from 0 to (1- (length elements))
      do (if (listp walker)
             (setf (row-major-aref out e)
                   (unmarshal-fn version
                                 (fmt:data-type walker)
                                 walker
                                 circle-hash))
             (setf (row-major-aref out e) (unmarshal-fn version t walker circle-hash))))
    out))
(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
                         (type (eql (coding-idiom :object))) token &optional (circle-hash nil))
  (let* ((package (find-package (fmt:object-package-name token)))
         (values (fmt:class-slots-values  token))
         (class-out (find-class (intern (symbol-name (fmt:object-class-name token)) package)))
         (out (allocate-instance class-out))
         (slots (class-persistent-slots  out)))

    (setf (gethash (fmt:id token) circle-hash) out)

    (loop
      for (slot value) on values by #'cddr
      do (when (member slot slots)
           (if (listp value)
               (setf (slot-value out slot) (unmarshal-fn version (fmt:data-type value) value circle-hash))
               (setf (slot-value out slot) (unmarshal-fn version t value circle-hash)))))
    (initialize-unmarshalled-instance out)))
(defmethod marshal ((object standard-object) &optional (circle-hash nil))
  (let* ((class (class-of object))
         (pslots (class-persistent-slots object))
         (dummy nil)
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
                                (intern (package-name (symbol-package (class-name class))) :keyword)))
            (dolist (walker pslots)
              (setq outlist (nconc outlist (list walker (marshal (slot-value object walker) circle-hash))))))))
    outlist))
(in-package :climi)

;;; the patch I added that makes the FreeType renderer pick the right defaults seems to make it a lot slower
;;; especially now that McCLIM is no longer caching all the fonts because that doesn't work when *default-text-style*
;;; changes. Cache all the fonts again for the FreeType renderer until this gets much faster upstream
#+mcclim-ffi-freetype
(defmethod text-style-mapping :around ((port clim-freetype::clx-freetype-port)
                                       (text-style text-style)
                                       &optional character-set)
  (declare (ignore character-set))
  (ensure-gethash text-style (port-text-style-mappings port) (call-next-method)))

;;; McCLIM is missing the accept-values application class which display-exit-boxes expects as an argument if you want to change what the function displays via :exit-boxes
;;; https://github.com/McCLIM/McCLIM/issues/582
(define-application-frame accept-values ()
  ((stream :initform *query-io* :initarg :stream)
   (body :initform nil :initarg :body)
   (exit-boxes :initform '((:exit "OK") (:abort "Cancel")) :initarg :exit-boxes)
   (select-first-query :initform nil :initarg :selected-first-query)
   (modify-initial-query :initform nil :initarg :modify-initial-query)
   (resize-frame :initform nil :initarg :resize-frame)
   (align-prompts :initform nil :initarg :align-prompts)
   (label :initform nil :initarg :label)
   (scroll-bars :initform nil :initarg :scroll-bars)
   (x-position :initform nil :initarg :x-position)
   (y-position :initform nil :initarg :y-position)
   (foreground :initform nil :initarg :foreground)
   (background :initform nil :initarg :background)
   (text-style :initform nil :initarg :text-style)
   (width :initform nil :initarg :width)
   (height :initform nil :initarg :height)
   (initially-select-query-identifier
    :initform nil :initarg :initially-select-query-identifier)
   (resynchronize-every-pass :initform nil :initarg :resynchronize-every-pass)
   (own-window :initform nil :initarg :own-window)
   (view :initarg :view))
  (:menu-bar nil)
  (:panes (abstract (clim:make-pane 'clim:basic-pane)))
  (:layouts (default abstract))
  (:command-definer t))
(defmethod run-frame-top-level :around ((frame accept-values) &key)
  (letf (((frame-process frame) (current-process)))
    (funcall (frame-top-level-lambda frame) frame)))
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
(defmethod default-frame-top-level
    ((frame accept-values)
     &key command-parser
          command-unparser
          partial-command-parser
          prompt)
  (declare (ignore command-parser command-unparser partial-command-parser prompt))
  ;; Give each pane a fresh start first time through.
  (let* ((stream (slot-value frame 'stream))
         (command-table (frame-command-table frame))
         (align-prompts (slot-value frame 'align-prompts))
         (body (slot-value frame 'body))
         (label (slot-value frame 'label))
         (initially-select-query-identifier (slot-value frame 'initially-select-query-identifier))
         (initially-select-p (slot-value frame 'initially-select-query-identifier))
         (resynchronize-every-pass (slot-value frame 'resynchronize-every-pass))
         (select-first-query (slot-value frame 'select-first-query))
         (own-window (slot-value frame 'own-window))
         (exit-boxes (slot-value frame 'exit-boxes))
         (modify-initial-query (slot-value frame 'modify-initial-query))
         (resize-frame (slot-value frame 'resize-frame))
         (scroll-bars (slot-value frame 'scroll-bars))
         (x-position (slot-value frame 'x-position))
         (y-position (slot-value frame 'y-position))
         (width (slot-value frame 'width))
         (height (slot-value frame 'height)))
    (declare (ignore own-window exit-boxes modify-initial-query
                     resize-frame scroll-bars x-position y-position width height))
    (when (and align-prompts ;; t means the same as :right
               (not (eq align-prompts :left)))
      (setf align-prompts :right))
    (multiple-value-bind (cx cy) (stream-cursor-position stream)
      (let* ((return-values nil)
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
                          (return-from default-frame-top-level return-values))
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
        (apply 'values return-values)))))
(defun invoke-accepting-values
    (stream body
     &rest args
     &key own-window exit-boxes
          (initially-select-query-identifier nil initially-select-p)
          select-first-query
          modify-initial-query resynchronize-every-pass resize-frame
          align-prompts label scroll-bars
          x-position y-position width height
          (command-table 'accept-values)
          (frame-class 'accept-values))
  (declare (ignore own-window exit-boxes modify-initial-query
                   resize-frame scroll-bars x-position y-position width height
                   initially-select-query-identifier initially-select-p
                   select-first-query resynchronize-every-pass align-prompts
                   label command-table))
  (run-frame-top-level (apply #'make-application-frame frame-class
                              :calling-frame *application-frame*
                              :stream stream
                              :body body
                              args)))
#+mcclim-ffi-freetype
(in-package :clim-freetype)
#+mcclim-ffi-freetype
(defun find-best-match (family face)
  (let ((result (mcclim-fontconfig:match-font (append *main-filter*
                                                      (make-family-pattern family)
                                                      (make-face-pattern face))
                                              '(:family :style :file :charset))))
    (list (cdr (assoc :family result))
          (cdr (assoc :style result))
          (cdr (assoc :file result))
          (cdr (assoc :charset result)))))
#+mcclim-ffi-freetype
(defun make-family-pattern (family)
  (list (cond
          ((typep family 'freetype-font-family) `(:family . ,(clim-extensions:font-family-name family)))
          ((stringp family) `(:family . ,family))
          ((eq family :fix) '(:family . "monospace"))
          ((eq family :sans-serif) '(:family . "sans-serif"))
          ((eq family :serif) '(:family . "serif"))
          (t '(:family . "sans-serif")))))
#+mcclim-ffi-freetype
(defun make-face-pattern (face)
  (loop
    for f in (if (listp face) face (list face))
    append (cond
             ((typep f 'freetype-font-face) `(("style" . ,(clim-extensions:font-face-name face))))
             ((stringp face) `((:style . ,face)))
             ((eq f :roman) '((:style . "Regular")))
             ((eq f :bold) '((:style . "Bold")))
             ((eq f :italic) '((:style . "Italic")))
             (t nil))))
(in-package :asdf/output-translations)

;;; https://gitlab.common-lisp.net/asdf/asdf/issues/28
(defun wrapping-output-translations ()
    `(:output-translations
    ;; Some implementations have precompiled ASDF systems,
    ;; so we must disable translations for implementation paths.
      #+(or clasp #|clozure|# ecl mkcl)
      ,@(let ((h (resolve-symlinks* (lisp-implementation-directory))))
          (when h `(((,h ,*wild-path*) ()))))
      #+sbcl ,@(let ((h (resolve-symlinks* (lisp-implementation-directory))))
                 (when (and h (not (eq uiop:*image-dumped-p* :executable))) `(((,h ,*wild-path*) ()))))
      #+mkcl (,(translate-logical-pathname "CONTRIB:") ())
      ;; All-import, here is where we want user stuff to be:
      :inherit-configuration
      ;; These are for convenience, and can be overridden by the user:
      #+abcl (#p"/___jar___file___root___/**/*.*" (:user-cache #p"**/*.*"))
      #+abcl (#p"jar:file:/**/*.jar!/**/*.*" (:function translate-jar-pathname))
      ;; We enable the user cache by default, and here is the place we do:
      :enable-user-cache))
(in-package :yadfa)
(define-condition uwu (simple-error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-line "OOPSIE WOOPSIE!! Uwu We made a fucky wucky!! A wittle fucko boingo!" stream)
             (write-line "The code monkeys at our headquarters are working VEWY HAWD to fix this!" stream))))
