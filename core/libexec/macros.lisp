;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa"; coding: utf-8-unix; -*-
(in-package :yadfa)
(defmacro handle-query (bindings (stream &rest forms) &body body)
  "Macro used to prompt the user for input using restarts when the user enters the wrong input. @var{FORMS} is a list containing the lambda list @code{(case (&optional set-value) &key (error-text \"\") (prompt-text \"\"))}"
  (a:with-gensyms (tag block)
    `(block ,block
       (tagbody
          ,tag
          (let* ,bindings
            ,@(iter (for form in forms)
                (collect (a:with-gensyms (value)
                           (destructuring-bind (case (&optional set-value) &key (error-text "") (prompt-text ""))
                               form
                             `(restart-case (when ,case
                                              (error 'invalid-user-input :format-control ,error-text))
                                ,@(when set-value
                                    `((use-value (,value)
                                                 :interactive (lambda ()
                                                                (if clim:*application-frame*
                                                                    ;; For some reason McCLIM does not echo when using CL:READ on the
                                                                    ;; Listener's standard input until CL:READ returns. CLIM:ACCEPT otoh
                                                                    ;; actually does, so let's use that with McCLIM instead.
                                                                    (list (eval (clim:accept 'clim:expression
                                                                                             :stream ,stream
                                                                                             :prompt ,prompt-text)))
                                                                    (progn
                                                                      (format ,stream "~s: " ,prompt-text)
                                                                      (list (eval (read ,stream))))))
                                                 :report ,prompt-text
                                                 (setf ,set-value ,value)
                                                 (go ,tag)))))))))
            (return-from ,block (progn ,@body)))))))
(defmacro defmatch (source target &body return)
  (flet ((arg (arg sym)
           (typecase arg
             ((and list (not null)) arg)
             (null (a:make-gensym sym))
             ((eql t) `(,(a:make-gensym sym) element-type))
             ((and symbol (not keyword)) `(,(a:make-gensym sym) ,arg))
             (t (error 'simple-error :format-control "Invalid argument ~s" :format-arguments `(,arg))))))
    `(defmethod type-match (,(arg source 'source)
                            ,(arg target 'target))
       ,@return)))
(defmacro define-type (name (&rest superclasses) (&rest slot-specifiers) &rest class-options)
  `(progn (s:eval-always (defclass ,name (,@superclasses element-type) ,slot-specifiers
                           (:metaclass element-type-class)
                           ,@(iter (for class-option in class-options)
                               (unless (s:memq (car class-option) '(:super-effective :not-very-effective :no-effect :element-name))
                                 (collect class-option)))))
          (or (gethash ',name *element-types*)
              (setf (gethash ',name *element-types*) (make-instance ',name)))
          ,@(iter (for class-option in class-options)
              (let ((option-name (car class-option)))
                (when (s:memq option-name '(:super-effective :not-very-effective :no-effect))
                  (appending (iter (for target in (cdr class-option))
                               (collect `(s:eval-always (unless (find-class ',target nil)
                                                          (defclass ,target (element-type) () (:metaclass element-type-class)))))
                               (collect `(defmatch ,name ,target ,option-name)))))
                (collect `(setf (slot-value (find-class ',name) 'name) ,(if (eq option-name :element-name)
                                                                            (second class-option)
                                                                            nil)))))
          (find-class ',name)))
(defmacro accept-with-effective-frame (&body body)
  `(cond
     (c:*application-frame*
      ,@body)
     (t (clim:run-frame-top-level (clim:make-application-frame 'yadfa-clim::emacs-frame
                                                               :width 1024 :height 768
                                                               :emacs-frame-lambda (lambda (frame)
                                                                                     (let ((*query-io* (clim:frame-query-io frame)))
                                                                                       ,@body)))))))
(defmacro with-effective-frame (&body body)
  `(cond
     (c:*application-frame*
      ,@body)
     (t (clim:run-frame-top-level (clim:make-application-frame 'yadfa-clim::emacs-frame
                                                               :width 1024 :height 768
                                                               :emacs-frame-lambda (lambda (frame)
                                                                                     (let ((*query-io* (clim:frame-query-io frame)))
                                                                                       ,@body
                                                                                       (loop until (eql #\Return (c:read-gesture
                                                                                                                  :stream *query-io*))))))))))
(defmacro present-with-effective-frame (&body body)
  `(cond
     (c:*application-frame*
      (push (clim:updating-output (*query-io*)
              ,@body)
            yadfa-clim::*records*))
     (t (clim:run-frame-top-level (clim:make-application-frame 'yadfa-clim::emacs-frame
                                                               :width 1024 :height 768
                                                               :emacs-frame-lambda (lambda (frame)
                                                                                     (let ((*query-io* (clim:frame-query-io frame)))
                                                                                       ,@body
                                                                                       (loop until (eql #\Return (c:read-gesture
                                                                                                                  :stream *query-io*))))))))))
(defmacro updating-present-with-effective-frame
    ((stream
      &key (unique-id nil unique-id-supplied-p) (id-test nil id-test-supplied-p)
           (cache-value nil cache-value-supplied-p)
           (cache-test nil cache-test-supplied-p)
           (fixed-position nil fixed-position-supplied-p)
           (all-new nil all-new-supplied-p)
           (parent-cache nil parent-cache-supplied-p)
           (record-type nil record-type-supplied-p)
      &allow-other-keys) &body body)
  `(cond
     (c:*application-frame*
      (push (clim:updating-output (,stream ,@(and unique-id-supplied-p `(:unique-id ,unique-id)) ,@(and id-test-supplied-p `(:id-test ,id-test))
                                   ,@(and cache-value-supplied-p `(:cache-value ,cache-value)) ,@(and cache-test-supplied-p `(:cache-test ,cache-test))
                                   ,@(and fixed-position-supplied-p `(:fixed-position ,fixed-position)) ,@(and all-new-supplied-p `(:all-new ,all-new))
                                   ,@(and parent-cache-supplied-p `(:parent-cache ,parent-cache))
                                   ,@(and record-type-supplied-p (and `(:record-type ,record-type))))
              ,@body)
            yadfa-clim::*records*))
     (t (clim:run-frame-top-level (clim:make-application-frame 'yadfa-clim::emacs-frame
                                                               :width 1024 :height 768
                                                               :emacs-frame-lambda (lambda (frame)
                                                                                     (let ((*query-io* (clim:frame-query-io frame)))
                                                                                       ,@body
                                                                                       (loop until (eql #\Return (c:read-gesture
                                                                                                                  :stream *query-io*))))))))))
(defmacro defevent (event-id &rest args)
  `(progn
     (setf (gethash ',event-id *events*) (make-instance 'event :id ',event-id ,@args))
     ',event-id))
(defmacro defonesie (base-class direct-superclasses &body body)
  #.(format nil "macro that generates the classes and methods of the onesie used to open and close the snaps of them. method used to toggle the onesie is @code{TOGGLE-ONESIE}. @var{BASE-CLASS} is the name of the class you want to give the onesie.@var{DIRECT-SUPERCLASSES} are the direct superclasses of @var{BASE-CLASS} (obviously). @var{BODY} is the slot specifier and class options of @var{BASE-CLASS}

~a."
            (xref yadfa-bin:toggle-onesie :function))
  `(progn
     (defclass ,(a:format-symbol (symbol-package base-class) "~a" (symbol-name base-class))
         ,(if (iter (for i in direct-superclasses)
                (when (subtypep i 'yadfa:onesie)
                  (leave t)))
              direct-superclasses
              `(yadfa:onesie ,@direct-superclasses))
       ,@body)
     (defclass ,(a:format-symbol (symbol-package base-class) "~a/OPENED" (symbol-name base-class))
         (,(a:format-symbol (symbol-package base-class) "~a" (symbol-name base-class))
          yadfa:onesie/opened) ())
     (defclass ,(a:format-symbol (symbol-package base-class) "~a/CLOSED" (symbol-name base-class))
         (,(a:format-symbol (symbol-package base-class) "~a" (symbol-name base-class))
          yadfa:onesie/closed) ())
     (export '(,(a:format-symbol (symbol-package base-class) "~a" (symbol-name base-class))
               ,(a:format-symbol (symbol-package base-class) "~a/OPENED" (symbol-name base-class))
               ,(a:format-symbol (symbol-package base-class) (format nil "~a/CLOSED" (symbol-name base-class))))
             ,(symbol-package base-class))
     (defmethod toggle-onesie% ((self ,(a:format-symbol (symbol-package base-class) "~a/OPENED" (symbol-name base-class))))
       (change-class self ',(a:format-symbol (symbol-package base-class) "~a/CLOSED" (symbol-name base-class))))
     (defmethod toggle-onesie% ((self ,(a:format-symbol (symbol-package base-class) "~a/CLOSED" (symbol-name base-class))))
       (change-class self ',(a:format-symbol (symbol-package base-class) "~a/OPENED" (symbol-name base-class))))))
(defmacro ensure-zone (position &body body)
  #.(format nil "defines the classes of the zones and adds an instance of them to the game's map hash table if it's not already there

~a, ~a, ~a."
            (xref defzone :macro) (xref defzone* :macro) (xref ensure-zone* :macro))
  (declare (type list position))
  #-(or sbcl ccl) (check-type position list)
  `(progn (unless (get-zone ',position)
            (setf (get-zone ',position)
                  (make-instance 'zone ,@body)))
          (export ',(fourth position) ',(symbol-package (fourth position)))
          (get-zone ',position)))
(defmacro defzone (position &body body)
  #.(format nil "defines the classes of the zones and adds an instance of them to the game's map hash table. Intended to be used to replace existing zones in more intrusive mods. Best to wrap this in an event and run @code{TRIGGER-EVENT} so it doesn't overwrite the zone every time this piece of code is loaded

~a, ~a, ~a, ~a."
            (xref defzone* :macro) (xref ensure-zone :macro) (xref ensure-zone* :macro) (xref trigger-event :function))
  (declare (type list position))
  #-(or sbcl ccl)
  (check-type position list)
  `(progn
     (setf (get-zone ',position)
           (make-instance 'zone ,@body))
     (export ',(fourth position) ',(symbol-package (fourth position)))
     (get-zone ',position)))
(defmacro ensure-zone* (position &body body)
  #.(format nil "Like @code{ENSURE-ZONE}, but position is a quoted list

~a, ~a, ~a."
            (xref defzone :macro) (xref defzone* :macro) (xref ensure-zone :macro))
  (declare (type list position))
  #-(or sbcl ccl)
  (check-type position list)
  `(progn (unless (get-zone ,position)
            (setf (get-zone ,position)
                  (make-instance 'zone ,@body)))
          (export ',(fourth position) ',(symbol-package (fourth position)))
          (get-zone ,position)))
(defmacro defzone* (position &body body)
  #.(format nil "Like @code{DEFZONE}, but position is a quoted list

~a, ~a, ~a."
            (xref defzone :macro) (xref ensure-zone :macro) (xref ensure-zone* :macro))
  (declare (type list position))
  #-(or sbcl ccl)
  (check-type position list)
  `(progn
     (setf (get-zone ,position)
           (make-instance 'zone ,@body))
     (export ',(fourth position) ',(symbol-package (fourth position)))
     (get-zone ,position)))
(defmacro make-pocket-zone (position &body body)
  "defines the classes of the zones and adds an instance of them to the game's map hash table if it's not already there"
  (declare (type list position))
  #-(or sbcl ccl)
  (check-type position list)
  `(setf (get-zone '(,@position :pocket-map))
         (make-instance 'zone ,@body)))
