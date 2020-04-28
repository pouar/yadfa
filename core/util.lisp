;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-util"; coding: utf-8-unix; -*-
(in-package :yadfa-util)
(defun shl (x width bits)
  "Compute bitwise left shift of @var{X} by @var{BITS} bits, represented on @var{WIDTH} bits"
  (logand (ash x bits)
          (1- (ash 1 width))))
(defun shr (x width bits)
  "Compute bitwise right shift of @var{X} by @var{BITS} bits, represented on @var{WIDTH} bits"
  (logand (ash x (- bits))
          (1- (ash 1 width))))
(defmethod lambda-list ((lambda-exp list))
  (cadr lambda-exp))
(defmethod lambda-list ((lambda-exp function))
  (swank-backend:arglist lambda-exp))
(defmacro do-push (item &rest places)
  (a:once-only (item)
    `(progn ,@(loop for place in places collect `(push ,item ,place)))))
(defun remove-nth (n sequence)
  (remove-if (constantly t) sequence :start n :count 1))
(defun insert (list value n)
  (if (<= n 0)
      (cons value list)
      (cons (car list) (insert (cdr list) value (- n 1)))))
(define-modify-macro insertf (value n) insert)
(declaim (inline substitute/swapped-arguments remove-if/swapped-arguments))
(defun substitute/swapped-arguments (sequence new old &rest keyword-arguments)
  (apply #'substitute new old sequence keyword-arguments))
(define-modify-macro substitutef (new old &rest keyword-arguments)
  substitute/swapped-arguments
  "Modify-macro for @code{SUBSTITUTE}. Sets place designated by the first argument to
the result of calling @code{SUSTITUTE} with @var{OLD}, @var{NEW}, place, and the @var{KEYWORD-ARGUMENTS}.")
(defun remove-if/swapped-arguments (sequence test &rest keyword-arguments)
  (apply #'remove-if test sequence keyword-arguments))
(define-modify-macro removef-if (test &rest keyword-arguments)
  remove-if/swapped-arguments
  "Modify-macro for @code{REMOVE-IF}. Sets place designated by the first argument to
the result of calling @code{REMOVE-IF} with @var{TEST}, place, and the @var{KEYWORD-ARGUMENTS}.")
(defun random-from-range (start end)
  (+ start (random (+ 1 (- end start)))))
(defun type-specifier-p (type-specifier)
  "Returns true if @var{TYPE-SPECIFIER} is a valid type specifier."
  #+sbcl (sb-ext:valid-type-specifier-p type-specifier)
  #+openmcl (ccl:type-specifier-p type-specifier)
  #+ecl (c::valid-type-specifier type-specifier)
  #+clisp (null
           (nth-value 1 (ignore-errors
                         (ext:type-expand type-specifier))))
  ;; If you're wondering why we're parsing it multiple times, it's because CMUCL does this too.
  ;; If CMUCL doesn't trust KERNEL:SPECIFIER-TYPE to get it right the first time, then why should we?
  #+cmucl (not (and (let ((type (kernel:specifier-type type-specifier)))
                      (typep type 'kernel:unknown-type)
                      (typep (kernel:specifier-type (kernel:unknown-type-specifier type)) 'kernel:unknown-type))))
  ;; Managed to get this out of the free version before it crashed trying to startup. Never could get it to work.
  #+lispworks (type:valid-type-specifier-p type-specifier)
  ;; type specifiers are too complicated for me to figure out whether it's valid or not, but this is good enough for this game.
  #-(or sbcl openmcl ecl clisp cmucl lispworks) (or (typep type-specifier
                                                           '(or
                                                             null
                                                             (and symbol (not keyword))
                                                             class))
                                                    (and (listp type-specifier)
                                                         (typep (car type-specifier) '(and symbol (not keyword))))))
(defun coerced-function-p (form)
  "checks whether the type is a lambda expression or function"
  (handler-case (coerce form 'function)
    (type-error () nil)))
(deftype type-specifier ()
  '(satisfies
    type-specifier-p))
(deftype coerced-function ()
  '(satisfies coerced-function-p))
(defmacro append* (&rest args)
  "Variant of @code{APPEND} that also makes a copy of its last argument"
  `(append ,@args nil))
(defmacro lappendf (list &rest args)
  "Modify macro that appends @var{ARGS} at the beginning of @var{LIST} instead of the end. Might be faster."
  (a:once-only (list)
    `(setf ,list (append ,@args ,list))))
(define-modify-macro appendf* (&rest lists) append*
  "Modify-macro for APPEND*. Appends LISTS to the place designated by the first
argument.")
(defmacro out (&rest strings)
  (check-type strings list)
  (alexandria:with-gensyms (element)
    `(dolist (,element (list ,@(substitute #\Newline :% strings :test #'eq)))
       (princ ,element))))
(declaim (inline list-length-< list-length-<=))
(s:eval-always
  (defun list-length-<= (length list)
    (declare (type list list)
             (type integer length))
    (let ((n (1- length)))
      (or (minusp n) (nthcdr n list))))
  (defun list-length-< (length list)
    (declare (type list list)
             (type integer length))
    (list-length-<= (1+ length) list)))
(defmacro defunassert (name-args-declares asserts &body body)
  "Wrapper macro that brings the behavior of SBCL's type declaration to other implementations, @var{NAME-ARGS-DECLARES} is the function name, lambda list, and optionally the docstring and declarations (omitting the type declarations) @var{ASSERTS} is the type specifiers for the lambda list as a plist, @var{BODY} is the body of the function"
  (declare (inline list-length-<))
  (let* ((declarep (and
                    (list-length-< 2 name-args-declares)
                    (typep (car (last name-args-declares)) 'list)
                    (eq (caar (last name-args-declares)) 'declare)))
         (types (multiple-value-bind (req op rest key allow aux keyp)
                    (a:parse-ordinary-lambda-list (second name-args-declares))
                  (declare (ignore rest allow aux keyp))
                  (iter (for i in (append req (mapcar 'car op) (mapcar 'cadar key)))
                    (when (member i (s:plist-keys asserts))
                      (collect `(type ,(getf asserts i) ,i)))))))
    `(defun
         ,@(if declarep
               (butlast name-args-declares)
               name-args-declares)
         ,@`(,(append (if declarep
                       (car (last name-args-declares))
                       '(declare))
               types)
             #-(or sbcl ccl) ,@(multiple-value-bind (req op rest key allow aux keyp)
                                   (parse-ordinary-lambda-list (second name-args-declares))
                                 (declare (ignore rest allow aux keyp))
                                 (iter (for i in (append req (mapcar 'car op) (mapcar 'cadar key)))
                                   (when (member i (plist-keys asserts))
                                     (collect `(check-type ,i ,(if (member i (plist-keys asserts))
                                                                   (getf asserts i)
                                                                   t))))))
             ,@body))))
(defmacro with-standard-io-syntax* (&body body)
  "Like with-standard-io-syntax, but use a readtable that isn't readonly, because apparently some implementations forgot to prevent you from changing this readonly structure causing undefined behavior."
  `(with-standard-io-syntax
     (let ((*readtable* (copy-readtable *readtable*)))
       ,@body)))
