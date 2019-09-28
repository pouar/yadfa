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
  `(progn ,@(loop for place in places collect `(push ,item ,place))))
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
  #-(or sbcl openmcl ecl clisp) (typep type-specifier
                                       '(or
                                         null
                                         (and symbol (not keyword))
                                         list
                                         class)))
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
(define-modify-macro appendf* (&rest lists) append*
  "Modify-macro for APPEND*. Appends LISTS to the place designated by the first
argument.")
