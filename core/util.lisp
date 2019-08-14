;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa"; coding: utf-8-unix; -*-
(in-package :yadfa-util)
(defmacro defunassert (name-args-declares asserts &body body)
  "Wrapper macro that brings the behavior of SBCL's type declaration to other implementations, NAME-ARGS-DECLARES is the function name, lambda list, and optionally the docstring and declarations (omitting the type declarations) ASSERTS is the type specifiers for the lambda list as a plist, BODY is the body of the function"
  (labels ((get-var (_ var)
             (cond ((eq _ '&key)
                    (cond ((and (typep var 'list)
                                (typep (car var) 'list)
                                (typep (caar var) 'keyword))
                           (cadar var))
                          ((typep var 'list)
                           (car var))
                          (t var)))
                   ((eq _ '&optional)
                    (cond ((typep var 'list)
                           (car var))
                          (t var)))
                   (t var)))
           (list-length-> (list length)
             (declare (type list list)
                      (type length integer))
             (cond ((<= length 0) t)
                   ((cdr list) (list-length-> (cdr list) (1- length)))))
           (check-type% (asserts check-type m j)
             (if check-type
                 `(check-type ,(get-var m j) ,(getf asserts (get-var m j) t))
                 `(type ,(getf asserts (get-var m j) t) ,(get-var m j))))
           (check (name-args-declares asserts check-type)
             (iter
               (with l = nil)
               (with m = nil)
               (for j in (second name-args-declares))
               (setf l (iter (for k in '(&rest &key &optional &allow-other-keys))
                         (when (eq j k)
                           (setf m k)
                           (leave k))))
               (unless l
                 (collect `,(check-type% asserts check-type m j))))))
    (let* ((sbclp (uiop:featurep :sbcl))
           (declarep (and sbclp
                          (list-length-> name-args-declares 2)
                          (typep (car (last name-args-declares)) 'list)
                          (eq (caar (last name-args-declares)) 'declare)))
           (types (check name-args-declares asserts nil)))
      `(defun
           ,@(if declarep
                 (butlast name-args-declares)
                 name-args-declares)
           ,@(if sbclp
              `(,(append (if declarep
                          (car (last name-args-declares))
                          '(declare))
                  types)
                ,@body)
              (append
               (check name-args-declares asserts t)
               `((locally (declare ,@types)
                   ,@body))))))))
(defun shl (x width bits)
  "Compute bitwise left shift of x by 'bits' bits, represented on 'width' bits"
  (logand (ash x bits)
          (1- (ash 1 width))))
(defun shr (x width bits)
  "Compute bitwise right shift of x by 'bits' bits, represented on 'width' bits"
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
  "Modify-macro for SUBSTITUTE. Sets place designated by the first argument to
the result of calling SUSTITUTE with OLD NEW, place, and the KEYWORD-ARGUMENTS.")
(defun remove-if/swapped-arguments (sequence test &rest keyword-arguments)
  (apply #'remove-if test sequence keyword-arguments))
(define-modify-macro removef-if (test &rest keyword-arguments)
  remove-if/swapped-arguments
  "Modify-macro for REMOVE-IF. Sets place designated by the first argument to
the result of calling REMOVE-IF with TEST, place, and the KEYWORD-ARGUMENTS.")
(defun random-from-range (start end)
  (+ start (random (+ 1 (- end start)))))
(defun type-specifier-p (type-specifier)
  "Returns true if TYPE-SPECIFIER is a valid type specifier."
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
(defunassert (list-length-> (list length))
    (list list length integer)
  (cond ((<= length 0) t)
        ((cdr list) (list-length-> (cdr list) (1- length)))))
(defunassert (list-length-<= (list length))
    (list list length integer)
  (not (list-length-> list length)))
(defunassert (list-length->= (list length))
    (list list length integer)
  (list-length-> list (1- length)))
(defunassert (list-length-< (list length))
    (list list length integer)
  (not (list-length->= list length)))
