;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-events"; coding: utf-8-unix; -*-
(in-package :yadfa-events)
(defevent test-battle-1
  :lambda (lambda (self)
            (declare (ignore self))
            (f:fmt t "Time to battle" #\Newline)
            (set-new-battle
             '((enemy))
             :continuable t
             :win-events '(test-battle-2))))
(defevent test-battle-3
  :lambda (lambda (self)
            (declare (ignore self))
            (f:fmt t  "You won" #\Newline)))
(defevent test-battle-2
  :lambda (lambda (self)
            (declare (ignore self))
            (f:fmt t "Time to battle 2" #\Newline)
            (set-new-battle
             '((enemy) (enemy))
             :continuable t
             :win-events '(test-battle-3))))
