(in-package :yadfa-events)
(defevent test-battle-1
    :lambda '(lambda (self)
                 (declare (ignore self))
                 (format t "Time to battle~%")
                 (set-new-battle
                     '((enemy))
                     :continuable t
                     :win-events '(test-battle-2))))
(defevent test-battle-3
    :lambda '(lambda (self)
                 (declare (ignore self))
                 (format t "You won~%")))
(defevent test-battle-2
    :lambda '(lambda (self)
                 (declare (ignore self))
                 (format t "Time to battle 2~%")
                 (set-new-battle
                     '((enemy) (enemy))
                     :continuable t
                     :win-events '(test-battle-3))))
