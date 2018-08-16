(in-package :yadfa/events)
(ensure-event test-battle-1
    :lambda '(lambda (self)
                 (declare (ignore self))
                 (format t "Time to battle~%")
                 (set-new-battle
                     '((enemy))
                     :continuable t
                     :win-events (list (gethash 'test-battle-2 (events-of-game))))))
(ensure-event test-battle-3
    :lambda '(lambda (self)
                 (declare (ignore self))
                 (format t "You won~%")))
(ensure-event test-battle-2
    :lambda '(lambda (self)
                 (declare (ignore self))
                 (format t "Time to battle 2~%")
                 (set-new-battle
                     '((enemy) (enemy))
                     :continuable t
                     :win-events (list (gethash 'test-battle-3 (events-of-game))))))
