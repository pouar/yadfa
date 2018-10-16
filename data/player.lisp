(in-package :yadfa)
(unless (player-of *game*)
    (setf (player-of *game*) (make-instance 'player)))
