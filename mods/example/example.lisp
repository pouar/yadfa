(in-package #:yadfa-mod-example)
(defun test () (print 1))
(ensure-event :test-4
    :lambda '(lambda (self)
                 (declare (ignore self))
                 (format t "Print some text~%")))
(setf-init-hook/zone (0 0 0 "debug") :mod-1
    (setf
        (getf (actions-of (getf (props-of zone) :shop)) :mod)
        (make-action
            :documentation "This Mod Works"
            :lambda '(lambda
                         (prop &rest keys &key &allow-other-keys)
                         (declare (ignore prop))
                         (format t "Hello World~%")))))
(setf-init-hook/zone (0 0 -1 "debug") :mod-2
    (pushnew (gethash :test-4 (events-of-game)) (events-of zone)))
(defclass yadfa-mod-example:celebrate (stat/move) ()
    (:default-initargs
        :description "The Pokémon congratulates you on your special day!"
        :name "Celebrate"
        :attack '(lambda (target user self)
                     (let ((a (calculate-damage target user (power-of self))))
                         (format t "~a used ~a~%" (name-of user) (name-of self))
                         (format t "Congratulations ~a!~%" (name-of (player-of *game*)))))))
(import 'yadfa-mod-example:celebrate :yadfa/moves)
