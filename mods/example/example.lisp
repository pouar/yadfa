(in-package #:yadfa.furaffinity.pouar.myawesomemod)
(defun test () (print 1))
(defevent test-4
    :lambda '(lambda (self)
                 (declare (ignore self))
                 (format t "Print some text~%")))
(defevent test-5
    :lambda '(lambda (self)
                 (declare (ignore self))
                 (setf
                     (getf (actions-of (getf (props-of (get-zone '(0 0 0 yadfa-zones:debug-map))) :shop)) :mod)
                     (make-action
                         :documentation "This Mod Works"
                         :lambda '(lambda
                                      (prop &rest keys &key &allow-other-keys)
                                      (declare (ignore prop))
                                      (format t "Hello World~%"))))))
(trigger-event 'test-5)
(pushnew 'test-4 (events-of (get-zone '(0 0 -1 yadfa-zones:debug-map))))
(defclass celebrate (stat/move) ()
    (:default-initargs
        :description "The Pokémon congratulates you on your special day!"
        :name "Celebrate"
        :attack '(lambda (target user self)
                     (let ((a (calculate-damage target user (power-of self))))
                         (format t "~a used ~a~%" (name-of user) (name-of self))
                         (format t "Congratulations ~a!~%" (name-of (player-of *game*)))))))
