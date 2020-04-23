(in-package #:yadfa.furaffinity.pouar.myawesomemod)
(defun test () (print 1))
(y:defevent test-4
  :lambda (lambda (self)
            (declare (ignore self))
            (format t "Print some text~%")))
(y:defevent test-5
  :lambda (lambda (self)
            (declare (ignore self))
            (setf
             (getf (y:actions-of (getf (y:props-of (y:get-zone '(0 0 0 yz:debug-map))) :shop)) :mod)
             (y:make-action
              :documentation "This Mod Works"
              :lambda (lambda
                          (prop &rest keys &key &allow-other-keys)
                        (declare (ignore prop keys))
                        (format t "Hello World~%"))))))
(y:trigger-event 'test-5)
(pushnew 'test-4 (y:events-of (y:get-zone '(0 0 -1 yz:debug-map))))
(defclass celebrate (y:stat/move) ()
  (:default-initargs
   :description "The Pokémon congratulates you on your special day!"
   :name "Celebrate"
   :attack '(lambda (target user self)
             (let ((a (y:calculate-damage target user (y:power-of self))))
               (format t "~a used ~a~%" (y:name-of user) (y:name-of self))
               (format t "Congratulations ~a!~%" (y:name-of (y:player-of *game*)))))))
