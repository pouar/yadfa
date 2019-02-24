(in-package :yadfa/moves)
(defclass default (stat/move) ()
    (:default-initargs
        :description "Default attack when not wielding a weapon, usually doesn't show up in your move list"
        :power 40
        :attack '(lambda (target user self)
                     (let ((a (calculate-damage target user (power-of self))))
                         (format t "~a attacks ~a~%" (name-of user) (name-of target))
                         (decf (health-of target) a)
                         (format t "~a received ~a damage~%" (name-of target) a)))))
(defclass mush (stat/move) ()
    (:default-initargs
        :name "Mush"
        :description "Mush the target's diaper"
        :attack '(lambda (target user self)
                     (declare (ignorable user self))
                     (format t "~a mushes the back of ~a's diaper!~%" (name-of user) (name-of target))
                     (if (<= (getf (calculate-diaper-usage target) :messiness) 0)
                         (format t "But it had no effect!~%")
                         (progn
                             (format t "~a's diaper has been mushed~%" (name-of target))
                             (set-status-condition 'yadfa/status-conditions:mushed target))))))
