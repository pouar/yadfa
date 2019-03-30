(in-package :yadfa-moves)
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
                             (set-status-condition 'yadfa-status-conditions:mushed target))))))
