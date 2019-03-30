(in-package :yadfa-items)
(defclass test-clothing (clothing)
    ()
    (:default-initargs
        :special-actions (list
                             :test (make-action
                                       :documentation "test item"
                                       :lambda '(lambda (item user)
                                                    (declare (ignorable item user))
                                                    (format t "Testing:~a ~a~%" (name-of user) (name-of item)))))))
