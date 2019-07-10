(in-package :yadfa-events)
(defevent get-diaper-locked-1
  :lambda '(lambda (self)
            (format t "*~a tugs at the tabs trying to remove them, but they won't budge. Better find a solution before its too late*~%~%" (name-of j))))
