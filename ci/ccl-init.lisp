(handler-bind ((error #'(lambda (c)
                          (let ((r (find-restart 'continue c)))
                            (when r (invoke-restart r))))))
  (defun ccl::fd-input-available-p (fd &optional milliseconds)
    #+windows-target
    (case (ccl::%unix-fd-kind fd)
      (:socket
       (rlet ((infds #>fd_set)
              (tv :timeval :tv_sec 0 :tv_usec 0))
             (ccl::fd-zero infds)
             (ccl::fd-set fd infds)
             (when (and milliseconds (>= milliseconds 0))
               (multiple-value-bind (seconds millis)
                   (floor milliseconds 1000)
                 (setf (pref tv :timeval.tv_sec) seconds
                       (pref tv :timeval.tv_usec) (* 1000 millis))))
             (let* ((result (#_select 1 infds (%null-ptr) (%null-ptr)
                                      (if (and milliseconds (>= milliseconds 0))
                                          tv (%null-ptr)))))
               (cond ((> result 0) (values t 0))
                     ((= result 0) (values nil 0))
                     (t (values nil (- (#_GetLastError))))))))
      (:pipe (if (ccl::data-available-on-pipe-p fd)
                 (values t 0)
                 (if (and milliseconds (> milliseconds 0))
                     (values (process-wait-with-timeout "input-wait" milliseconds #'ccl::data-available-on-pipe-p fd) 0)
                     (values nil 0))))
      (:file (let* ((curpos (ccl::fd-tell fd))
                    (eofpos (%stack-block ((peofpos 8))
                                          (#_GetFileSizeEx (%int-to-ptr fd) peofpos)
                                          (%%get-unsigned-longlong peofpos 0))))
               (values (< curpos eofpos) 0)))
      ;;(:character-special (windows-tty-input-available-p fd milliseconds))
      
      (t (values nil 0)))
    #-windows-target
    (rlet ((pollfds (:array (:struct :pollfd) 1)))
          (setf (ccl::pref (ccl::paref pollfds (:* (:struct :pollfd)) 0) :pollfd.fd) fd
                (ccl::pref (ccl::paref pollfds (:* (:struct :pollfd)) 0) :pollfd.events) #$POLLIN)
          (let* ((res (ccl::int-errno-call (#_poll pollfds 1 (or milliseconds -1)))))
            (declare (fixnum res))
            (values (> res 0) res)))))

;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

