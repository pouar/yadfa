#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                          (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
        (load quicklisp-init)))
(ql:quickload :drakma)
(ql:quickload :cl-json)
(setf drakma:*body-format-function* (lambda (a b) :utf-8))
(loop for i in (let*
                   ((stream
                        (drakma:http-request "https://api.bintray.com/packages/pouar/yadfa-flatpak/repo"
                            :basic-authorization (list "pouar" (uiop:getenv "KEY"))
                            :want-stream t))
                       (json (cl-json:decode-json stream)))
                   (close stream)
                   (cddr (assoc :versions json)))
    do
    (drakma:http-request
        (format nil "https://api.bintray.com/packages/pouar/yadfa-flatpak/repo/versions/~a" i)
        :basic-authorization (list "pouar" (uiop:getenv "BINTRAYKEY"))
        :method :delete))
