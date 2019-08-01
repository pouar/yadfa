(ql:quickload :yadfa)
(ql:quickload :plump-sexp)
(with-open-file (r (uiop:merge-pathnames*
                    (make-pathname :name "net.pouar.yadfa.appdata.xml" :type "lisp")
                    (translate-logical-pathname "yadfa:home;flatpak;"))
                   :direction :input)
  (with-open-file (s (uiop:merge-pathnames*
                      (make-pathname :name "net.pouar.yadfa.appdata" :type "xml")
                      (translate-logical-pathname "yadfa:home;flatpak;"))
                     :direction :output
                     :if-exists :supersede)
    (plump:serialize (plump-sexp:parse (read r)) s)))
