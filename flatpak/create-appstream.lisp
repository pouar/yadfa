(ql:quickload :plump-sexp)
(with-open-file (r (uiop:merge-pathnames*
                    (make-pathname :directory '(:relative "flatpak") :name "net.pouar.yadfa.appdata.xml" :type "lisp")
                    (asdf:component-pathname (asdf:find-system "yadfa")))
                   :direction :input)
  (with-open-file (s (uiop:merge-pathnames*
                      (make-pathname :directory '(:relative "flatpak") :name "net.pouar.yadfa.appdata" :type "xml")
                      (asdf:component-pathname (asdf:find-system "yadfa")))
                     :direction :output
                     :if-exists :supersede)
    (plump:serialize (plump-sexp:parse (read r)) s)))
