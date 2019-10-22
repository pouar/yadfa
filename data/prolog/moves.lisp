(in-package :yadfa-moves)
(defgeneric pants% (target user self)
  (:method (target user self)
    (declare (ignore self))
    (format t "~a tries to pants ~a~%" (name-of user) (name-of target))
    (format t "The attack has no effect on ~a~%" (name-of target)))
  (:method ((target pantsable-character) user self)
    (declare (ignore self))
    (let* ((pants (filter-items (wear-of target) '(or pants skirt dress)))
           (stat
             (when pants
               (iter (for i in (wear-of target))
                 (when (typep i '(or tabbed-briefs pullon))
                   (let ((severity (cond ((and (> (sogginess-of i) 300) (> (messiness-of i) 4000))
                                          'both)
                                         ((> (messiness-of i) 4000)
                                          'messy)
                                         ((> (sogginess-of i) 300)
                                          'soggy)))
                         (padding i))
                     (leave `(padding ,padding severity ,severity)))))))
           (old-condition (find 'yadfa-status-conditions:pantsed (getf (status-conditions-of *battle*) target)
                                :test (lambda (o e)
                                        (typep e o)))))
      (if stat
          (progn
            (cond ((filter-items pants '(or pants skirt))
                   (format t "~a pantses ~a~%"
                           (name-of user)
                           (name-of target)))
                  ((filter-items pants 'dress)
                   (format t "~a raises ~a's ~a~%"
                           (name-of user)
                           (name-of target)
                           (name-of (car (filter-items pants 'dress))))))
            (unless old-condition
              (push (make-instance 'yadfa-status-conditions:pantsed) (getf (status-conditions-of *battle*) target)))
            (format t "~a gets a horrified look on ~a face as ~a ~a is exposed to the world~%"
                    (name-of target)
                    (if (malep target) "his" "her")
                    (if (malep target) "his" "her")
                    (cond ((getf stat 'both)
                           (format nil "soggy mushy padding"))
                          ((getf stat 'messy)
                           "messy padding")
                          ((getf stat 'soggy)
                           "soggy padding")
                          (t "padding")))
            (let ((audience (iter (for i in (if (typep target 'enemy)
                                                (enemies-of *battle*)
                                                (team-of *game*)))
                              (unless (eq target i)
                                (collect i)))))
              (when audience
                (format t (if (> (list-length audience) 1)
                              "~a's team mates start laughing at ~a~%"
                              "~a's team mate starts laughing at ~a~%")
                        (name-of target)
                        (if (malep target)
                            "him"
                            "her"))
                (unless old-condition
                  (iter (for i in audience)
                    (set-status-condition 'yadfa-status-conditions:laughing i))))))
          (progn
            (format t "~a tries to pants ~a~%" (name-of user) (name-of target))
            (format t "The attack has no effect on ~a~%" (name-of target)))))))
