(in-package :marshal)
(defmethod unmarshal-fn ((version (eql (coding-idiom :coding-release-no)))
                            (type (eql (coding-idiom :object))) token &optional (circle-hash nil))
    (let* ((package    (find-package (fmt:object-package-name token)))
              (values     (fmt:class-slots-values  token))
              (class-out  (find-class (intern (symbol-name (fmt:object-class-name token))
                                          package)))
              (out        (allocate-instance class-out))
              (slots      (class-persistent-slots  out)))

        (setf (gethash (fmt:id token) circle-hash) out)

        (loop
            for (slot value) on values by #'cddr
            do (when (member slot slots)
                   (if (listp value)
                       (setf (slot-value out slot)
                           (unmarshal-fn version
                               (fmt:data-type value)
                               value
                               circle-hash))
                       (setf (slot-value out slot) (unmarshal-fn version t value circle-hash)))))
        (initialize-unmarshalled-instance out)))
(defmethod marshal ((object standard-object) &optional (circle-hash nil))
    (let* ((class   (class-of object))
              (pslots  (class-persistent-slots object))
              (dummy   nil)
              (outlist nil))
        (setq dummy (getvalue circle-hash object))
        (if dummy
            (setq outlist (list (coding-idiom :reference) dummy))
            (progn
                (when pslots
                    (setq dummy (genkey circle-hash))
                    (setvalue circle-hash object dummy)
                    (setf outlist (list (coding-idiom :object)
                                      dummy
                                      (class-name class)
                                      (intern (package-name (symbol-package (class-name class)))
                                          :keyword)))
                    (dolist (walker pslots)
                        (setq outlist
                            (nconc outlist
                                (list walker (marshal (slot-value object walker)
                                                 circle-hash))))))))
        outlist))
