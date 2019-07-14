(in-package :yadfa)
(defstruct event
  "An event in the game"
  (id nil
   :type symbol)
  (lambda '(lambda (self)
            (declare (ignore self)) nil)
    :type (or list function))
  (predicate '(lambda (self)
               (declare (ignore self)) t)
   :type (or list function))
  (repeatable nil :type boolean)
  (major nil :type boolean)
  (major-depends nil :type symbol)
  (optional nil)
  (finished-depends '() :type list)
  (attributes ()))
(defstruct action
  "An action for a prop or item"
  (documentation nil :type simple-string)
  (attributes ())
  (lambda '(lambda (prop))
    :type (or list function)))
