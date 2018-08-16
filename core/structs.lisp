(in-package :yadfa)
(defstruct event
    "An event in the game"
    (id nil :type symbol) (lambda '(lambda (self))) (repeatable nil :type boolean))
(defstruct action
    "An action for a prop or item"
    (documentation nil :type simple-string) (lambda '(lambda (prop))))
