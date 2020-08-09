;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-enemies"; coding: utf-8-unix; -*-
(in-package :yadfa-enemies)
(defclass padded-fursuiter-servant (potty-enemy) ()
  (:default-initargs
   :name "Padded Fursuiter Servant"
   :description "These are basically generic \"servants\" that you can also use as a plushie. Since they're not allowed to take bathroom breaks, they're thickly padded and have special fursuits that keep all the fluids and smells in. Some are too embarrassed to use their diapers for their intended purposes and try so hard to hold it in only to uncontrollably flood and mess themselves. Other's have given up and just use their diapers whenever they have to go."
   :male (a:random-elt '(t nil))
   :species "Fox"
   :bladder/contents (random 500)
   :bowels/contents (random 700)
   :wear (make-instances yadfa-items:fursuit yadfa-items:kurikia-thick-diaper)))
(defmethod process-battle-accident ((character padded-fursuiter-servant) attack (item item) reload (selected-target base-character))
  (declare (ignore attack item reload selected-target))
  (let* ((male (malep character))
         (heshe (if male "he" "she"))
         (himher (if male "him" "her")))
    (cond ((or (>= (bladder/contents-of character)
                   (bladder/maximum-limit-of character))
               (>= (bowels/contents-of character) (bowels/maximum-limit-of character)))
           (when (>= (bladder/contents-of character) (bladder/maximum-limit-of character))
             (format t "~a lets out a quiet moan as ~a accidentally wets ~aself in battle~%"
                     (name-of character)
                     heshe
                     himher)
             (wet :wetter character)
             (set-status-condition 'yadfa-status-conditions:wetting character))
           (when (>= (bowels/contents-of character) (bowels/maximum-limit-of character))
             (format t "~a involuntarily squats down as ~a accidentally messes ~aself in battle~%"
                     (name-of character)
                     heshe
                     himher)
             (mess :messer character)
             (set-status-condition 'yadfa-status-conditions:messing character))
           t)
          ((and (watersport-limit-of character)
                (<= (- (bladder/maximum-limit-of character) (bladder/contents-of character)) (watersport-limit-of character))
                (< (random (watersport-chance-of character)) 1))
           (format t "~a floods ~aself in the middle of battle~%" (name-of character) himher)
           (wet :wetter character))
          ((and (mudsport-limit-of character)
                (<= (- (bowels/maximum-limit-of character) (bowels/contents-of character)) (mudsport-limit-of character))
                (< (random (mudsport-chance-of character)) 1))
           (format t "~a squats down and messes ~aself in the middle of battle~%" (name-of character) himher)
           (mess :messer character)))))
(defmethod initialize-instance :after
    ((c padded-fursuiter-servant) &rest args &key &allow-other-keys)
  (destructuring-bind (&key (watersport-limit nil watersportp) (mudsport-limit nil mudsportp) &allow-other-keys)
      args
    (declare (ignore watersport-limit mudsport-limit))
    (cond ((and watersportp mudsportp)
           (let ((limits (a:random-elt (list (cons (bladder/need-to-potty-limit-of c) (bowels/need-to-potty-limit-of c)) '(nil)))))
             (setf (watersport-limit-of c) (car limits) (mudsport-limit-of c) (cdr limits))))
          (watersportp
           (setf (mudsport-limit-of c) (a:random-elt (list (bowels/need-to-potty-limit-of c) nil))))
          (mudsportp
           (setf (watersport-limit-of c) (a:random-elt (list (bladder/need-to-potty-limit-of c) nil)))))))
(defclass fursuiter-servant (potty-enemy) ()
  (:default-initargs
   :name "Fursuiter Servant"
   :description "Claims that he \"doesn't want to be associated with diapers\" and that he will \"sweat all the fluids out\", so he's kept in one of those watertight fursuits to keep him from making puddles until he changes his mind."
   :male (a:random-elt '(t nil))
   :species "Fox"
   :bladder/contents (random 500)
   :bowels/contents (random 700)
   :wear (make-instances yadfa-items:watertight-fursuit yadfa-items:tshirt yadfa-items:jeans yadfa-items:boxers)))
