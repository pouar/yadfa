;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-events"; coding: utf-8-unix; -*-
(in-package :yadfa-events)
(defevent enter-lukurbo-1
  :lambda '(lambda (self)
            (declare (ignore self))
            (out "*You reach a town where you see a bunch of anthros in fursuits*")
            (out (name-of (player-of *game*)) ": OH BOY A PLUSHIE!!!! *glomps one of them*")
            (out "*fursuiter falls backward from being glomped*")
            (out (name-of (player-of *game*)) ": Aren't you adorable?")
            (out "*the fursuiter speaks in charades*")
            (out (name-of (player-of *game*)) ": You don't talk much do you?")
            (out "Random citizen: They're mute, much like the mascots you see in theme parks. Hi, welcome to Lukurbo. We're world famous for our padded servants here, and yes, they double as plushies.")
            (out  "*You decide to keep one as a pet*")
            (finish-output)
            (format t "~a: I'm gonna call you ~a *snuggles*~%~%"
             (name-of (player-of *game*))
             (name-of (accept-with-frame-resolved (clim:accepting-values (*query-io* :resynchronize-every-pass t :exit-boxes '((:exit "Accept")))
                                                    (fresh-line *query-io*)
                                                    (let ((a (make-instance 'yadfa-allies:furry
                                                                            :name (clim:accept 'string
                                                                                               :prompt "Fursuiter Name"
                                                                                               :default (second
                                                                                                         (assoc :name (progn
                                                                                                                        (c2mop:ensure-finalized
                                                                                                                         (find-class 'yadfa-allies:furry))
                                                                                                                        (c2mop:compute-default-initargs
                                                                                                                         (find-class 'yadfa-allies:furry)))))
                                                                                               :view clim:+text-field-view+
                                                                                               :stream *query-io*))))
                                                      (do-push a (team-of *game*) (allies-of *game*))
                                                      a)))))))
