;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-events"; coding: utf-8-unix; -*-
(in-package :yadfa-events)
(defevent enter-lukurbo-1
  :lambda '(lambda (self)
            (declare (ignore self))
            (write-line "*You reach a town where you see a bunch of anthros in fursuits*")
            (format t "~a: OH BOY A PLUSHIE!!!! *glomps one of them*~%" (name-of (player-of *game*)))
            (write-line "*fursuiter falls backward from being glomped*")
            (format t "~a: Aren't you adorable?~%" (name-of (player-of *game*)))
            (write-line "*the fursuiter speaks in charades*")
            (format t "~a: You don't talk much do you?~%" (name-of (player-of *game*)))
            (write-line "Random citizen: They're mute, much like the mascots you see in theme parks. Hi, welcome to Lukurbo. We're world famous for our padded servants here, and yes, they double as plushies.")
            (write-line "*You decide to keep one as a pet*")
            (accept-with-frame-resolved (clim:accepting-values (*query-io* :resynchronize-every-pass t :exit-boxes '((:exit "Accept")))
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
                                            (format t "~a: I'm gonna call you ~a *snuggles*~%" (name-of (player-of *game*)) (name-of a)))))))
