(in-package :clim-listener)
;;;; because it was quicker and easier than trying to write one of these myself from scratch

(clim:define-command-table clim-listener::yadfa-commands)
(clim:define-command-table clim-listener::listener :inherit-from (clim-listener::application-commands
                                                                clim-listener::lisp-commands
                                                                clim-listener::asdf-commands
                                                                clim-listener::filesystem-commands
                                                                clim-listener::show-commands
                                                                clim-listener::yadfa-commands)
    :menu (("Listener"   :menu clim-listener::application-commands)
              ("Lisp"       :menu clim-listener::lisp-commands)
              ("Filesystem" :menu clim-listener::filesystem-commands)
              ("Show"       :menu clim-listener::show-commands)
              ("Yadfa" :menu clim-listener::yadfa-commands)))
;(clim-listener::define-listener-command (yadfa-command :menu ("Yadfa" . (:after "Yadfa"))) ((test 'string :prompt "test2")) (print test))
;(clim-listener::add-menu-item-to-command-table (clim-listener::find-command-table 'yadfa-commands) "Yadfa" 'string "2")
(clim:define-command (yadfa-set-player :command-table clim-listener::yadfa-commands :menu "Set Player")
    ((name 'string
         :prompt "Name")
        (gender 'boolean
            :prompt "Is Male")
        (species 'string
            :prompt "Species"))
    (yadfa/bin::set-player name gender species))
