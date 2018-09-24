(in-package :clim-listener)
;;;; because it was quicker and easier than trying to write one of these myself from scratch

(clim:define-command-table yadfa-commands)
(clim:define-command (yadfa-set-player :command-table yadfa-commands :menu "Set Player")
    ((name 'string
         :prompt "Name"
         :default (yadfa:name-of (yadfa:player-of yadfa:*game*)))
        (gender 'boolean
            :prompt "Is Male"
            :default (yadfa:malep (yadfa:player-of yadfa:*game*)))
        (species 'string
            :prompt "Species"
            :default (yadfa:species-of (yadfa:player-of yadfa:*game*))))
    (yadfa/bin::set-player name gender species))
(clim:add-menu-item-to-command-table (clim:find-command-table 'listener) "Yadfa" :menu (clim:find-command-table 'yadfa-commands))
(pushnew (clim:find-command-table 'yadfa-commands) (clim:command-table-inherit-from (clim:find-command-table 'listener)))
