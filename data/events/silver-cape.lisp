;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-events"; coding: utf-8-unix; -*-
(in-package :yadfa-events)
(defevent enter-silver-cape-1
  :lambda (lambda (self)
            (declare (ignore self))
            (setf (getf (warp-points-of (get-zone '(0 0 0 yadfa-zones:secret-underground))) 'yadfa-zones:silver-cape) '(0 5 0 silver-cape))))
(defevent obtain-pirate-ship-1
  :lambda (lambda (self)
            (declare (ignore self))
            (out "You find a note attached to the ship" :% :%
                 "\"Beyond the dock you will find the Sea Of Parenthesis. A sea so perilous that it is nearly impossible to navigate without the right ship (or at least a pain in the ass). But fear not, here you will find a world famous ship called Emacs. It is the only ship in the game that can sail these perilous waters. Enjoy\"" :% :%)))
(defevent get-location-to-pirate-cove-1
  :lambda (lambda (self)
            (declare (ignore self))
            (out "You see a shark interrogating one of the diapered raccoon bandits sitting in a locked pillory" :% :%
                 "Shark: Tell us where the location of Pirate Cove is!!!" :% :%
                 "Diapered Raccoon: You'll never get me to talk!!!!" :% :%
                 "Shark: We'll see about that *turns on a water faucet*" :% :%
                 "*The raccoon starts squirming in place, trying to move his arms in a desperate attempt to hold the front of his diaper but can't and has to rely purely on will to hold it in, which is fading fast.*" :% :%
                 "Diapered Raccoon: OK!!! PIRATE's COVE IS AT (0 0 0 YADFA/ZONES:PIRATES-COVE) NOW PLEASE LET ME GO!!!! I HAVE TO GO TO THE BATHROOM!!!!!!" :% :%
                 "Shark: Who said anything about letting you go?" :% :%
                 "*The shark starts tickling the raccoon's feet*" :% :%
                 "Diapered Raccoon: NUUUUU!!!!!!! *laughs and floods his diapers, which is clearly obvious since the insignia on the front doubles as a wetness indicator that turns yellow when wet.*" :% :%
                 "Shark: There, now you can sit in it while we find that cove." :% :%
                 "Diapered Raccoon: Good luck. The cove is located in the middle of a sea of parenthesis, which you can't navigate without a copy of Emacs." :% :%
                 "Shark: NOOOO!!!! MY IBM PC XT CAN ONLY RUN VI!!!!!!!" :% :%
                 "*Pirate's Cove was added to the map for your ship*" :% :%)
            (pushnew '(0 0 0 yadfa-zones:pirates-cove) (getf (attributes-of (getf (props-of (get-zone '(0 3 1 yadfa-zones:your-ship))) :controls)) :destinations))))
