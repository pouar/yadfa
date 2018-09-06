(in-package :yadfa/events)
(defevent enter-silver-cape-1
    :lambda '(lambda (self)
                 (declare (ignore self))
                 (setf (getf (warp-points-of (get-zone (0 0 0 yadfa/zones:secret-underground))) 'yadfa/zones:silver-cape) '(0 5 0 silver-cape))))
(defevent obtain-pirate-ship-1
    :lambda '(lambda (self)
                 (declare (ignore self))
                 (format t "You find a note attached to the ship~%")
                 (format t "\"Hey Player, you might need a ship to travel these seas in this game with,~%so I coded you one. I named it after one of my favorite songs by Cannibal Corpse.~%Enjoy~%~%~%With Love, Pouar Dragon\"~%~%")
                 (format t "Game? Is this guy some sort of nutcase? You decide not to look at a gift horse in the mouth and accept the gift.")))
