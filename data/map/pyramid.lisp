;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-zones"; coding: utf-8-unix; -*-
(in-package :yadfa-zones)
(ensure-zone (0 0 0 yadfa-zones:pyramid)
             :name "Pyramid Entrance"
             :description "You're at the pyramid entrance"
             :must-wear 'pyramid
             :must-not-wear 'pyramid
             :must-wear* 'pyramid
             :must-not-wear* 'pyramid)
(ensure-zone (1 0 0 yadfa-zones:pyramid)
             :name "Pyramid Hall"
             :description "You're at the pyramid entrance"
             :must-wear 'pyramid
             :must-not-wear 'pyramid
             :must-wear* 'pyramid
             :must-not-wear* 'pyramid
             :events '(yadfa-events:pyramid-puzzle-1))
