;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-events"; coding: utf-8-unix; -*-
(in-package :yadfa-events)
(defevent pointless-quest-1
  :lambda (lambda (self)
            (declare (ignorable self))
            (f:fmt t "The author is running out of ideas, so lets add some filler content" #\Newline #\Newline
                   "*Player starts 20 minute power up sequence by standing there screaming*" #\Newline #\Newline
                   "Damn it, that takes up a lot less time in a text based game. Time to bring out the pointless quests" #\Newline #\Newline
                   "But what quest can we perform? I know, well dome some tasks in the Digimon universe, but there's a catch. That particular universe is in a 90s Linux system. To make things a bit easier, and because Pouar doesn't want to try and implement some of the details because he's lazy, we'll let this Guilmon do some of this stuff for you" #\Newline #\Newline
                   "*Guilmon recompiles the kernel to get video drivers and reboots, fries several monitors trying to get X working, recompiles the kernel and reboots again 20 times trying to find the right sound driver because you can only load one at a time, locks the system up because he forgot to run and configure isapnp before running modprobe, configures and runs isapnp before running modprobe, recompiles the kernel and reboots again to get the drivers for networking, struggles trying to write ppp chat scripts by hand, gives up and runs pppsetup from slackware, buys a new modem because Linux back then didn't support winmodem, goes to Netscape's website to download Netscape Navigator, journeys down over 9000 directories from the ftp cli client to get there, etc, etc, then shoots himself in the head.*" #\Newline #\Newline)))
