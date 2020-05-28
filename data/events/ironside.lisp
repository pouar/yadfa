;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-events"; coding: utf-8-unix; -*-
(in-package :yadfa-events)
(defevent ironside-university-joke-1
  :predicate (lambda (self)
               (declare (ignore self))
               (= (random 3) 0))
  :lambda (lambda (self)
            (declare (ignore self))
            (f:fmt t "Diapered *Raccoon Bandit barges in*" #\Newline #\Newline
                   "Diapered *Raccoon Bandit: Hey, is this one of those parenting classes that is looking for a live demonstration?" #\Newline #\Newline
                   "Instructor: No" #\Newline #\Newline
                   "Diapered Raccoon Bandit: Great, today I'm going to give all you aspiring parents a live demonstration on how to change your bab's diaper" #\Newline #\Newline
                   "*Diapered Raccoon Bandit drags in a heavily blushing Rookie Diapered Raccoon Bandit wearing nothing but a soggy mushy diaper and pacigag*" #\Newline #\Newline
                   "Instructor: Just how long is this supposed to take?" #\Newline #\Newline
                   "Diapered Raccoon Bandit: Well considering the University decided to use a bunch of \"Gun Free Zone\" signs instead of security guards or police, I should have a good 45 minutes before anybody shows up to drag me out, but I should be done by then. You'd think after 6 shootings they'd learn their lesson, but eh." #\Newline #\Newline
                   "*The Diapered Raccoon Bandit straps the Rookie down onto the teacher's desk*" #\Newline #\Newline
                   "Diapered Raccoon Bandit: First you want to strap your bab down to keep him from running away and hiding his shame. Be sure to take recordings of this for posterity." #\Newline #\Newline
                   "*The giggling students pull out their smartphones and start recording against the Rookie's will*" #\Newline #\Newline
                   "Diaper Raccoon Bandit: Now you change your blushy bab's diaper and put in a new one like this" #\Newline #\Newline
                   "*The Diapered Raccoon Bandit removes the rookie's diaper soggy messy diaper in front of all the students*" #\Newline #\Newline
                   "Diaper Raccoon Bandit: Don't be afraid if your bab puts up a fuss. It's perfectly normal for him to be thoroughly embarrassed when you change him in public like this for the world to see, but it makes great entertainment at your bab's expense." #\Newline #\Newline
                   "*The Diapered Raccoon Bandit puts a thick clean diaper on the rookie*" #\Newline #\Newline
                   "Diaper Raccoon Bandit: You want to make sure you use diapers that are extremely thick to not just stop leaks, but to force your bab to waddle around and struggle to walk like a toddler. Sometimes your bab will be forced to crawl around on the floor like the bab he is." #\Newline #\Newline
                   "*The Diapered Raccoon Bandit unstraps the rookie and carries him in his arms like an infant*" #\Newline #\Newline
                   "Diaper Raccoon Bandit: And that's all there is to it, now I'm going to leave before the cops get the chance to show up and drag me away. So you're on your own for the rest of your class. Bye." #\Newline #\Newline
                   "*Diapered Raccoon Bandit takes off*" #\Newline #\Newline)))
