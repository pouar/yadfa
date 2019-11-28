;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-puzzle"; coding: utf-8-unix; -*-
(uiop:define-package :yadfa-puzzle
  (:mix :clim :yadfa)
  (:use :iterate :clim-lisp :clim-extensions)
  (:export #:run-game))
(in-package :yadfa-puzzle)
(define-presentation-type puzzle-piece () :inherit-from 'expression)
(defclass game-push-button (push-button) ())
(defvar *puzzle-size*)
(define-application-frame game-frame
    ()
  ((patterns
    :initform (make-array `(,(cdr *puzzle-size*) ,(cdr *puzzle-size*))
                          :initial-contents (iter
                                              (with pw = (car *puzzle-size*))
                                              (with ps = (cdr *puzzle-size*))
                                              (with pattern = (mcclim-raster-image:with-output-to-rgba-pattern
                                                                  (stream :width pw :height pw)
                                                                (draw-rectangle* stream 0 0 pw pw :ink +green+)
                                                                (flet ((draw-rosette2 (stream x y radius n &rest drawing-options)
                                                                         (loop with alpha = (/ (* 2 pi) n)
                                                                               and radius = (/ radius 2)
                                                                               for i below n
                                                                               do (apply #'clim:draw-circle* stream
                                                                                         (+ (* radius (cos (* alpha i))) x)
                                                                                         (+ (* radius (sin (* alpha i))) y)
                                                                                         radius
                                                                                         :filled nil
                                                                                         drawing-options))))
                                                                  (draw-rosette2 stream (/ pw 2) (/ pw 2) (/ pw 2) 18
                                                                                 :ink clim:+steel-blue+ :line-thickness 2))))
                                              (for y from 0 to (1- ps))
                                              (collect (iter (for x from 0 to (1- ps))
                                                         (collect (make-rectangular-tile
                                                                   (transform-region (make-translation-transformation (* x (- (/ pw ps)))
                                                                                                                      (* y (- (/ pw ps))))
                                                                                     pattern)
                                                                   (/ pw ps) (/ pw ps)))))))
    :accessor pattern-of)
   (puzzle
    :initform (make-array `(,(cdr *puzzle-size*) ,(cdr *puzzle-size*))
                          :initial-contents (iter (with a = (make-array (* (cdr *puzzle-size*) (cdr *puzzle-size*))
                                                                        :initial-contents (alexandria:shuffle
                                                                                           (iter
                                                                                             (for y from 0 to (1- (cdr *puzzle-size*)))
                                                                                             (with i = 0)
                                                                                             (appending (iter (for x from 0 to (1- (cdr *puzzle-size*)))
                                                                                                          (collect `(,i ,y ,x))
                                                                                                          (incf i)))))))
                                              (with i = 0)
                                              (for y from 0 to (1- (cdr *puzzle-size*)))
                                              (collect (iter (for x from 0 to (1- (cdr *puzzle-size*)))
                                                         (collect (aref a i))
                                                         (incf i)))))
    :accessor puzzle-of)
   (swap-1 :initform nil
           :accessor swap-1-of)
   (swap-2 :initform nil
           :accessor swap-2-of))
  (:pane (horizontally ()
           (make-clim-stream-pane :name 'puzzle :incremental-redisplay t :scroll-bars nil
                                  :display-time :command-loop :display-function 'draw-puzzle :width 320 :height 600)
           (vertically ()
             (make-clim-interactor-pane :name 'int :display-time :command-loop :width 480 :height 600)))))

(define-game-frame-command (com-select-piece :name t)
    ((piece puzzle-piece :prompt "Which Piece?"))
  (setf (swap-2-of *application-frame*) (swap-1-of *application-frame*)
        (swap-1-of *application-frame*) piece))
(define-game-frame-command (com-swap-pieces :name t)
    ()
  (let (swap)
    (when (and (swap-1-of *application-frame*) (swap-2-of *application-frame*))
      (setf swap (apply #'aref (puzzle-of *application-frame*) (swap-1-of *application-frame*)))
      (setf (apply #'aref (puzzle-of *application-frame*) (swap-1-of *application-frame*))
            (apply #'aref (puzzle-of *application-frame*) (swap-2-of *application-frame*)))
      (setf (apply #'aref (puzzle-of *application-frame*) (swap-2-of *application-frame*)) swap)
      (setf (swap-1-of *application-frame*) nil)
      (setf (swap-2-of *application-frame*) nil))))
(define-presentation-to-command-translator select-piece
    (puzzle-piece com-select-piece game-frame
     :gesture :select
     :documentation "Select Piece"
     :pointer-documentation "Select Piece")
    (object)
  (list object))
(defun draw-puzzle (frame pane)
  (let* ((pw (car *puzzle-size*))
         (ps (cdr *puzzle-size*))
         (px (/ pw ps)))
    (iter (for y from 0 to (1- ps))
      (iter (for x from 0 to (1- ps))
        (updating-output (pane :unique-id `(,x ,y) :id-test 'equal :cache-test 'equal
                               :cache-value `(,(aref (puzzle-of frame) y x)
                                              ,(when (equal (swap-1-of frame)
                                                            `(,y ,x))
                                                 `(,y ,x))
                                              ,(when (equal (swap-2-of frame)
                                                            `(,y ,x))
                                                 `(,y ,x))))
          (with-output-as-presentation (pane `(,y ,x) 'puzzle-piece)
            (draw-design pane (apply #'aref (pattern-of frame) (cdr (aref (puzzle-of frame) y x)))
                         :transformation (let ((translate (make-translation-transformation (* x px) (* y px))))
                                           (if (or (equal (swap-1-of frame) `(,y ,x)) (equal (swap-2-of frame) `(,y ,x)))
                                             (compose-transformations translate (make-scaling-transformation 0.9 0.9 (make-point (/ px 2) (/ px 2))))
                                             translate)))))))
    (stream-set-cursor-position pane 20 pw)
    (formatting-item-list (pane)
      (formatting-cell (pane)
        (with-output-as-presentation (pane '(com-swap-pieces) `(command :command-table ,(frame-command-table frame)))
          (surrounding-output-with-border
              (pane :shape :rounded :radius 6
                    :background +gray80+ :highlight-background +gray90+)
            (format pane "Swap Pieces"))
          #| gadgets currently don't work as presentations
          (with-output-as-gadget (pane)
          (make-pane 'push-button :client frame
          :label "Swap Pieces"))|#))
      (when (apply '< (iter (for y from 0 to (1- ps))
                        (appending (iter (for x from 0 to (1- ps))
                                     (collect (car (aref (puzzle-of frame) y x)))))))
        (formatting-cell (pane) (format pane "You Win"))))))
(defun run-game (&key (width 320) (height 4))
  (let ((*puzzle-size* `(,width . ,height)))
    (run-frame-top-level (make-application-frame 'game-frame :pretty-name "Puzzle"))))
