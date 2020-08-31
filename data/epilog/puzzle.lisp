;;;; -*- mode: Common-Lisp; sly-buffer-package: "yadfa-puzzle"; coding: utf-8-unix; -*-
(in-package :yadfa-puzzle)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (c:define-presentation-type puzzle-piece () :inherit-from 'expression))
(defclass game-push-button (push-button) ())
(defvar *puzzle-size*)
(defvar *patterns*)
(defvar *puzzle*)
(defvar *swap-1*)
(defvar *swap-2*)
(defvar *win*)
(declaim (type list *swap-1* *swap-2* *puzzle-size*)
         (type array *puzzle* *patterns*)
         (type (or boolean cons) *win*))
(c:define-command-table puzzle-commands)
(cc:define-conditional-application-frame game-frame
    ()
  (:enable-commands (puzzle-commands))
  ()
  (:command-table (game-frame :inherit-from (puzzle-commands)))
  (:pane (c:vertically ()
                       (c:make-clim-stream-pane :name 'puzzle :incremental-redisplay t :scroll-bars nil
                                                :display-time :command-loop :display-function 'draw-puzzle :width 800 :height 450)
                       (c:make-clim-interactor-pane :name 'int :display-time :command-loop :width 800 :height 150))))
(define-game-frame-command (com-exit-game :name t)
    ()
  (c:frame-exit c:*application-frame*))
(defmethod c:run-frame-top-level ((frame game-frame) &key)
  (let ((*patterns* (make-array `(,(cdr *puzzle-size*) ,(cdr *puzzle-size*))
                                :initial-contents (iter
                                                   (with pw = (car *puzzle-size*))
                                                   (with ps = (cdr *puzzle-size*))
                                                   (with pattern = (mcclim-raster-image:with-output-to-rgba-pattern
                                                                       (stream :width pw :height pw)
                                                                     (c:draw-rectangle* stream 0 0 pw pw :ink c:+green+)
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
                                                                  (collect (c:make-rectangular-tile
                                                                            (c:transform-region (c:make-translation-transformation (* x (- (/ pw ps)))
                                                                                                                                   (* y (- (/ pw ps))))
                                                                                                pattern)
                                                                            (/ pw ps) (/ pw ps))))))))
        (*puzzle* (make-array `(,(cdr *puzzle-size*) ,(cdr *puzzle-size*))
                              :element-type 'cons
                              :initial-contents (iter (with a = (make-array (* (cdr *puzzle-size*) (cdr *puzzle-size*))
                                                                            :element-type 'cons
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
                                                                     (incf i))))))
        (*swap-1* nil)
        (*swap-2* nil)
        *win*)
    (declare (special *patterns* *puzzle* *swap-1* *swap-2* *win*)
             (type list *swap-1* *swap-2* *puzzle-size*)
             (type array *puzzle* *patterns*)
             (type (or boolean cons) *win*))
    (handler-case (call-next-method)
      (c:frame-exit ()
        *win*))))
(cc:define-conditional-command (com-end-puzzle :name t)
    (game-frame :disable-commands (puzzle-commands))
  ())
(c:define-command (com-select-piece :name t :command-table puzzle-commands)
    ((piece puzzle-piece :prompt "Which Piece?"))
  (locally (declare (type list piece))
    (setf *swap-2* *swap-1*
          *swap-1* piece)))
(c:define-command (com-swap-pieces :name t :command-table puzzle-commands)
    ()
  (when (and *swap-1* *swap-2*)
    (let ((swap (apply #'aref *puzzle* *swap-1*)))
      (declare (type cons swap))
      (setf (apply #'aref *puzzle* *swap-1*) (apply #'aref *puzzle* *swap-2*)
            (apply #'aref *puzzle* *swap-2*) swap
            *swap-1* nil
            *swap-2* nil))
    (let ((had-accident (process-potty)))
      (declare (type cons had-accident))
      (cond ((apply '< (iter (for y from 0 to (1- (cdr *puzzle-size*)))
                             (appending (iter (for x from 0 to (1- (cdr *puzzle-size*)))
                                              (collect (car (aref *puzzle* y x)))))))
             (setf *win* t)
             (cc:change-entity-enabledness 'com-end-puzzle)
             (write-line "You completed the puzzle"))
            ((or (car had-accident) (cdr had-accident))
             (setf *win* had-accident)
             (cc:change-entity-enabledness 'com-end-puzzle)
             (format t
                     "You ~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~} your pamps~%"
                     `(,@(when (car had-accident)
                           '("flooded"))
                       ,@(when (cdr had-accident)
                           '("messed")))))))))
(c:define-presentation-to-command-translator select-piece
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
                (c:updating-output (pane :unique-id `(,x ,y) :id-test 'equal :cache-value (sxhash `(,(aref *puzzle* y x)
                                                                                                     ,(when (equal *swap-1*
                                                                                                                   `(,y ,x))
                                                                                                        `(,y ,x))
                                                                                                     ,(when (equal *swap-2*
                                                                                                                   `(,y ,x))
                                                                                                        `(,y ,x)))))
                                   (c:with-output-as-presentation (pane `(,y ,x) 'puzzle-piece)
                                     (c:draw-design pane (apply #'aref *patterns* (cdr (aref *puzzle* y x)))
                                                    :transformation (let ((translate (c:make-translation-transformation (* x px) (* y px))))
                                                                      (if (or (equal *swap-1* `(,y ,x)) (equal *swap-2* `(,y ,x)))
                                                                          (c:compose-transformations translate (c:make-scaling-transformation 0.9l0 0.9l0 (c:make-point (/ px 2) (/ px 2))))
                                                                          translate)))))))
    (setf (c:stream-cursor-position pane) (values 20 pw))
    (c:formatting-item-list (pane)
                            (c:updating-output (pane :cache-value *win*)
                                               (typecase *win*
                                                 (null (c:formatting-cell (pane)
                                                                          (c:with-output-as-presentation (pane '(com-swap-pieces) `(c:command :command-table ,(c:frame-command-table frame)))
                                                                            (c:surrounding-output-with-border
                                                                             (pane :shape :rounded :radius 6
                                                                                   :background c:+gray80+ :highlight-background c:+gray90+)
                                                                             (format pane "Swap Pieces")))))
                                                 (t (c:formatting-cell (pane)
                                                                       (c:with-output-as-presentation (pane '(com-exit-game) `(c:command :command-table ,(c:frame-command-table frame)))
                                                                         (c:surrounding-output-with-border
                                                                          (pane :shape :rounded :radius 6
                                                                                :background c:+gray80+ :highlight-background c:+gray90+)
                                                                          (format pane "Exit Game")))))))
                            (c:formatting-cell (pane) (c:present (player-of *game*) (type-of (player-of *game*)) :view +stat-view+ :stream pane)))))
(defun run-game (&key (width 320) (height 5))
  (let ((*puzzle-size* `(,width . ,height)))
    (c:run-frame-top-level (c:make-application-frame 'game-frame :pretty-name "Puzzle" :width 800 :height 600))))
