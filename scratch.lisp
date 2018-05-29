(in-package :marching-squares)

(untrace incorporate)

(defparameter *game*
  (make-instance 'marching-squares
                 :level-blueprint *ramping-level*))

(start-game *game*)

(setf *size* 11)

(defparameter *test-level*
  (setf (level-blueprint *game*)
        (make-instance
         'level-blueprint
         :width 31
         :height 31
         :grid #("    V         V         u   "
                 "                            "
                 "                            "
                 "                            "
                 "                            "                 
                 "##U#Z#######U#Z####-##-#~##~########    "
                 ""
                 "                         "
)
         :bindings `((#\b . (:trigger :release x))
                     (#\B . (:blocked-square x))
                     (#\e . (:trigger :release y))
                     (#\E . (:blocked-square y))
                     (#\f . (:trigger :release z))
                     (#\F . (:blocked-square z))
                     ;; (#\X . (:trigger :lose))
                     (#\@ . (:trigger :win))
                     (#\8 . (:trigger :invert))
                     (#\V . :start)
                     (#\W . (:start :inverted))
                     (#\H . :help)
                     (#\% . :vanisher)
                     (#\u . (:spawn spawn-1))
                     (#\U . (:press-button button-group-1 (:trigger spawn-1)))
                     (#\Z . (:button button-group-2 (:trigger gate-3)))
                     (#\- . (:door door-1))
                     (#\= . (:door door-2))
                     (#\~ . (:gate gate-3))
                     (#\^ . (:door door-4))
                     (#\: . (:door door-5))))))


(trace 'incorporate)
(setf (palette-background (palette *game*))
      (list 0.4 0.6 0.4 1))

(setf (palette-background (palette *game*))
      (list 0.8 0.7 0.2 1))

(setf (palette-background (palette *game*))
      (list 0.6 0.2 0.2 1))

(invert (elt (items (mobiles *game*)) 2))

(setf (location (elt (items (mobiles *game*)) 2)) :trash)

(map ()
     (lambda (u)
       (setf (decoration u)
             :test-decoration))
     (items (mobiles *game*)))

(let ((location (location (elt (mobiles (game-level *game*)) 2))))
  (setf (location (elt (mobiles (game-level *game*)) 2))
        (loc (game-level *game*) (- (row location) 4) (- (col location) 2))))

(trigger-by-name :start (game-level *game*))

(defclass test-game (game) ())
(defmethod game-idle ((game test-game)))

(defmethod game-setup progn ((game test-game))
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:blend-equation :func-add)
  (gl:enable :depth-test))

(defmethod game-idle ((game test-game))
  (gl:clear-color 1 1 0 1)
  (gl:clear :color-buffer)
  (gl:clear :depth-buffer)

  (gl:with-pushed-matrix
    (gl:color 1 1 1 0.5)
    (gl:translate 0 0 -0.2)
    (gl:rect -0.5 -0.5 0.5 0.5))
  (gl:with-pushed-matrix
    (gl:color 0 0 0 0.5)
    (gl:rect 0 0 1 1))
  
  (gl:flush)
  (sdl2:gl-swap-window *window*)
  (sleep 0.1))

(start-game (make-instance 'test-game :width 300 :height 300))

(loop
  for i from 0 below 31
  for loc = (loc (game-level *game*) 6 i)
  unless (objects-at loc)
    do (activate-square loc))

(loop
  for i from 0 below 31
  for loc = (loc (game-level *game*) 29 i)
  do (map () (lambda (u) (change-class u 'garbage))
          (remove-if-not #'squarep (objects-at loc))))
