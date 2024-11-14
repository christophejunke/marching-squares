(in-package :marching-squares)

(defun intro-level ()
  (make-instance
   'level-blueprint
   :name "Don't panic"
   :class 'shakeable-level
   :width 31
   :height 31
   :grid #("               V                  "
           "                                  "
           "     #######-#####H#######        "
           "     ####### #############        "
           "     ###               ###        "
           "   #H###               ###        "
           "   #################   ###        "
           "             #####     ###        "
           "             ##B##  b  ###        "
           "                       ###        "
           "                       ###        "
           "     ########=###=####X###        "
           "     ###       #                  "
           "     ###       #      8           "
           "     #########:#:#########        "
           "                                  "
           "                                  "
           "   ########~########~####X##      "
           "     #     e###E#                 "
           " #H#                ?      #H#    "
           " ###       ##     ######H# ###    "
           " ###            #  ####### ###    "
           "          #### ##  ## /           "
           "                 # ## /           "
           "           ##     8## /           "
           "         #    ##8# ## /           "
           "           # #     ## /       /   "
           "          #      #    /       /   "
           "/       /        #    /       /   "
           " ####H###^#H#^#####^####H##### ## "
           " ########@###@#####@########## ## "
           "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")

   :palettes (let ((common '((:inverted-square 0.0 0.0 0.0 1.0))))
               `((:blue . ((:background 0.3 0.5 0.6 1.0)
                           ,@common))
                 (:red . ((:background 0.6 0.3 0.3 1)
                          ,@common))
                 (dx . ((:door 0.0 0.0 0.0 0.0)))
                 (door-1 . ((:foreground 0.5 0.5 0.5 1.0)))
                 (door-2 . ((:foreground 0.5 0.7 0.5 1.0)))
                 (door-3 . ((:foreground 0.5 0.7 0.8 1.0)))
                 (door-5 . ((:foreground 0.8 0.8 0.5 1.0)))))
   :on-start (lambda (level) (pick-palette level :blue))
   :on-winning (next 'level/lateral-thinking)
   :triggers '((:class level-1/shake-destroy))
   :bindings '((#\b . (:trigger :release x))
               (#\B . (:blocked-square x))
               (#\e . (:trigger :release y))
               (#\E . (:blocked-square y))
               (#\f . (:trigger :release z))
               (#\F . (:blocked-square z))
               (#\/ . (:invisible-blocker))
               (#\X . (:gate dx))
               (#\@ . (:trigger :win))
               (#\8 . (:trigger :invert))
               (#\V . :start)
               (#\H . (:help "Press Esc to restart level"))
               (#\- . (:door door-1))
               (#\= . (:door door-2))
               (#\~ . (:door door-3))
               (#\^ . (:door door-4))
               (#\: . (:door door-5))
               (#\? . (:class/loc level-1/check-alternative-solution)))))

;; (setf (level-blueprint *game*) 'intro-level)

;; (setf (cdr (assoc :inverted-square (current-palette (game-level *game*))))
;;       '(0.0 0.0 0.0 1.0))

(defclass wall-square (abstract-square)
  ((falling :accessor falling :initform nil)
   (tumbling :accessor tumbling :initform nil)))

(defmethod is-pressed-by ((door press-door) (wall wall-square))
  nil)

(defclass visibly-wall-square (wall-square) ())

(defmethod display ((square wall-square))
  (display :wall))

(defmethod display ((square visibly-wall-square))
  (color '(:alpha 0.55 :wall))
  (gl:rect 0 0 1 1))

(defmethod display ((blocker invisible-blocker))
  (set-color :inverter :alpha 1)
  (gl:translate 0.5 0.5 0.5)
  (csq 0.15))

(defmethod allow-move-p ((object wall-square)
                         (target invisible-blocker))
 nil)

(defmethod compute-next-move ((square wall-square))
  ;; more like an UPDATE thing
  (setf (direction square)
        (cond
          ((falling square)
           (random-elt '(:left :right)))
          ((tumbling square)
           (let* ((candidates)
                  (loc (location square))
                  (neighbours (neighbours loc :nw :w :sw :ne :e :se)))
             (destructuring-bind (nw w sw ne e se) neighbours
               (when (and (allow-move-p square nw)
                          (allow-move-p square w)
                          (allow-move-p square sw))
                 (push :left candidates))
               (when (and (allow-move-p square ne)
                          (allow-move-p square e)
                          (allow-move-p square se))
                 (push :right candidates)))
             (and candidates (random-elt candidates))))))
  (let ((result (multiple-value-list (call-next-method))))
    (let ((was-falling (shiftf (falling square)
                               (eq :fall (first result)))))
      (cond
        ((and was-falling (not (falling square)))
         (setf (tumbling square) t))
        ((tumbling square)
         (setf (tumbling square) nil))))
    (values-list result)))

(defclass shakeable (transformable)
  ((shakep :initform nil :accessor shakep)
   (x-magnitude :initform 1 :accessor x-magnitude)
   (y-magnitude :initform 1 :accessor y-magnitude)
   (duration :initform 1 :accessor duration)))

(defclass shakeable-level (shakeable level)
  ())

(defmethod update ((s shakeable))
  (call-next-method)
  (when (shakep s)
    (setf (duration s) (max 0 (1- (duration s))))
    (when (zerop (duration s))
      (setf (shakep s) nil))))

(defun random-around (x size)
  (if (<= size 0)
      x
      (gaussian-random (- x size) (+ x size))))

(defmethod transform-model-view ((s shakeable))
  (when (shakep s)
    (let ((dx (random-around 0 (x-magnitude s)))
          (dy (random-around 0 (y-magnitude s))))
      (gl:translate dx dy 0))))

(defun detach% (array layer level row col)
  (let ((loc (loc level row col)))
    (when (find :wall (objects-at loc))
      (let ((ws (make-instance 'wall-square :location loc)))
        (forced-remove array row col :wall)
        (forced-remove layer row col :wall)
        (build ws loc)
        (activate-square loc ws)))))

(defun detach-wall (level row col)
  (detach% (level-array level)
           (layer-grid (layer level :background))
           level
           row
           col))

(defun shake (x y d)
  (let ((level (game-level *game*)))
    (setf (x-magnitude level) x)
    (setf (y-magnitude level) y)
    (setf (duration level) (abs d))
    (setf (shakep level) t)))

(defclass level-1/check-alternative-solution
    (global-trigger has-location oneshot invisible)
  ())

(defmethod triggerable ((trigger level-1/check-alternative-solution))
  (let ((square (find-if #'invertiblep (objects-at (location trigger)))))
    (and square (not (invertedp square)))))

(defmethod trigger ((trigger level-1/check-alternative-solution))
  (trigger-by-name 'shake-destroy *game*))

(defclass level-1/shake-destroy (trigger has-name) ()
  (:default-initargs :name 'shake-destroy))

(defmethod trigger ((action level-1/shake-destroy))
  (bt:make-thread
   (lambda ()
     (let* ((margin 2)
            (iter 6)
            (delete-limit (- iter 4))
            (level (game-level *game*))
            (array (level-array level))
            (layer (layer-grid (layer level :wall))))
       (setf (x-magnitude level) 0.4)
       (setf (y-magnitude level) 0.2)
       (setf (duration level) 2)
       (setf (shakep level) t)
       (pick-palette level :red)
       (flet ((detach (row col) (detach% array layer level row col)))
         (loop
           (decf iter)
           (when (zerop iter)
             (setf margin 200))
           (when (= iter 1)
             (setf (location (aref array 20 24)) :trash)
             (detach 17 4)
             (detach 17 23)
             (detach 18 5)
             (detach 26 11)
             (detach 21 27)
             (detach 20 28)
             (detach 21 28)
             (detach 21 23)
             (detach 21 24)
             (detach 17 23)
             (detach 17 25)
             (detach 17 26))
           (loop
             for col from 1 upto 30
             do
                (loop for row from 1 upto 15
                      do (dolist (object (ensure-list (aref array row col)))
                           (typecase object
                             ((eql :wall)
                              (unless (> (random 100) margin)
                                (detach row col)))
                             ((or symbol wall-square) nil)
                             (t (when (= iter delete-limit)
                                  (setf (location object) :trash)))))))
           (when (zerop iter)
             (return))
           (incf margin 5)
           (sleep (+ 0.2 (random 0.6)))))))))
