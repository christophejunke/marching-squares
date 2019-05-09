(in-package :marching-squares)

(defparameter *intro-level*
  (make-instance
   'level-blueprint
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
           "     ########=###=########        "
           "     ###       #                  "
           "     ###       #      8           "
           "     #########:#:#########        "
           "                                  "
           "                                  "
           "   ########~########~#######      "
           "     #     e###E#        #        "
           " #H#                ?      #H#    "
           " ###       ##     ######H# ###    "
           " ###            #  ####### ###    "
           "          #### ##  ## /           "
           "                 # ## /  XXXXX    "
           "           ##     8## /           "
           "         #    ##8# ## /           "
           "           # #     ## /       /   "
           "          #      #    /       /   "
           "/       /        #    /       /   "
           " ####H###^#H#^#####^####H##### ## "
           " ########@###@#####@########## ## "
           "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
   :on-start (lambda ()
               (setf (palette-background (palette *game*))
                     '(0.1 0.2 0.3 1.0 ))
               ;; (setf (palette-background (palette *game*))
               ;;       '(0.9 0.9 0.9 1.0 ))
               )
   :bindings '((#\b . (:trigger :release x))
               (#\B . (:blocked-square x))
               (#\e . (:trigger :release y))
               (#\E . (:blocked-square y))
               (#\f . (:trigger :release z))
               (#\F . (:blocked-square z))
               (#\/ . (:invisible-blocker))
               ;; (#\X . (:trigger :lose))
               (#\@ . (:trigger :win))
               (#\8 . (:trigger :invert))
               (#\V . :start)
               (#\H . (:help "Press Esc to restart level"))
               (#\- . (:door door-1))
               (#\= . (:door door-2))
               (#\~ . (:door door-3))
               (#\^ . (:door door-4))
               (#\: . (:door door-5))
               (#\? . (:class/loc level-1/check-alternative-solution))
               (t . (:class level-1/shake-destroy)))))
*game*

(defparameter *game*
  (make-instance 'marching-squares
                 :level-blueprint *intro-level*))

(setf (level-blueprint *game*) *intro-level*)
(setf (level-blueprint *game*) *chicken-level*)
(setf (level-blueprint *game*) *button-intro-level*)
(setf (level-blueprint *game*) *ramping-level*)

;;!!!!
(setf (palette-inverted-square (palette *game*))
      (list 1.0 0 0 0.5))

(setf (palette-inverter (palette *game*))
      (list 1.0 0 0 0.5))

(start-game *game*)

(defclass wall-square (abstract-square)
  ((falling :accessor falling :initform nil)))

(defmethod display ((square wall-square))
  (display :wall))

(defmethod display ((blocker invisible-blocker))
  (set-color #'palette-inverter :alpha 1)
  (gl:translate 0.5 0.5 0.5)
  (csq 0.15))

(defmethod allow-move-p ((object wall-square)
                         (target invisible-blocker))
  nil)

(defmethod compute-next-move ((square wall-square))
  ;; more like an UPDATE thing
  (setf (direction square)
        (if (falling square)
            (random-elt '(:left :right))
            nil))
  (let ((result (multiple-value-list (call-next-method))))
    (setf (falling square)
          (eq :fall (first result)))
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
  (let* ((loc (loc level row col))
         (ws (make-instance 'wall-square :location loc)))
    (forced-remove array row col :wall)
    (forced-remove layer row col :wall)
    (build ws loc)
    (activate-square loc ws)))

(defun shake (x y d)
  (let ((level (game-level *game*)))
    (setf (x-magnitude level) x)
    (setf (y-magnitude level) y)
    (setf (duration level) (abs d))
    (setf (shakep level) t)))


(defclass level-1/check-alternative-solution
    (global-trigger has-location oneshot)
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
       (setf (palette-background (palette *game*))
             (list 0.3 0.1 0.1 1))
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




;; (allow-move-p 
;;  (first (objects-at (loc (game-level *game*) 28 21)))
;;  (first (objects-at (loc (game-level *game*) 28 22))))

(setf (palette-background (palette *game*))
      '(0.1 0.2 0.3 1.0 ))

(shake 0.1 0 3)

;; NOTE : alexandria gaussian random pull request!!! when zero zero

(resolve 'door-1 *game*)

(first (resolve 'door-3 *game*))


(let ((loc (copy-loc
            (location (dogroup (trigger (triggers *game*))
                        (when (typep trigger 'level-1/check-alternative-solution)
                          (return trigger)))))))
  (decf (row loc))
  (activate-square loc))

;; 0.03
;; 1.48
;; (trigger (first (resolve 'shake-destroy *game*)))

;; (decf *size* 11)

(let ((out *standard-output*)
      (first t))
  (defmethod update :around (any)
    (when first
      (print
       (compute-applicable-methods #'update (list any))
       out)
      (setf first nil))
    (call-next-method)))

(setf (palette-background (palette *game*))
      '(0.1 0.2 0.3 1.0 ))

(setf (palette-blocked-square (palette *game*))
      '(1 1 1 0.25))

(setf (palette-inverted-square (palette *game*))
      (list 0.7 0.7 1.0 0.25))

(defparameter *chicken-level*
  (setf (level-blueprint *game*)
        (make-instance
         'level-blueprint
         :width 33
         :height 33
         :grid #("                                   "
                 "                                   "
                 "                                   "
                 "                                   "
                 "         #######                   "
                 "        #   V      V               "
                 "                                    "
                 "                                    "
                 "            #  ### #               "
                 "                                   "
                 "           ###    ###              "
                 "                                   "
                  "                                   "
                 "                                   "
                 "          #####  #####             "
                 "                                   "
                 "                                   "
                 "               #      #            "
                 "        ##     ##     ##           "
                 "                                   "
                 "                                   "
                 "                                   "
                 "XXXXXXX######-########-##XXXXXXXXXX"
                 "                                   "
                 "                                   "
                 "            #@#      #@#           "
                 "            # #      # #           "
                 "                                   "
                 "                                   "
                 "                                   "
                 "                                   "
                 "                                   "
                 "                                   "
                 "                                   "
                 "                                   "
                 "                                   "
                 )
         :bindings `((#\b . (:trigger :release x))
                     (#\B . (:blocked-square x))
                     (#\e . (:trigger :release y))
                     (#\E . (:blocked-square y))
                     (#\f . (:trigger :release z))
                     (#\F . (:blocked-square z))
                     (#\@ . (:trigger :win))
                     (#\> . (:blocked-square exit))
                     (#\< . (:trigger :release exit))
                     (#\8 . (:trigger :invert))
                     (#\V . :start)

                     (#\W . (:start :inverted))
                     (#\H . :help)
                     (#\% . :vanisher)
                     (#\u . (:spawn spawn-1))
                     (#\U . (:press-button button-group-1 (:trigger spawn-1)))
                     (#\Z . (:button button-group-1 (:trigger gate-3)))
                     (#\S . (:button button-group-3 (:trigger gate-4)))
                     (#\L . (:button button-group-2 (:trigger gate-5)))
                     (#\$ . (:button $ (:trigger unlock-right-square)))
                     (#\+ . (:door final))
                     (#\, . (:door bye))

                     (#\* . (:gate must-close :state :open))
                     (#\! . (:button button-group-5 (:trigger must-close)))

                     (#\_ . (:gate unlock-right-square))
                     (#\- . (:door door-1))
                     (#\= . (:door door-2))
                     (#\~ . (:gate gate-3))
                     (#\^ . (:gate gate-4))
                     (#\: . (:gate gate-5))))))


(defparameter *button-intro-level*
  (setf (level-blueprint *game*)
        (make-instance
         'level-blueprint
         :width 33
         :height 33
         :grid #("                                "
                 " # V          #   #      V   #  "
                 "  ####Z## ##### # #####~#####   "
                 "     ###                         "
                 "                                 "
                 "       ######L######L###        "
                 "       #    ###                 "
                 " ###V    ##              #####  "
                 " ###:########## # ############  "
                 "              #-#-############  "
                 " ###            #          B##  "
                 " ###            #               "
                 " ###-#####^##########S#####     "
                 " ###      b                     "
                 " ###                            "
                 " #############S#########   #    "
                 " ###########E#                  "
                 "                           #    "
                 "             #=#=#=#=#######    "
                 "             # #e# #            "
                 "             # # # #            "
                 "           ### #+# ###_#####    "
                 "                 #              "
                 "              8  #8             "
                 " ###8########### ####>#$*###8   "
                 " #          #### #### ##   #    "
                 " #      F        #### ##   # #  "
                 " #     #!#            ######+#  "
                 " #                           #  "
                 "        f              <     #  "
                 " # #######,#,#,#,#,#,#,#######  "
                 " #########@#@#@#@#@#@#@#######  "
                 )
         :on-start (lambda ()
                     (setf (palette-background (palette *game*))
                           '(0.8 0.6 0.1 1.0 ))
                     (setf (palette-inverted-square (palette *game*))
                           (list 0.7 0.7 1.0 0.25)))
         :bindings `((#\b . (:trigger :release x))
                     (#\B . (:blocked-square x))
                     (#\e . (:trigger :release y))
                     (#\E . (:blocked-square y))
                     (#\f . (:trigger :release z))
                     (#\F . (:blocked-square z))
                     (#\@ . (:trigger :win))
                     (#\> . (:blocked-square exit))
                     (#\< . (:trigger :release exit))
                     (#\8 . (:trigger :invert))
                     (#\V . :start)

                     (#\W . (:start :inverted))
                     (#\H . :help)
                     (#\% . :vanisher)
                     (#\u . (:spawn spawn-1))
                     (#\U . (:press-button button-group-1 (:trigger spawn-1)))
                     (#\Z . (:button button-group-1 (:trigger gate-3)))
                     (#\S . (:button button-group-3 (:trigger gate-4)))
                     (#\L . (:button button-group-2 (:trigger gate-5)))
                     (#\$ . (:button $ (:trigger unlock-right-square)))
                     (#\+ . (:door final))
                     (#\, . (:door bye))

                     (#\* . (:gate must-close :state :open))
                     (#\! . (:button button-group-5 (:trigger must-close)))

                     (#\_ . (:gate unlock-right-square))
                     (#\- . (:door door-1))
                     (#\= . (:door door-2))
                     (#\~ . (:gate gate-3))
                     (#\^ . (:gate gate-4))
                     (#\: . (:gate gate-5))))))

(defparameter *new-level*
  (setf (level-blueprint *game*)
        (make-instance
         'level-blueprint
         :width 31
         :height 32
         :grid #("#  V                     V   #            "
                 "#                           #             "
                 "########-################-##             "
                 "#        #                 #              "
                 "#        #                 #              "
                 "####~###########Z###########              "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 )
         :bindings `((#\b . (:trigger :release x))
                     (#\B . (:blocked-square x))
                     (#\e . (:trigger :release y))
                     (#\E . (:blocked-square y))
                     (#\f . (:trigger :release z))
                     (#\F . (:blocked-square z))
                     (#\@ . (:trigger :win))
                     (#\> . (:blocked-square exit))
                     (#\< . (:trigger :release exit))
                     (#\8 . (:trigger :invert))
                     (#\V . :start)
                     (#\W . (:start :inverted))
                     (#\H . :help)
                     (#\% . :vanisher)
                     (#\u . (:spawn spawn-1))
                     (#\U . (:press-button button-group-1 (:trigger spawn-1)))
                     (#\Z . (:button button-group-1 (:trigger gate-3)))
                     (#\S . (:button button-group-3 (:trigger gate-4)))
                     (#\L . (:button button-group-2 (:trigger gate-5)))
                     (#\$ . (:button $ (:trigger unlock-right-square)))
                     (#\+ . (:door final))
                     (#\, . (:door bye))

                     (#\* . (:gate must-close))
                     (#\! . (:button button-group-5 (:trigger must-close)))

                     (#\_ . (:gate unlock-right-square))
                     (#\- . (:door door-1))
                     (#\= . (:door door-2))
                     (#\~ . (:gate gate-3))
                     (#\^ . (:gate gate-4))
                     (#\: . (:gate gate-5))))))


(defparameter *test-level*
  (setf (level-blueprint *game*)
        (make-instance
         'level-blueprint
         :width 31
         :height 31
         :grid #("    L         L         u   "
                 "                            "
                 "                            "
                 "                            "
                 "                                "
                 "##U#Z#######U#Z####-##-#~##~# #####    "
                 ""
                 "                  #%##%#%##%#     "
                 "                  # ## # ## #     "
                 ""
                 "    8    8   8   8 WV 8   8           "
                 " #################################")
         :bindings `((#\b . (:trigger :release x))
                     (#\B . (:blocked-square x))
                     (#\e . (:trigger :release y))
                     (#\E . (:blocked-square y))
                     (#\f . (:trigger :release z))
                     (#\F . (:blocked-square z))
                     (#\L . :start)
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

(defun dbg ()
  (loop
    with array = (items (active-objects *game*))
    with limit = (fill-pointer array)
    for i below (array-total-size array)
    when (= i limit) do (format t "~&--------------------~%")
      do (format t "~&~3,,'0d ~S~%" i (aref array i))
    finally (return array)))

(loop
  (dotimes (i 10) (terpri))
  (dbg)
  (sleep 0.5))

(trigger-by-name :start (game-level *game*))

(setf (location (find-if #'squarep (items (mobiles *game*)))) :trash)

(trigger-by-name 'spawn-1 *game*)

;; test-level, one door says open, the other one closes
;; (expected behaviour)
(progn
  (activate-square (loc (game-level *game*) 1 22))
  (loop repeat 30
        do (activate-square (loc (game-level *game*) 1 19))
        (sleep 0.1)))

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
