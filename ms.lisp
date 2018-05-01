(defpackage :marching-squares
  (:use
   :cl
   :alexandria)
  (:import-from :sdl2
                #:set-render-draw-color
                #:scancode-key-to-value
                #:scancode-value
                #:with-everything
                #:with-renderer
                #:with-event-loop
                #:gl-make-current)
  (:import-from :trivia
                #:match))

(in-package :marching-squares)

;; size (better if multiple of 10)
(defparameter *size* 20)

(defvar *gl*)
(defvar *window*)
(defvar *renderer*)
(defvar *game*)

(defclass square-trigger (global-trigger
                          has-location)
  ())

(defclass square-lambda (square-trigger
                         lambda-trigger)
  ())

(defclass inverter (global-trigger
                    oneshot
                    has-location
                    has-angle
                    has-absolute-microstep
                    transformable)
  ((dy :initform 0 :accessor dy)
   (up :initform 0 :accessor up))
  (:default-initargs :angle (random 360)))

(defmethod triggerable ((trigger square-trigger))
  (find-if #'squarep (objects-at (location trigger))))

(defmethod triggerable ((inverter inverter))
  (find-if #'invertiblep (objects-at (location inverter))))

(defmethod trigger ((inverter inverter))
  (map () #'invert (objects-at (location inverter))))

(defclass releaser (square-trigger
                    has-absolute-microstep
                    transformable
                    oneshot)
  ((alpha :accessor alpha :initform 0)
   (counter :accessor counter :initform 0)
   (target :accessor target :initarg :target)))

(defmethod trigger ((releaser releaser))
  (release (level (location releaser)) (target releaser)))

(defmethod transform-model-view ((trigger releaser))
  (gl:translate 0.5 0.5 0.5)
  (gl:rotate (* 3 (counter trigger) #.(/ 180 pi)) 0 0 1))

(defmethod display ((trigger releaser))
  (set-color #'palette-foreground :alpha (alpha trigger))
  (csq 0.1))

(defclass helper (square-trigger invisible)
  ((text :initarg :text
         :accessor text
         :initform "Press Esc to restart level")))

(defmethod trigger ((helper helper))
  (sdl2:set-window-title
   *window*
   (text helper)))

(defmethod delta-microstep ((trigger releaser) delta)
  (setf (counter trigger)
        (mod (+ (counter trigger)
                (/ delta internal-time-units-per-second 1/2))
             #.(* 2 pi)))
  (setf (alpha trigger)
        (- 0.6 (abs (/ (sin (counter trigger)) 2)))))

(defclass start-trigger (named-trigger
                         has-location
                         oneshot) ()
  (:default-initargs :name :start))

(defclass spawn-trigger (named-trigger
                         has-location) ()
  (:default-initargs :name :spawn))

(defclass inverted-start-trigger (start-trigger) ())

(defmethod trigger ((trigger start-trigger))
  (activate-square (location trigger)))

(defmethod trigger ((trigger spawn-trigger))
  (activate-square (location trigger)))

(defmethod trigger ((trigger inverted-start-trigger))
  (activate-square (location trigger)
                   (make-instance 'square
                                  :location (location trigger)
                                  :inverted t)))

;;;;;
;;;;;

(defclass vanisher (global-trigger
                    has-absolute-microstep
                    has-location)
  ((pulse :initform 0 :accessor pulse)
   (offset :initform (random (* 2 pi)) :accessor offset)))

(defmethod delta-microstep ((vanisher vanisher) dt)
  (setf (pulse vanisher)
        (mod (+ (pulse vanisher)
                (/ dt internal-time-units-per-second 1/6))
             #.(* 2 pi))))

(defmethod display ((vanisher vanisher))
  (let* ((pulse (+ (offset vanisher) (pulse vanisher)))
         (intensity (+ 0.3 (/ (sin pulse) 5)))
         (y 1)
         (dx (sin (/ (expt pulse 2) 8))))
    (gl:color 0 0 0 0.1)
    (gl:rect 0 (+ y 0.1) 1 (+ y 0.2))
    (gl:color (/ intensity 3)
              (/ intensity 2)
              0
              intensity)
    (gl:rect (- 0.2 dx) y (+ 0.8 dx) (+ y 0.05))))

(defclass vanishing-square (square)
  ((ratio :accessor vanishing-ratio :initform 0)))

(defmethod display ((square vanishing-square))
  (call-next-method)
  (gl:color 0 0 0 (vanishing-ratio square))
  (gl:rect 0 0 1 1))

(defmethod update ((g vanishing-square))
  (setf (location g) :trash))

(defmethod microstep ((square vanishing-square) ratio)
  (call-next-method)
  (setf (vanishing-ratio square) ratio))

(defgeneric vanish (object)
  (:method (_))
  (:method ((s square))
    (change-class s 'vanishing-square)))

(defmethod trigger ((vanisher vanisher))
  (map () #'vanish (objects-at (location vanisher))))

;;;;;
;;;;;

(defun parse-action (expression level)
  (match expression
    ((list :trigger name) (lambda () (trigger-by-name name level)))))

(defmethod build (expression location)
  (flet ((new (class &rest args)
           (apply #'make-instance class :location location args)))
    (match expression
      ((eq nil) nil)
      ((list :spawn name) (new 'spawn-trigger :name name))
      ((eq :start) (new 'start-trigger))
      ((eq :help) (new 'helper))
      ((eq :vanisher) (new 'vanisher))
      ((list :press-button group action)
       (make-button group location
                    (parse-action action (level location))
                    :latchp t))
      ((list :button group action)
       (make-button group location
                    (parse-action action (level location))))
      ((list :start :inverted) (new 'inverted-start-trigger))
      ((list :and-group group-name expression)
       (build expression (named-and-group group-name)))
      ((list :gate name) (make-door name :location location :pressp nil))      
      ((list :door name) (make-door name :location location :pressp t))
      ((list :blocked-square name) (new 'square :name name :blockedp t))
      ((list :trigger :invert) (new 'inverter))
      ((list :trigger :lose) (new 'looser))
      ((list :trigger :win) (new 'winner))
      ((list :trigger :release name) (new 'releaser :target name))
      (e (add-object-at-location%% location e)))))

;;;; GAME

(defclass winner (square-lambda
                  has-absolute-microstep)
  ((up :initform 0 :accessor up)
   (counter :initform 0 :accessor counter))
  (:default-initargs :action (lambda () (win *game*))))

(defclass looser (square-lambda invisible) ()
  (:default-initargs :action (lambda () (loose *game*))))

(defmethod delta-microstep ((trigger winner) dt)
  (setf (counter trigger)
        (mod (+ (counter trigger)
                (/ dt internal-time-units-per-second))
             #.(* 2 pi)))
  (setf (up trigger)
        (+ -.1 (/ (cos (counter trigger)) 2))))

(defmethod display ((trigger winner))
  (gl:color 1 1 1 (up trigger))
  (gl:rect 0 0.9 1 1))

(define-condition restart-game-signal () ())
(defun restart-game-loop (&rest args)
  (declare (ignore args))
  (invoke-restart 'restart-game-loop))

(defgeneric game-setup (game)
  (:method-combination progn)
  (:method progn (_)))

(defgeneric game-loop (game)
  (:method (game)
    (with-event-loop (:method :poll)
      (:keydown
       (:keysym keysym)
       (game-command game (keybind (scancode-value keysym) game)))
      (:quit () t)
      (:idle () (game-idle game)))))

(defgeneric start-game (game)
  (:method (game)
    (with-everything (:gl *gl*
                      :window (*window* :w (width game)
                                        :h (height game)
                                        :title (title game)
                                        :flags '(:shown :opengl)))
      (with-renderer (*renderer* *window*)
        (gl-make-current *window* *gl*)
        (game-setup game)
        (handler-bind ((restart-game-signal #'restart-game-loop))
          (tagbody
           start
             (restart-case (game-loop game)
               (restart-game-loop ()
                 :report "Restart game loop"
                 (go start)))))))))

(defgeneric game-command (game command)
  (:method (game command) nil)
  (:method (game (command function)) (funcall command)))

(defgeneric game-idle (game))

;;;; MARCHING-SQUARES

(defparameter *default-palette*
  (make-palette :background (list 0.4 0.4 0.5 1)
                                        ;(list 30/256 60/256 70/256 1)
                                        ;(list 130/256 0 40/256 1)
                :wall '(0 0 0 1)
                :square '(1 1 1 1)
                :flash/feedback '(1 1 1 1)
                :inverted-square '(0.7 0.7 1 0.8)
                :blocked-square '(1 1 1 0.5)
                :foreground '(1.0 1.0 1.0 0.7)
                :inverter '(0.8 0.8 0.0 1.)))

(defclass marching-squares (has-palette game) ()
  (:default-initargs
   :direction nil
   :title "Marching squares"
   :width (* *size* 31)
   :height (* *size* 31)
   :palette *default-palette*))

;; (defparameter *default-palette*
;;   (setf (palette *game*)
;;         (make-palette :background (list 30/100 20/100 10/100 1)
;;                       :wall '(0.8 0.6 0.1 1)
;;                       :square '(0 0 0 01)
;;                       :flash/feedback '(0 0 0 1)
;;                       :inverted-square '(0 0 0 0.8)
;;                       :blocked-square '(0 0 0 .9)
;;                       :foreground '(10/10 9/10 1/10 0.8)
;;                       :inverter '(1 1 1 1))))

(defmethod game-command ((game marching-squares) (command symbol))
  (case command
    (:go-left (setf (direction game) :left))
    (:go-right (setf (direction game) :right))
    (:restart (restart-game-loop))))

(defmethod arbiter-compare
    ((arbiter marching-squares) location first second)
  "Game arbiter"
  (or (and (eq :fall (move-type first))
           (not (eq :fall (move-type second))))
      (and (not (invertedp (move-object first)))
           (invertedp (move-object second)))))

;; (make-palette
;;              :background '(0.29296875d0 0.29296875d0 0.390625d0 1.0)
;;              :square '(1 1 1 1))

(defmethod initialize-instance :after
    ((game marching-squares) &key &allow-other-keys)
  (setf (keybind :scancode-f1 game) :restart-graphics)
  (setf (keybind :scancode-left game) :go-left)
  (setf (keybind :scancode-right game) :go-right)
  (setf (keybind :scancode-escape game) :restart))

(defmethod game-setup progn ((game marching-squares))
  (gl:enable :blend)
  ;; (gl:enable :depth-test)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:blend-equation :func-add)
  (gl:clear-color 0.4 0.4 0.3 1.0)
  (gl:clear :color-buffer)
  (gl:ortho 0 31 31 0 -1 1))

(defmethod (setf game-level) :after ((level level) (game game))
  (setf (width game) (width level)
        (height game) (height level)))

(defparameter *test-level* nil)

(defmethod game-loop :before ((game marching-squares))
  (reinitialize-instance game)
  (setf (game-level *game*)
        (build (level-blueprint *game*) *game*))
  (sdl2:set-window-title *window* (title *game*))
  (trigger-by-name :start (game-level *game*)))

(defmethod display :after ((game marching-squares))
  (gl:flush)
  (sdl2:gl-swap-window *window*))

(defmethod game-idle ((game marching-squares))
  (update game)
  (let ((microsteps (microsteps game))
        (duration (microsteps-duration game)))
    (if (plusp microsteps)
        (let ((micro-delay (/ duration microsteps)))
          (dotimes (step microsteps)
            (microstep game (/ step microsteps))
            (display game)
            (sleep micro-delay)))
        (progn
          (display game)
          (sleep (sleep-delay game))))))

(defmethod display ((game marching-squares))
  (with-accessors ((palette palette)) game
    (apply #'gl:clear-color (palette-background palette))
    (gl:clear :color-buffer :depth-buffer)
    (display (game-level game))))

(defparameter *shadow* 0.1)
(defun shadow-square (&aux (shadow *shadow*))
  (gl:rect shadow shadow (1+ shadow) (1+ shadow)))

(defmethod display ((cell (eql :wall)))
  (let ((wall-color (palette-wall (palette *game*))))
    (apply #'gl:color wall-color)
    (gl:rect 0 0 1 1)))

(defun set-color (reader &key alpha palette)
  (destructuring-bind (r g b a) (funcall reader (or palette
                                                    (palette *game*)))
    (gl:color r g b (or alpha a))))

(defmethod delta-microstep ((trigger inverter) dt)
  (setf (angle trigger)
        (mod (round (+ (angle trigger)
                       (/ (* dt 180)
                          internal-time-units-per-second)))
             360))
  (setf (dy trigger)
        (destructuring-bind (north south) (neighbours trigger :n :s)
          (if (or (objects-at north) (objects-at south))
              (if (< (dy trigger) 0.01) 0 (* (dy trigger) 0.9))
              (/ (sin (* #.(/ pi 180) (angle trigger))) 6)))))

(defmethod transform-model-view ((trigger inverter))
  (let ((s (+ 0.9 (random 0.3))))
    (gl:translate 0.5 (+ 0.5 (dy trigger)) 0)
    (gl:scale s s 1)
    (gl:rotate 45 1 0 1)
    (gl:rotate (- (angle trigger)) 0 1 0)))

(declaim (inline csq))
(defun csq (size)
  (gl:rect (- size) (- size) size size))

(defmethod display ((trigger inverter))
  (set-color #'palette-square :alpha 0.35)
  (csq 0.2)
  (gl:translate 0 0 0.05)
  (csq 0.1)
  (gl:translate 0 0 -0.1)
  (csq 0.1)
  (set-color #'palette-inverter)
  (csq 0.05))

(defmethod update ((game marching-squares))
  (let ((direction (direction game)))
    (dogroup (mobile (mobiles game))
      (setf (direction mobile) direction)))
  (setf (direction game) nil)
  (update (mobiles game))
  (update (active-objects game))
  (arbiter-moves game (mobiles game))
  (trigger (triggers game))  
  (update (game-level game)))

(defmethod allow-move-p (mobile (wall (eql :wall))) nil)

(defmethod compute-next-move ((square square))
  (unless (blockedp square)
    (let* ((location (location square))
           (down (first (neighbours location :s))))
      (if (allow-move-p square down)
          (values :fall down)
          (let ((up (first (neighbours location :n))))
            (when (allow-move-p square up)
              (case (direction square)
                (:left (destructuring-bind (nw w)
                           (neighbours location :nw :w)
                         (when (and (allow-move-p square nw)
                                    (allow-move-p square w))
                           (values :left w))))
                (:right (destructuring-bind (ne e)
                            (neighbours location :ne :e)
                          (when (and (allow-move-p square ne)
                                     (allow-move-p square e))
                            (values :right e)))))))))))

(defparameter *intro-level*
  (make-instance
   'level-blueprint
   :width 31
   :height 31
   :grid #("               V                  "
           "                                  "
           "                                  "
           "           #H#######-#####        "
           "           ######### #####        "
           "                       ###        "
           "             #####  b  ###        "
           "             ##B##     ###        "
           "                       ###        "
           "                       ###        "
           "     ########=###=########        "
           "     ###       #                  "
           "               #       8          "
           "     #########:#:#########        "
           "                                  "
           "                                  "
           " # ########~########~####### #    "
           "           e###E#                 "
           "                                  "
           "           ##     ######          "
           "                #                 "
           "          #### ##  #              "
           "                 #       XXXXX    "
           "           ##     8#              "
           "         #    ##8#                "
           "           # ###   #              "
           "          #      #                "
           "                 #                "
           "      ###^###^#####^##            "
           "                                  "
           "XXXXXXXXX@XXX@XXXXX@XXXXXXXXXXXXXXXXXXX")
   :bindings '((#\b . (:trigger :release x))
               (#\B . (:blocked-square x))
               (#\e . (:trigger :release y))
               (#\E . (:blocked-square y))
               (#\f . (:trigger :release z))
               (#\F . (:blocked-square z))
            ;; (#\X . (:trigger :lose))
               (#\@ . (:trigger :win))
               (#\8 . (:trigger :invert))
               (#\V . :start)
               (#\H . :help)
               (#\- . (:door door-1))
               (#\= . (:door door-2))
               (#\~ . (:door door-3))
               (#\^ . (:door door-4))
               (#\: . (:door door-5)))))

(defparameter *ramping-level*
  (make-instance
   'level-blueprint
   :width 31
   :height 31
   :grid #("         V   W     V                     "
           "                       ###########       "
           "############# ######################    "
           "#######              ######################    "
           "########             #####################    "
           "#######              ##############      "
           "####BBB####=##=##=###BBB###########         "
           "####BB                BB###########         "
           "####B         b        B########  "
           "                                  "
           "           8     8                "
           "              8                   "
           "                                  "
           "          ############### ###########       "
           "    #####    #            ###########               "
           "     #                    ###########               "
           " #           #            ###########               "
           "### ## ### # ########## # ###########               "
           "### ##^###^# # ########^# ###########               "
           "### ##     # # ###      # ###########               "
           "### ##     # # #E#      # ###########               "
           "### ##     # #         e# ###########               "
           "### ##     # #          # ###########               "
           "###^##     ###          #^###########               "
           "                                                    "
           "%%%%%%%%%%%%%%%   %%%%%%%%%%%%%%%%%%%%%%%%%%"
           "                                                    "
           "               #                                     "
           "               ##                                    "
           "                                                     "
           "                 @                                   ")
   :bindings '((#\b . (:trigger :release x))
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
               (#\- . (:door door-1))
               (#\= . (:door door-2))
               (#\~ . (:door door-3))
               (#\^ . (:door door-4))
               (#\: . (:door door-5)))))

