(defpackage :marching-squares
  (:use
   :bricabrac.sdl2.event-loop
   :cl
   :alexandria)
  (:import-from :sdl2
                #:set-render-draw-color
                #:scancode-key-to-value
                #:scancode-value
                #:with-init
                #:with-window
                #:with-gl-context
                #:with-renderer
                #:with-event-loop
                #:gl-make-current)
  (:import-from :trivia
                #:match))

(in-package :marching-squares)

;; size (better if multiple of 10)
(defparameter *size* 30)

(defvar *gl*)
(defvar *window*)
(defvar *renderer*)
(defvar *game*)

(defclass square-trigger (has-location
                          trigger)
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

(defclass release-group (named-group
                         global-trigger
                         oneshot)
  ())

(defclass releaser (square-trigger
                    has-absolute-microstep
                    transformable
                    has-group)
  ((alpha :accessor alpha :initform 0)
   (counter :accessor counter :initform 0)
   (target :accessor target :initarg :target)))

(defmethod trigger ((releaser releaser))
  (let ((level (level (location releaser)))
        (target (target releaser)))
    (release level target)))

(defmethod transform-model-view ((trigger releaser))
  (gl:translate 0.5 0.5 0.5)
  (gl:rotate (* 3 (counter trigger) #.(/ 180 pi)) 0 0 1))

(defmethod display ((trigger releaser))
  (color `(:alpha ,(alpha trigger) :foreground))
  (csq 0.1))

(defclass helper (square-trigger global-trigger invisible)
  ((text :initarg :text
         :accessor text
         :initform "")))

(defmethod trigger ((helper helper))
  (set-title (text helper)))

(defmethod delta-microstep ((trigger releaser) delta)
  (setf (counter trigger)
        (mod (+ (counter trigger)
                (/ delta internal-time-units-per-second 1/16))
             #.(* 2 pi)))
  (setf (alpha trigger)
        (- 1 (abs (/ (sin (counter trigger)) 6)))))

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
  (let ((square (make-instance 'square
                               :location (location trigger)
                               :inverted t)))
    (incorporate (location trigger) square)))

(define-symbol-macro *level* (game-level *game*))

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

(defmethod display((vanisher vanisher))
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
  (optima:ematch expression
    ((list* :release names)
     (lambda ()
       (dolist (name names)
         (release level name))))
    ((list* :trigger names)
     (lambda ()
       (dolist (name names)
         (trigger-by-name name level))))))

(defclass invisible-blocker (has-location
                             immaterial
                             invisible)
  ())

(defmethod build ((s symbol) context)
  (if (and s (fboundp s))
      (build (funcall s) context)
      (call-next-method)))

;; FIXME: not all in a single function
(defmethod build (expression location)
  (flet ((new (class &rest args)
           (apply #'make-instance
                  class
                  :location location
                  args)))
    (match expression
      ((eq nil) nil)
      ((list :spawn name) (new 'spawn-trigger :name name))
      ((eq :start) (new 'start-trigger))
      ((list* :help message) (new 'helper :text (ensure-car message)))
      ((eq :vanisher) (new 'vanisher))
      ((list :press-button group action)
       (make-button group location
                    (parse-action action (level location))
                    :latchp t))
      ((list :button group action)
       (etypecase group
         (symbol (make-button group
                              location
                              (parse-action action (level location))))))
      ((list :start :inverted) (new 'inverted-start-trigger))
      ((list* :gate name options)
       (apply #'make-door name :location location :pressp nil options)) 
      ((list :door name) (make-door name :location location :pressp t))
      ((list :blocked-square name) (new 'square :name name :blockedp t))
      ((list :invisible-blocker) (new 'invisible-blocker))
      ((list :trigger :invert) (new 'inverter))
      ((list* :class class initargs) (apply #'make-instance class initargs))
      ((list* :class/loc class initargs) (apply #'new class initargs))
      ((list :trigger :lose) (new 'looser))
      ((list :trigger :win) (new 'winner))
      ((list :trigger :release name)
       (new 'releaser :group-name `(:releaser-for ,name)
                      :group-class 'release-group
                      :combination :or
                      :target name))
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
  (gl:rect 0 0.6 1 1))

(define-condition restart-game-signal () ())
(define-condition restart-window () ())

(defun restart-game-loop (&rest args)
  (declare (ignore args))
  (invoke-restart 'restart-game-loop))

(defgeneric game-setup (game)
  (:method-combination progn)
  (:method progn (_)))

(use-package :bricabrac.sdl2.event-loop)

(defun sdl2-break (&aux (all (sb-thread:list-all-threads)))
  (sb-thread:interrupt-thread (or (find "SDL2"
                                        all
                                        :test #'search
                                        :key #'sb-thread:thread-name)
                                  (error "Not found in ~s" all))
                              #'break))

(defgeneric game-loop (game)
  (:method (game)
    (do-match-events (:method :poll)
      (with-key-down-event (_ :keysym keysym)
        (game-command game (keybind (scancode-value keysym) game)))
      (with-window-event-resized (_ :width width :height height)
        (resize-game game width height))
      (:quit () (return))
      (:idle () (game-idle game)))))

(defvar *display* 0)

(defun display-size (&optional (index *display*))
  (let ((displays (sdl2:get-num-video-displays)))
    (check-type displays (integer 1 *))
    (check-type index (integer 0 *))
    (if (<= index displays)
        (sdl2:get-display-bounds index)
        (error "Bad index ~d (max. is ~d)" index displays))))

(defparameter *size-ratio* 1/2)
(defparameter *min-dimension* 200)
(defparameter *max-dimension* 500)

(defun adjust-size (size &optional
                           (min *min-dimension*)
                           (max *max-dimension*)
                           (ratio *size-ratio*))
  (assert (<= 0 min max))
  (assert (<= 0 ratio 1))
  (clamp (* size ratio) min max))

;; aspect ratio is width/height
(defun window-dimensions (aspect-ratio)
  (let ((display (display-size)))
    (if (>= aspect-ratio 1)
        ;; large game
        (let ((width (adjust-size (sdl2:rect-width display))))
          (values width (/ width aspect-ratio)))
        ;; tall game
        (let ((height (adjust-size (sdl2:rect-height display))))
          (values (* height aspect-ratio) height)))))

(defgeneric start-game (game)
  (:method (game)
    (with-init (:everything)
      ;; In graphical thread
      (tagbody
       create-window
         (multiple-value-bind (width height)
             (window-dimensions (/ (width game) (height game)))
           (with-window (*window* :w (round width)
                                  :h (round height)
                                  :title (title game)
                                  :flags '(:shown :opengl :resizable))
             (with-gl-context (*gl* *window*)
               (with-renderer (*renderer* *window*)
                 (gl-make-current *window* *gl*)
                 (game-setup game)
                 (handler-bind ((restart-game-signal #'restart-game-loop)
                                (restart-window
                                  (lambda (condition)
                                    (declare (ignore condition))
                                    (go create-window))))
                   (tagbody
                    start
                      (restart-case (game-loop game)
                        (restart-game-loop ()
                          :report "Restart game loop"
                          (go start)))))))))))))

(defgeneric game-command (game command)
  (:method (game command) nil)
  (:method (game (command function)) (funcall command)))

(defclass locked ()
  ((lock :initform (bt:make-lock) :reader lock)))

(defgeneric game-idle (game)
  (:method :around ((object locked))
    (bt:with-lock-held ((lock object))
      (call-next-method))))

;;;; MARCHING-SQUARES

(defclass marching-squares (has-palette
                            game
                            locked)
  ()
  (:default-initargs
   :direction nil
   :title "Marching squares"
   :width 31
   :height 31
   :palette *palette*))

(defun fill-view (width height)
  (let* ((max (max width height))
         (w-delta (/ (- max width) 2))
         (h-delta (/ (- max height) 2)))
    (values (ceiling (- w-delta))
            (ceiling (- h-delta))
            max
            max)))

(defun shrink-view (width height)
  (let* ((min (min width height))
         (w-delta (/ (- min width) 2))
         (h-delta (/ (- min height) 2)))
    (values (ceiling (- w-delta))
            (ceiling (- h-delta))
            min
            min)))

(defgeneric resize-game (game width height)
  (:method ((game marching-squares) width height)
    (multiple-value-call #'gl:viewport (shrink-view width height))
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:ortho -1 1 -1 1 -1 1)))

(defmethod game-setup progn ((game marching-squares))
  (gl:enable :blend)
  ;; (gl:enable :depth-test)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:blend-equation :func-add)
  (gl:clear-color 0.4 0.4 0.3 1.0)
  (gl:clear :color-buffer)
  (gl:ortho 0 31 31 0 -1 1))

(defmethod game-command ((game marching-squares) (command symbol))
  (case command
    ;; (:break (break))
    (:restart-loop (restart-game-loop))
    (:go-left (setf (direction game) :left))
    (:go-right (setf (direction game) :right))
    (:restart (restart-game-loop))
    (:restart-graphics (signal 'restart-window))))

(defmethod arbiter-compare
    ((arbiter marching-squares) location first second)
  "Game arbiter"
  (or (and (eq :fall (move-type first))
           (not (eq :fall (move-type second))))
      (and (not (invertedp (move-object first)))
           (invertedp (move-object second)))))

(defmethod initialize-instance :after
    ((game marching-squares) &key &allow-other-keys)
  (setf (keybind :scancode-f1 game) :restart-graphics)
  (setf (keybind :scancode-left game) :go-left)
  (setf (keybind :scancode-right game) :go-right)
  (setf (keybind :scancode-escape game) :restart)
  (setf (keybind :scancode-f2 game) :restart-loop)
  (setf (keybind :scancode-f3 game) :break))

;; NO: e.g. prepare next blueprint while level is playing
;;
;; (defmethod (setf level-blueprint) :after ((blueprint has-dimensions)
;;                                           (game game))
;;   (setf (width game) (width blueprint)
;;         (height game) (height blueprint)))

(defparameter *test-level* nil)

(defmethod game-loop :before ((game marching-squares))
  (reinitialize-instance game)
  (sdl2:set-window-title *window* (title *game*))
  (setf (game-level *game*)
        (build (level-blueprint *game*) *game*))
  (trigger-by-name :start (game-level *game*)))

(defmethod display :after ((game marching-squares))
  (gl:flush)
  (sdl2:gl-swap-window *window*))

(defmethod game-idle :around ((game marching-squares))
  (loop
    (restart-case (return (call-next-method))
      (accept () :report "Try next loop iteration"))))

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
  (display (game-level game)))

(defparameter *shadow* 0.1)
(defun shadow-square (&aux (shadow *shadow*))
  (gl:rect shadow shadow (1+ shadow) (1+ shadow)))

(defparameter *grid* nil)

(defmethod display ((cell (eql :wall)))
  (color :wall)
  (if *grid*
      (gl:rect 0.1 0.1 0.9 0.9)
      (gl:rect 0 0 1 1)))

(defmethod delta-microstep ((trigger inverter) dt)
  (setf (angle trigger)
        (mod (round (+ (angle trigger)
                       (/ (* dt 180)
                          internal-time-units-per-second
                          2)))
             360))
  (setf (dy trigger)
        (destructuring-bind (north south) (neighbours trigger :n :s)
          (if (or (objects-at north) (objects-at south))
              (if (< (dy trigger) 0.01) 0 (* (dy trigger) 0.9))
              (/ (sin (* #.(/ pi 180) (angle trigger))) 6)))))

(defmethod transform-model-view ((trigger inverter))
  (let ((s (+ 0.9 (random 0.15))))
    (gl:translate 0.5 (+ 0.5 (dy trigger)) 0)
    (gl:scale s s 1)
    (gl:rotate 50 1 0 1)
    (gl:rotate (- (angle trigger)) 0 1 0)))

(declaim (inline csq))
(defun csq (size)
  (gl:rect (- size) (- size) size size))

(defmethod display ((trigger inverter))
  (color '(:alpha 0.8 :wall))
  (csq 0.2)
  (color '(:alpha 0.8 :inverter))
  (csq 0.15)
  (color '(:alpha 0.4 :square))
  (gl:translate 0 0 -0.1)
  (csq 0.1)
  (gl:translate 0 0 +0.2)
  (csq 0.1))

(defgeneric propagate-inputs (item))

(defmethod update ((arbiter move-arbiter))
  (update (mobiles arbiter))
  (call-next-method)
  (arbiter-moves arbiter (mobiles arbiter)))

;; (defmethod update :after ((arbiter move-arbiter))
;;   (arbiter-moves arbiter (mobiles arbiter)))

(defmethod update ((object has-active-objects))
  (update (active-objects object))
  (call-next-method))

;; ;; override any order existing from applicable method
;; (defmethod update ((game game))
;;   (propagate-inputs game)
;;   (update (mobiles game))
;;   ;;; ????!!!!
;;   (trigger (remove-if (lambda (u) (typep u 'button-group))
;;                       (items (triggers *game*))))
;;   (arbiter-moves game (mobiles game))
;;   (update (active-objects game))
;;   (trigger (triggers game))
;;   (update (game-level game)))

;; override any order existing from applicable method
(defmethod update ((game game))
  (propagate-inputs game)
  ;; move to next position
  (update (mobiles game))
  ;; terminate last action
  (update (active-objects game))
  ;; maybe trigger changes
  (trigger (triggers game))
  ;; update level
  ;; based on new object states and inputs, compute next moves
  (arbiter-moves game (mobiles game))
  (post-move-update (active-objects game))
  (update (game-level game)))

(defmethod update ((object has-triggers))
  (trigger (triggers object))
  (call-next-method))

(defmethod propagate-inputs ((game game))
  (let ((direction (direction game)))
    (dogroup (mobile (mobiles game))
      (setf (direction mobile) direction)))
  (setf (direction game) nil))

(defmethod allow-move-p (mobile (wall (eql :wall))) nil)

(defmethod compute-next-move ((square abstract-square))
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

;;(setf (level-blueprint *game*) *intro-level*)

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
           "%%%%%%%%%%%%%%%# #%%%%%%%%%%%%%%%%%%%%%%%%%%"
           "               # #                                  "
           "               #                                     "
           "               #                                     "
           "               #####                                 "
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



