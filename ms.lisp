(in-package :marching-squares)

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
  (and (find-if #'squarep (objects-at (location trigger)))
       (call-next-method)))

(defmethod triggerable ((inverter inverter))
  (find-if #'invertiblep (objects-at (location inverter))))

(defmethod trigger ((inverter inverter))
  (map () #'invert (objects-at (location inverter))))

(defclass helper (square-trigger global-trigger)
  ((text :initarg :text
         :accessor text
         :initform "")))

(defclass invisible-helper (helper invisible) ())

(defclass visible-helper (helper has-absolute-microstep oneshot)
  ((angle :initform 0 :accessor visible-helper-angle)))

(defmethod delta-microstep ((h visible-helper) dt)
  (setf (visible-helper-angle h)
        (mod (+ (visible-helper-angle h)
                (/ dt internal-time-units-per-second))
             #.(* 2 pi))))

(defmethod display ((helper visible-helper))
  (let ((angle (* (visible-helper-angle helper) #.(/ 180 pi))))
    (gl:translate 1/2 1/2 1/2)
    (color :square)
    (gl:rotate angle 0.2 0.1 0.1)
    (csq 1/10)
    (gl:translate 0 0 1/10)
    (gl:rotate angle 0.2 0.1 0.1)
    (color :wall)
    (gl:translate 0 0 1/10)
    (gl:rotate angle 0 0 1)
    (csq 1/10)
    (color :help)
    (gl:translate 0 0 1/10)
    (gl:rotate angle 1 0 0)
    (csq 1/10)))

(defmethod trigger ((helper helper))
  (set-title (text helper)))

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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *dispatcher-function* (constantly nil))
  (defun parse-action (expression level)
    (funcall *dispatcher-function* expression level))
  (defparameter *actions* (make-hash-table :test #'equalp))

  (defmacro defaction (pattern (level-var) &body body)
    (setf (gethash pattern *actions*) (list level-var body))
    (let ((expression (copy-symbol :expression))
          (level (copy-symbol :level)))
      `(setf *dispatcher-function*
             (lambda  (,expression ,level)
               (check-type ,level level)
               (optima:ematch ,expression
                 ,@(loop
                     for (pattern level-var body) in (hash-table-alist *actions*)
                     collect (list pattern
                                   `(compile
                                     nil
                                     (lambda (&aux (,level-var ,level))
                                       ,@body))))))))))

(defaction (list* :trigger names) (level)
  (dolist (name names)
    (trigger-by-name name level)))

(defaction (list* :release names) (level)
  (dolist (name names)
    (release level name)))

(defclass invisible-blocker (has-location
                             immaterial
                             invisible)
  ())

(defmethod build ((f function) context)
  (build (funcall f) context))

(defmethod build ((s symbol) context)
  (if (and s (fboundp s))
      (progn
        (warn "deprecated: use function object instead")
        (build (funcall s) context))
      (call-next-method)))

(defun some-square-p (place)
  (find-if #'squarep (objects-at place)))

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
      ((list* :help! message)
       (new 'visible-helper :text (ensure-car message)))
      ((list* :help message) (new 'invisible-helper :text (ensure-car message)))
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
      ((or (list :trigger :invert)
	   :invert)
       (new 'inverter))
      ((list* :class class initargs) (apply #'make-instance class initargs))
      ((list* :class/loc class initargs) (apply #'new class initargs))
      ((list :trigger :lose) (new 'looser))
      ((list :trigger :win) (new 'winner))
      ((or (list :trigger :release name)
	   (list :release name))
       (new 'releaser :group-name `(:releaser-for ,name)
                      :predicate #'some-square-p
                      :group-class 'release-group
                      :combination :or
                      :target name))
      ((list :level s)
       (build (funcall s) location))
      (e
       (warn "default case: ~a" e)
       (check-type location loc)
       (add-object-at-location%% location e)))))

;;;; GAME

(defclass winner-group (named-group
                        global-trigger
                        oneshot)
  ())

(defmethod trigger ((w winner-group))
  (win *level*))

(defclass winner (square-trigger
                  has-group
                  has-absolute-microstep)
  ((up :initform 0 :accessor up)
   (counter :initform 0 :accessor counter))
  (:default-initargs :combination :and
                     :group-class 'winner-group
                     :group-name :win))


(defclass looser (square-lambda invisible) ()
  (:default-initargs :action (lambda () (loose *game*))))

(defmethod delta-microstep ((trigger winner) dt)
  (setf (counter trigger)
        (mod (+ (counter trigger)
                (/ dt internal-time-units-per-second 1/4))
             #.(* 2 pi)))
  (setf (up trigger)
        (+ -.1 (cos (counter trigger)))))

(defmethod display ((trigger winner))
  (gl:color (up trigger) (up trigger) (up trigger) .1)
  (gl:rect 0 0.4 1 1)
  (gl:rect 0 0.6 1 1)
  (gl:rect 0 0.9 1 1))

(define-condition restart-game-signal () ())
(define-condition restart-window () ())
(define-condition quit-game () ())

(defun restart-game-loop (&rest args)
  (declare (ignore args))
  (invoke-restart 'restart-game-loop))

(defun quit-game (&rest args)
  (declare (ignore args))
  (signal 'quit-game))

(defgeneric game-setup (game)
  (:method-combination progn)
  (:method progn (_)))

(defun sdl2-break (&aux (all (bordeaux-threads:all-threads)))
  (bt:interrupt-thread (or (find "SDL2"
                                 all
                                 :test #'search
                                 :key #'bt:thread-name)
                           (error "Not found in ~s" all))
                       #'break))

;; (defun mouse-command (mouse-button)
;;   (case mouse-button
;;     (#.sdl2-ffi:+sdl-button-left+  :go-left)
;;     (#.sdl2-ffi:+sdl-button-right+ :go-right)))

(defgeneric game-loop (game)
  (:method (game)
    (flet ((command (k) (keybind (scancode-value k) game)))
      (do-match-events (:method :poll)
        (with-key-up-event (_ :keysym keysym :repeat repeat)
          (when (zerop repeat)
            (game-cancel-command game (command keysym))))
        (with-key-down-event (_ :keysym keysym :repeat repeat)
          (when (zerop repeat)
            (game-command game (command keysym))))
        ;; (with-mouse-button-down-event (_ :button button)
        ;;   (game-command game (mouse-command button)))
        ;; (with-mouse-button-up-event (_ :button button)
        ;;   (game-cancel-command game (mouse-command button)))
        (with-window-event-resized (_ :width width :height height)
          (resize-game game width height))
        (:quit () (return))
        (:idle () (game-idle game))))))

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
           (declare (ignore width height))
           (with-window (*window* :w 800 ;;(round width)
                                  :h 600 ;; (round height)
                                  :title (title game)
                                  :flags '(:shown :opengl :resizable))
             (with-gl-context (*gl* *window*)
               (with-renderer (*renderer* *window*)
                 (gl-make-current *window* *gl*)
                 (game-setup game)
                 (block :main
                   (handler-bind ((restart-game-signal #'restart-game-loop)
                                  (quit-game (lambda (c)
                                               (declare (ignore c))
                                               (return-from :main)))
                                  (restart-window
                                    (lambda (condition)
                                      (declare (ignore condition))
                                      (go create-window))))
                     (tagbody
                      start
                        (restart-case (game-loop game)
                          (restart-game-loop ()
                            :report "Restart game loop"
                            (go start))))))))))))))

(defgeneric game-command (game command)
  (:method (game command) nil)
  (:method (game (command function)) (funcall command)))

(defgeneric game-cancel-command (game command)
  (:method (game command))
  (:documentation "Called when an ongoing action should be canceled"))

(defclass locked ()
  ((lock :initform (bt:make-lock) :reader lock)))

(defgeneric game-idle (game)
  (:method :around ((object locked))
    (bt:with-lock-held ((lock object))
      (call-next-method))))

;;;; MARCHING-SQUARES

;; recorded inputs

(defclass input-sequence ()
  ((inputs :initarg :inputs :accessor inputs :initform nil)
   (counter :initform 0 :accessor counter)))

(defmethod update ((state input-sequence))
  (with-accessors ((counter counter) (inputs inputs)) state
    (loop
      (if inputs
          (let ((top (first inputs)))
            (ematch top
              ((list (and (or nil :left :right) direction)
                     (guard count (typep count '(integer 0))))
               (cond
                 ((zerop count)
                  (pop inputs))
                 ((= count counter)
                  (setf counter 0)
                  (pop inputs))
                 (t
                  (incf counter)
                  (return direction))))
              ((or nil :left :right)
               (return (pop inputs)))
              ((or :wait (list :wait :stability))
               (cond
                 ((notevery (lambda (u) (eq (next-move u) nil))
                            (items (mobiles *game*)))
                  (setf counter 0)
                  (return nil))
                 ((= (incf counter) 1)
                  (setf counter 0)
                  (pop inputs))
                 (t (return nil))))))
          (return nil)))))

;; manual inputs

(defstruct square-input
  ;; direction on x-axis, retained from one step to another
  (direction 0)
  ;; last "pressed" direction in current step
  (command 0)
  ;; last "unpressed" direction in current step
  (stop 0))

(defmethod game-command ((input square-input) (command (eql :go-left)))
  (setf (square-input-direction input) -1)
  (decf (square-input-command input)))

(defmethod game-command ((input square-input) (command (eql :go-right)))
  (setf (square-input-direction input) 1)
  (incf (square-input-command input)))

(defmethod game-cancel-command ((input square-input) (command (eql :go-left)))
  (decf (square-input-stop input)))

(defmethod game-cancel-command ((input square-input) (command (eql :go-right)))
  (incf (square-input-stop input)))

(let (#+ms-debug
      (last nil))
  (defun square-input-step (inputs)
    (let ((d (square-input-direction inputs))
          (s (square-input-stop inputs))
          (c (square-input-command inputs)))
      (setf (square-input-command inputs) 0)
      (let ((actual-direction
              (ecase (cond
                       ((zerop d) 0)
                       ((zerop s) d)
                       ((zerop c) 0)
                       (t d))
                (0 nil)
                (1 :right)
                (-1 :left))))
        (when (and (not (zerop s))
                   (= (signum s) (signum d)))
          (setf (square-input-direction inputs) 0))
        (setf (square-input-stop inputs) 0)
        #+ms-debug
        (let ((debug `(:d ,d :s ,s :c ,c :=> ,actual-direction)))
          (unless (equalp debug last)
            (print debug))
          (shiftf last debug))
        actual-direction))))

(defclass marching-squares (game locked)
  ((input-state
    :accessor input-state
    :initform (make-square-input))
   (extra :initform nil :accessor extra))
  (:default-initargs
   :direction nil
   :title "marching-squares"
   :width 31
   :height 31))

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
    (gl:ortho 0 (width game) (height game) 0 -1 1)))

(defmethod game-setup progn ((game marching-squares))
  (gl:enable :blend)
  ;; (gl:enable :depth-test)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:blend-equation :func-add)
  (gl:clear-color 0.4 0.4 0.3 1.0)
  (gl:clear :color-buffer))

(defmethod game-command ((game marching-squares) (command symbol))
  (case command
    (:restart-loop (restart-game-loop))
    ((:go-left :go-right)
     (game-command (input-state game) command))
    (:restart (restart-game-loop))
    (:restart-graphics (signal 'restart-window))))

(defmethod game-cancel-command ((game marching-squares)
                                (command symbol))
  (case command
    ((:go-left :go-right)
     (game-cancel-command (input-state game) command))))

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
  (setf (keybind :scancode-h game) :go-left)
  (setf (keybind :scancode-j game) :go-right)
  (setf (keybind :scancode-escape game) :restart)
  (setf (keybind :scancode-f2 game) :restart-loop)
  (setf (keybind :scancode-f3 game) :break))

(defparameter *test-level* nil)

;; (defmethod (setf microsteps-duration) (value (game game))
;;   (call-next-method (rationalize value)))

(defmethod game-loop :before ((game marching-squares))
  (reinitialize-instance game)
  (sdl2:set-window-title *window* (title *game*))
  (let ((level (build (level-blueprint *game*) *game*)))
    (setf (game-level *game*) level)
    (setf (width game) (width level))
    (setf (height game) (height level))
    (multiple-value-bind (w h) (sdl2:get-window-size *window*)
      (resize-game game w h))
    (trigger-by-name :start level)))

(defmethod display :after ((game marching-squares))
  (gl:flush)
  (sdl2:gl-swap-window *window*))

(defmethod game-idle :around ((game marching-squares))
  (loop
    (restart-case (return (call-next-method))
      (accept () :report "Try next loop iteration"))))

;; (microsteps *game*)

(defmethod game-idle ((game marching-squares))
  (update game)
  (let ((microsteps (microsteps game))
        (duration  (microsteps-duration game)))
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

;; (defvar *depth* 0)

;; (defmethod display :around (_)
;;   (let ((before (get-internal-run-time)))
;;     (let ((*depth* (1+ *depth*)))
;;       (call-next-method))
;;     (let ((delay (- (get-internal-run-time) before)))
;;       (when (> delay 99)
;;         (format t "~v@a " *depth* delay)))))

(defparameter *shadow* 0.1)
(defun shadow-square (&aux (shadow *shadow*))
  (gl:rect shadow shadow (1+ shadow) (1+ shadow)))

(defparameter *grid* nil)

(defmethod display ((cell (eql :wall)))
  (color :wall)
  (gl:rect 0 0 1 1))

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
  (let ((s 0.5))
    (gl:translate 0.5 (+ 0.5 (dy trigger)) 0)
    (gl:scale s s 1)
    (gl:rotate (- (angle trigger)) 0 1 0.5)))

(declaim (inline csq))
(defun csq (size)
  (gl:rect (- size) (- size) size size))

(defmethod display ((trigger inverter))
  (color '(:alpha 0.7 :square))
  (csq 0.4)
  (color '(:alpha 0.8 :WALL))
  (csq 0.2))

(defgeneric propagate-inputs (item))

(defmethod update ((arbiter move-arbiter))
  (update (mobiles arbiter))
  (call-next-method)
  (arbiter-moves arbiter (mobiles arbiter)))

(defmethod update ((object has-active-objects))
  (update (active-objects object))
  (call-next-method))

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

(defmethod update ((state square-input))
  (square-input-step state))

(defmethod propagate-inputs ((game marching-squares))
  (let ((direction (update (input-state game))))
    (dogroup (mobile (mobiles game))
      (setf (direction mobile) direction))))

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
                (:left
		 (destructuring-bind (nw w) (neighbours location :nw :w)
                   (when (and (allow-move-p square nw)
                              (allow-move-p square w))
                     (values :left w))))
                (:right
		 (destructuring-bind (ne e) (neighbours location :ne :e)
                   (when (and (allow-move-p square ne)
                              (allow-move-p square e))
                     (values :right e)))))))))))

(defparameter *game*
  (make-instance 'marching-squares
                 :level-blueprint 'intro-level))
