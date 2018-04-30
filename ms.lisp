(defpackage ms (:use :cl))
(in-package ms)

(defparameter *size* 15)

(defclass has-update-category ()
  ((update-category
    :accessor update-category
    :initarg :update-category
    :initform nil)))

(defclass has-position ()
  ((level :initarg :level :accessor level)
   (row :initarg :row :accessor row)
   (col :initarg :col :accessor col)))

(defclass has-direction ()
  ((direction :initform nil :accessor direction)))

(defclass has-name ()
  ((name :accessor name :initarg :name)))

(defmacro let1 ((var expr) &body body)
  `(let ((,var ,expr))
     (prog1 ,var
       ,@body)))

(defclass has-level-source ()
  ((description :accessor level-source-description :initarg :description)
   (bindings :accessor level-source-bindings :initarg :bindings)))

(defclass has-animation-state ()
  ((step-function :initform nil :accessor animation-step-function)))

(defclass level (has-level-source)
  ((array :accessor level-array :initarg :array)
   (next :accessor level-next :initarg :next)
   (groups :accessor level-groups :initform (make-hash-table :test #'eq))
   (categories :accessor level-categories
               :initform (make-hash-table :test #'eq))))

(defvar *default-bindings*
  '((#\space . nil)
    (#\# . :wall)
    (#\- . :door)))

(defun make-level (lines &optional bindings next
                   &aux (rows (length lines)))
  (let* ((array (make-array (list rows 32)))
         (level (make-instance 'level
                               :array array
                               :next next
                               :description lines
                               :bindings bindings)))
    (prog1 level
      (dotimes (row rows)
        (dotimes (col 32)
          (setf (aref array row col)
                (create level
                        (aref (aref lines row) col)
                        :bindings bindings
                        :row row
                        :col col)))))))

(defgeneric created (level created)
  (:method (level object)))

(defgeneric create (level item &key bindings row col)
  (:method (level (item character) &key bindings row col)
    (flet ((resolve (alist)
             (let ((entry (assoc item alist)))
               (and entry
                    (create level
                            (cdr entry)
                            :bindings bindings
                            :row row
                            :col col)))))
      (or (resolve bindings)
          (resolve *default-bindings*))))
  (:method :around (level (item character) &key &allow-other-keys)
    (let1 (result (call-next-method))
      (created level result))))

(defgeneric release (level object)
  (:method ((level level) (name symbol))
    (map () #'release-element (gethash name (level-groups level)))))

(defun parse-action (level row col form)
  (declare (ignore row col))
  (trivia:ematch form
    ((list :lose) (lambda () (trigger-event *game* :restart)))
    ((list :win)  (lambda ()
                    (trigger-event *game* :next-level)))
    ((list :release name) (lambda () (release level name)))))

(defun register (level object name)
  (prog1 object
    (push object (gethash name (level-groups level)))))

(defun make-door (level name row col)
  (register level
            (make-instance 'door
                           :name name
                           :level level
                           :row row
                           :col col)
            name))

(defmethod created ((level level) (element has-update-category))
  (let ((category (update-category element)))
    (when category
      (pushnew element
               (gethash category (level-categories level))))))

(defun activate-square (level row col &optional square)
  (let ((square (or square
                    (make-instance 'square
                                   :level level
                                   :row row
                                   :col col))))
    (setf (aref (level-array level) row col) square)
    (pushnew square (game-squares *game*))))

(defclass door-group ()
  ((doors :accessor group-doors
          :initform (make-array 10 :fill-pointer 0 :adjustable t))))

(defclass door (has-update-category has-position has-name)
  ((state :accessor state :initform :closed)
   (group :accessor group :initarg :group)
   (openness :accessor openness :initform 0))
  (:default-initargs :update-category :door))

(defclass inverter (trigger) ())

(defclass square (has-update-category
                  has-position
                  has-direction)
  ((offset-x :initform 0 :accessor offset-x)
   (offset-y :initform 0 :accessor offset-y)
   (angle :initform 0 :accessor angle)
   (next-position :initform nil :accessor next-position)
   (state :accessor state :initform nil))
  (:default-initargs :update-category :square))

(defclass blocked-square (square) ())

(defgeneric release-element (element)
  (:method ((square blocked-square))
    (change-class square 'square)
    (activate-square (level square) (row square) (col square) square)))

(defclass inverted-square (square) ())
(defmethod (setf direction) (v (square inverted-square))
  (call-next-method (case v (:left :right) (:right :left) (t v)) square))

(defgeneric invert (square)
  (:method ((sq inverted-square)) (change-class sq 'square))
  (:method ((sq square)) (change-class sq 'inverted-square))
  (:method (_)))

(defmethod create (level item &key row col &allow-other-keys)
  (case item
    ((nil) nil)
    ;; (:door (make-door level :door row col))
    ((:restart :lose :win) (trigger-event *game* item))
    (:inverter
     (make-instance 'inverter
                    :action (lambda () (invert *event*))))
    (:start (make-instance 'spawn
                           :action (lambda ()
                                     (activate-square level row col))))
    (t
     (trivia:match item
       ((list :door name)
        (let* ((group (or #1=(gethash name (level-groups level))
                          (let ((fresh (make-instance 'door-group)))
                            (register level fresh :doors)
                            (setf #1# fresh))))
               (door (make-instance 'door
                                    :group group
                                    :level level
                                    :row row
                                    :col col)))
          (vector-push-extend door (group-doors group))
          door))
       ((list :blocked-square name)
        (register level
                  (make-instance 'blocked-square :row row :col col :level level)
                  name))
       ((list* :trigger action)
        (make-instance
         'trigger
         :row row
         :col col
         :action (parse-action level row col action)))
       (x x)))))

;; (defclass active-square (square) ())

(defclass trigger (has-update-category has-position)
  ((action :initarg :action :accessor trigger-action))
  (:default-initargs :update-category :trigger))

(defclass spawn (has-position has-update-category)
  ((action :accessor action :initarg :action))
  (:default-initargs :update-category :start))

(ql:quickload :sdl2)

(defvar *renderer*)
(defvar *window*)

(define-condition end-of-game () ())

  ;; (call-with-sdl2-context 'test)

(defclass game (has-direction)
  ((keymap :initform (make-hash-table) :accessor keymap)
   (level :initform nil :accessor game-level)
   (squares :initform nil :accessor game-squares)
   (intents :initform (make-hash-table :test #'equal)
            :accessor game-intents)))

(defparameter *game* (make-instance 'game))

(progn
  (defun keybind (key &optional (game *game*))
    #1=(gethash (sdl2:scancode-key-to-value key)
                (keymap game)))

  (defun (setf keybind) (callback key &optional (game *game*))
    (setf #1# callback)))

(defun restart-main-loop ()
  (error "Debugger"))

(defun color-setter* (r g b a)
  (lambda ()
    (sdl2:set-render-draw-color *renderer* r g b a)))

(defun color-setter (color)
  (destructuring-bind (r g b a) color
    (color-setter* r g b a)))

;; (setf (keybind :scancode-f2) (let ((setter (color-setter* 1 0 0 1 )))
;;                                (lambda ()
;;                                  (print "Pressed F2")
;;                                  (funcall setter))))

;; (setf (keybind :scancode-escape)
;;       (lambda () (trigger-event *game* :restart)))

(defgeneric display (game)
  (:method ((g game))
    (sdl2:set-render-draw-color *renderer* 100 0 50 100)
    (sdl2:render-clear *renderer*)
    (display (game-level g))
    (sdl2:render-present *renderer*)))

(let ((out *standard-output*))
  (defun info (x)
    (prog1 x
      (print x out))))

(defun again (&rest args)
  (declare (ignore args))
  (invoke-restart 'again))

(define-condition restart-function () ())

(defun signaller (condition)
  (lambda ()
    (info `(:signaling ,condition))
    (signal condition)))

(defvar *gl*)
(ql:quickload :cl-opengl)

(defun call-with-sdl2-context (function)
  ;; helps with reloading code while the game runs
  (check-type function symbol)
  (catch 'sdl2-block
    (sdl2:with-everything (:window (*window* :w (* *size* 32) :h 800
                                             :title "Marching squares"
                                             :flags '(:shown :opengl))
                           :gl *gl*)
      (sdl2:with-renderer (*renderer* *window*)
        (sdl2:gl-make-current *window* *gl*)
        ;; (gl:shade-model :smooth)
        ;; (gl:enable :blend)
        ;; (gl:hint :perspective-correction-hint :nicest)
        (handler-bind ((restart-function #'again))
          (tagbody
           start
             (restart-case (funcall function)
               (again ()
                 :report "Restart main function"
                 (go start)))))))))

(defgeneric update (object)
  (:method (_)))

(setf (keybind :scancode-f1) (signaller 'restart-function))
;; (setf (keybind :scancode-escape)  #'sdl2:push-quit-event)

(setf (keybind :scancode-left) :go-left)
(setf (keybind :scancode-right) :go-right)
(setf (keybind :scancode-escape) :restart)

(defgeneric conclude-step (object)
  (:method (_)))

(defgeneric microstep (object ratio)
  (:method (_ r)))

(defmethod conclude-step ((game game))
  (dolist (square (game-squares game))
    (conclude-step square))
  (conclude-step (game-level game)))

(defmethod conclude-step ((level level))
  (dolist (group (gethash :doors (level-groups level)))
    (map () #'conclude-step (group-doors group))))

(defmethod conclude-step ((door door))
  (when (eq (state door) :open)
    (change-class door 'garbage)))

(defmethod trigger-event (x y))

(defvar *event* nil)
(defmethod trigger-event ((trigger trigger) event)
  (let ((*event* event))
    (funcall (trigger-action trigger))))

(defmethod conclude-step ((square square))
  (with-accessors ((next next-position)
                   (state state)
                   (col col)
                   (row row)
                   (level level)) square
    (when next
      (setf (aref (level-array level) row col) nil)
      (destructuring-bind (_ lev r c) next
        (declare (ignore _))
        (setf (row square) r)
        (setf (col square) c)
        (setf (level square) lev)
        (let ((old (aref (level-array level) row col)))
          (when old
            (trigger-event old square)))
        (setf (aref (level-array lev) r c) square))
      (setf (angle square) 0)
      (setf (offset-x square) 0)
      (setf (offset-y square) 0)
      (setf next nil))))

(defun test ()
  (let ((steps 20)
        (next-direction))
    (sdl2:with-event-loop (:method :poll)
      (:keydown (:keysym keysym)
                (let* ((binding (keybind (sdl2:scancode-value keysym)
                                         *game*))
                       (command (typecase binding
                                  (function (funcall binding))
                                  (symbol binding))))
                  (case command
                    (:go-left (setf next-direction :left))
                    (:go-right (setf next-direction :right))
                    (:restart (trigger-event *game* :restart)
                     (setf next-direction nil)))))
      (:idle ()
             (setf (direction *game*) next-direction
                   next-direction nil)
             (update *game*)
             (loop
               for s below steps
               do (display *game*)
                  ;; (gl:flush)
                  ;; (sdl2:gl-swap-window *window*)
                  (microstep *game* (/ s steps))
                  (sleep 0.005))
             (conclude-step *game*))
      (:quit () :quit))))

;; (call-with-sdl2-context 'test)

(declaim (notinline rect%))
(defun rect% (x y sx sy fill r g b a)
  (sdl2:set-render-draw-color *renderer* r g b a)
  (if fill
      (sdl2:render-fill-rect *renderer* (sdl2:make-rect x y sx sy))
      (sdl2:render-draw-rect *renderer* (sdl2:make-rect x y sx sy))))

(defgeneric display-element (element size x y)
  (:method (_e s x y)
    ;; (rect% x y s s nil 0 0 0 0)
    ))

(defmethod display-element ((trigger inverter) size x y)
  (gl:color 0.8 0.7 0 0)
  (let ((rand 9))
    (gl:rect (+ x rand)
             (+ y rand)
             (+ x size (- rand))
             (+ y size (- rand)))))

(defmethod display-element ((trigger trigger) size x y)
  )

(defmethod display-element ((element (eql :wall)) s x y)
  (gl:color 0 0 0 0)
  (gl:rect x y (+ x s) (+ y s))
  ;; (rect% x y s s t 0 0 0 0)
  )

(defmethod display-element ((square blocked-square) s x y)
  (with-accessors ((dx offset-x)
                   (dy offset-y)
                   (angle angle)) square
    (gl:with-pushed-matrix
      (gl:translate (+ dx x) (+ dy y *size*) 0)
      (gl:rotate angle 0 0 1)
      (gl:color 0 0 0 0)
      (gl:rect 0 3 s (- s))
      (gl:color 0.2 0.2 0.2 0)
      (gl:rect 1 -1 (- s 1) (- (- s 1)))
      (gl:color 0.8 0.8 0.8 0)
      (gl:rect 3 -3 (- s 3) (- (- s 3))))))

(defmethod display-element ((square square) s x y)
  (with-accessors ((dx offset-x)
                   (dy offset-y)
                   (angle angle)) square
    (gl:with-pushed-matrix
      (gl:translate (+ dx x) (+ dy y *size*) 0)
      (gl:rotate angle 0 0 1)
      (gl:color 0 0 0 0)
      (gl:rect 0 0 s (- s))
      (gl:color 0.2 0.2 0.2 0)
      (gl:rect 1 -1 (- s 1) (- (- s 1)))
      (gl:color 1. 1. 1. 0)
      (gl:rect 3 -3 (- s 3) (- (- s 3))))))

(defmethod display-element ((square inverted-square) s x y)
  (with-accessors ((dx offset-x)
                   (dy offset-y)
                   (angle angle)) square
    (gl:with-pushed-matrix
      (gl:translate (+ dx x) (+ dy y *size*) 0)
      (gl:rotate angle 0 0 1)
      (gl:color 0 0 0 0)
      (gl:rect 0 0 s (- s))
      (gl:color 0.2 0.2 0.2 0)
      (gl:rect 1 -1 (- s 1) (- (- s 1)))
      (gl:color 0.9 0.9 0.4 0)
      (gl:rect 4 -4 (- s 4) (- (- s 4))))))

(defun radian (degree)
  (* degree #.(float (/ pi 180) 0d0)))

(defmethod microstep ((game game) ratio)
  (dolist (s (game-squares game))
    (microstep s ratio))
  (microstep (game-level game) ratio))

(defmethod microstep ((level level) ratio)
  (dolist (group (gethash :doors (level-groups level)))
    (map ()
         (lambda (door) (microstep door ratio))
         (group-doors group))))

(defmethod microstep ((square square) ratio)
  (case (state square)
    (:falling
     (setf (angle square) 0)
     (setf (offset-y square) (* ratio *size*)))
    (:staying)
    (:lefting
     (setf (angle square) (* ratio -90)))

    (:righting
     (setf (angle square) (+ -90 (* ratio 90)))
     (setf (offset-x square) *size*)
     ;; (setf (offset-y square) *size*)
     )))

(defmethod microstep ((door door) ratio)
  (case (state door)
    (:closed (setf (openness door) 0))
    (:open (setf (openness door) ratio))))

(defmethod display-element ((door door) s x y)
  (flet ((draw (low high)
           (gl:color 0.0 0.0 0.0 1)
           (gl:rect low y high (+ y 5))
           (gl:color 1.0 1.0 1.0 1)
           (gl:rect  (+ 1 low) (+ y 2) (1- high) (+ y 3))))
    (with-accessors ((openness openness)) door
      (let ((ratio (* (- 1 openness) (/ s 2))))
        (draw x (round (+ x ratio)))
        (draw (round (+ x s (- ratio)))
              (+ x s))))))

(defmethod display ((level level))
  (let ((array (level-array level))
        (size *size*))
    (destructuring-bind (rows cols) (array-dimensions array)
      (dotimes (row rows)
        (dotimes (col cols)
          (let ((y (* row size))
                (x (* col size)))
            (display-element (aref array row col)
                             size x y)))))))

(defgeneric trigger-event (game event))

(defmethod trigger-event ((game game) (event (eql :init)))
  (setf (game-squares game) nil)
  (setf (direction game) nil)
  (clrhash (game-intents game))
  (with-accessors ((description level-source-description)
                   (bindings level-source-bindings)
                   (next level-next))
      (game-level game)
    (setf (game-level game)
          (make-level description bindings next))))

(defclass garbage ()  ())
(defun garbagep (x)
  (or (null x)
      (typep x 'garbage)))

(defmethod trigger-event ((game game) (event (eql :restart)))
  (trigger-event game :init)
  (setf #1=(gethash :start (level-categories (game-level game)))
        (delete-if #'garbagep
                   (mapcar (lambda (u) (trigger-event u :restart))
                           #1#))))

(defmethod trigger-event ((game game) (event (eql :next-level)))
  (let ((next (level-next (game-level game))))
    (cond
      (next
       (setf (game-level game) next)
       (trigger-event game :restart))
      (t (info "Congratulations, you win.")
         (sdl2:push-quit-event)))))

(defmethod trigger-event ((spawn spawn) event)
  (prog1 nil
    (funcall (action spawn))))

(defstruct wish intent square accept-fn)
(defstruct neighbours
  (left nil)
  (up-left nil)
  (up nil)
  (up-right nil)
  (right nil)
  (down nil))

(defun pick-candidate (candidates)
  (cond
    ((not (cdr candidates)) (first candidates))
    (t (or (find :fall candidates :key #'wish-intent)
           (alexandria:random-elt
            (loop
              for x in candidates
              if (typep (wish-square x) 'inverted-square)
                collect x into inv
              else
                collect x into norm
              finally (list inv norm)
                      (return (or norm inv))))))))



(defmethod update ((game game))
  (let ((direction (direction game)))
    (dolist (s (game-squares game))
      (setf (direction s) direction)))
  (clrhash (game-intents game))
  (loop
    for square in (game-squares game)
    for (intent target fn) = (update square)
    when intent
      do (push (make-wish :intent intent
                          :square square
                          :accept-fn fn)
               (gethash target (game-intents game))))
  (maphash (lambda (target candidates)
             (declare (ignore target))
             (let ((kept (pick-candidate candidates)))
               (when (wish-accept-fn kept)
                 (funcall (wish-accept-fn kept)))))
           (game-intents game))
  (update (game-level game)))

(defmethod update ((level level))
  (map () #'update (gethash :doors (level-groups level))))

(defgeneric pushed (button)
  (:method ((garbage garbage)) nil)
  (:method ((door door))
    (not (is-free (neighbours-up (neighbours door))))))

(defgeneric open-door (door)
  (:method ((door door))
    (setf (state door) :open)))

(defmethod update ((group door-group))
  (when (every #'pushed (group-doors group))
    (map () #'open-door (group-doors group)))
  (map () #'update (group-doors group)))

(defmethod update ((square blocked-square)))


(defgeneric animation-step (object)
  (:method (_)))

(defun neighbours (cell)
  (with-accessors ((row row) (col col) (level level)) cell
    (let ((array (level-array level))
          (n (make-neighbours)))
      (prog1 n
        (flet ((neighbour (slot delta-row delta-col)
                 (let ((row (+ row delta-row))
                       (col (+ col delta-col)))
                   (when (array-in-bounds-p array row col)
                     (setf (slot-value n slot)
                           (list (aref array row col) level row col))))))
          (progn (neighbour 'left    0 -1)
                 (neighbour 'up-left -1 -1)
                 (neighbour 'right    0 1)
                 (neighbour 'up-right -1 1)
                 (neighbour 'up       -1 0)
                 (neighbour 'down    1 0)))))))

(defgeneric is-free (cell)
  (:method (any) nil)
  (:method ((trigger trigger)) t)
  (:method ((garbage garbage)) t)
  (:method ((cons cons)) (or (null (car cons))
                             (is-free (car cons)))))

;; (defmethod animation-step ((game game))
;;   (loop with continue = nil
;;         for square in (game-squares game)
;;         do (setf continue (or (animation-step square) continue))
;;         finally (return continue)))

(defmethod update ((square square))
  (with-accessors ((next next-position)
                   (state state)) square
    (with-accessors ((left neighbours-left)
                     (right neighbours-right)
                     (up-left neighbours-up-left)
                     (up-right neighbours-up-right)
                     (up  neighbours-up)
                     (down neighbours-down))
        (neighbours square)
      (setf (state square) :staying)
      (flet ((left () (setf state :lefting
                            next left))
             (right () (setf state :righting
                             next right))
             (fall () (setf state :falling
                            next down)))
        (cond
          ((is-free down) `(:fall ,(rest down) ,#'fall))
          ((not (is-free up))
           '(:stay nil nil))
          ((not (direction square)) '(:stay nil nil))
          ((and (is-free left)
                (is-free up-left)
                (eql (direction square) :left))
           `(:walk ,(rest left) ,#'left))
          ((and (is-free right)
                (is-free up-right)
                (eql (direction square) :right))
           `(:walk ,(rest right) ,#'right)))))))

;; (gethash :start (level-categories (game-level *game*)))
;; (aref (level-array (game-level *game*)) 0 16)

;; (mapcar #'neighbours (game-squares *game*))

;; (trigger-event *game* :restart)

;; (defmethod update ((door door))
;;   (case (next-state door)
;;     (setf (state door) door)))

;; (declaim (notinline test))



(defparameter *level-1*
  (make-level #("                V               "
                "                                "
                "                                "
                "                                "
                "            # #######-#####     "
                "            ###                 "
                "                                "
                "                                "
                "###################             "
                "       #      ##B##  b          "
                "       #                        "
                "       #                 #      "
                "       ##### ####=###=####      "
                "           ###                  "
                "                     8          "
                "                                "
                "          # ###~########~#      "
                "          ###  e#  #            "
                "                   E            "
                "               #      ######    "
                "                    #      # ###"
                "              ###  ##      ###  "
                "                    ##          "
                "               ##               "
                "                # #   8         "
                "                # ##            "
                "             #######8           "
                "                                "
                "                     #          "
                "            #^###^#####^######  "
                "                                "
                "                                "
                "       #### ###:######## ###    "
                "       #       8      F#:#      "
                "       #                        "
                "       #  ####:       @         "
                "       #  #   f                 "
                "       ####                     "
                "                                "
                "                                "
                "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                )
              (list (cons #\b `(:trigger :release x))
                    (cons #\B `(:blocked-square x))
                    (cons #\e `(:trigger :release y))
                    (cons #\E `(:blocked-square y))
                    (cons #\f `(:trigger :release z))
                    (cons #\F `(:blocked-square z))
                    (cons #\X `(:trigger :lose))
                    (cons #\@ `(:trigger :win))
                    (cons #\8 :inverter)
                    (cons #\V :start)
                    (cons #\- '(:door door-1))
                    (cons #\= '(:door door-2))
                    (cons #\~ '(:door door-3))
                    (cons #\^ '(:door door-4))
                    (cons #\: '(:door door-5)))))

(defparameter *level-2*
  (make-level #("               V                "
                "                        V       "
                "                      V         "
                "              V                 "
                "                                "
                "           ##-#######-####      "
                "           ## ####### ####      "
                "      8###################8###  "
                "       ################### ###  "
                "       ################      #  "
                "       #                     #  "
                "       # #####################  "
                "                                "
                "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                )
              '((#\b . '(:trigger :release x))
                (#\B . '(:blocked-square x))
                (#\e . '(:trigger :release y))
                (#\E . '(:blocked-square y))
                (#\f . '(:trigger :release z))
                (#\F . '(:blocked-square z))
                (#\X . '(:trigger :lose))
                (#\@ . '(:trigger :win))
                (#\8 . :inverter)
                (#\V . :start)
                (#\- . '(:door door-1)))))

(defparameter *level-2*
  (make-level #("               V                "
                "                        V       "
                "                      V         "
                "              V                 "
                "                                "
                "    ######=## ############   #  "
                "    ###### ## ############   #  "
                "    ###### ## ############   #  "
                "    ###### ##=################  "
                "    ###### ##     #########B##  "
                "    ###### ##     ######        "
                "    ###E##    ###b#             "
                "      # ##    #                 "
                "      # ##### #  8              "
                "      #     #-#-##              "
                "      #        e                "
                "      #                    # #  "
                "      #^###^# ####^####^#### #  "
                "            # #            # #  "
                "            #^#   8        # #  "
                "                           ###  "
                "                                "
                "                                "
                "                                "
                "  #####~###~~~####~####~######  "
                "  #                          #  "
                "  #    @                     #  "
                "  ############################  "                      
                "  ############################  "
                "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
                )
              '((#\b . (:trigger :release x))
                (#\B . (:blocked-square x))
                (#\e . (:trigger :release y))
                (#\E . (:blocked-square y))
                (#\f . (:trigger :release z))
                (#\F . (:blocked-square z))
                (#\X . (:trigger :lose))
                (#\@ . (:trigger :win))
                (#\8 . :inverter)
                (#\V . :start)
                (#\- . (:door door-1))
                (#\= . (:door door-2))
                (#\^ . (:door door-3))
                (#\~ . (:door door-4)))))

(setf (level-next *level-1*)
      *level-2*
      (game-level *game*) *level-2*)

(defun start ()
  (setf (game-level *game*) *level-1*)
  (trigger-event *game* :restart)
  (call-with-sdl2-context 'test))

(start)

;; (car (last
;;       (mapcan (lambda (u) (coerce (group-doors u) 'list))
;;               (gethash :doors (level-groups (game-level *game*))))))
