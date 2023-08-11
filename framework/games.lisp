(in-package :marching-squares)

(defclass game (has-direction
                move-arbiter
                has-active-objects
                has-triggers
                has-keymap
                namespace
                has-dimensions)
  ((title :initform "Unnamed"
          :accessor title
          :initarg :title)
   (level :initform nil :accessor game-level)
   (level-blueprint :initarg :level-blueprint :accessor level-blueprint)
   (sleep-delay :initarg :sleep-delay
                :accessor sleep-delay
                :initform 0.1)
   (microsteps :initarg :microsteps
               :accessor microsteps
               :initform 30)
   (microsteps-duration :initarg :microsteps-duration
                        :accessor microsteps-duration
                        :initform 0.10)))

(defgeneric restart-level (level))

(defgeneric loose (level)
  (:method (_) (restart-game-loop)))

(defgeneric win (level)
  (:method ((game game))
    (when-let (fn (on-winning (ensure-blueprint (level-blueprint game))))
      (funcall fn game)))
  (:method ((level level))
    (win (game level))))

(defun next (level)
  (lambda (game)
    (setf (level-blueprint game) level)
    (restart-game-loop)))

(defmethod microstep ((game game) ratio)
  (microstep (game-level game) ratio))

