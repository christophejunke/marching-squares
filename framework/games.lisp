(in-package :marching-squares)

(defclass game (has-direction
                has-active-objects
                has-triggers
                has-keymap
                move-arbiter
                namespace
                has-dimensions)
  ((title :initform "Unnamed" :accessor title :initarg :title)
   (level :initform nil :accessor game-level)
   (level-blueprint :initarg :level-blueprint :accessor level-blueprint)
   (sleep-delay :initarg :sleep-delay
                :accessor sleep-delay
                :initform 0.1)
   (microsteps :initarg :microsteps
               :accessor microsteps
               :initform 15)
   (microsteps-duration :initarg :microsteps-duration
                        :accessor microsteps-duration
                        :initform 0.15)))

(defgeneric restart-level (level))

(defgeneric loose (level)
  (:method (_) (restart-game-loop)))

(defgeneric win (level)
  (:method (level)))

(defmethod microstep ((game game) ratio)
  (microstep (game-level game) ratio))
