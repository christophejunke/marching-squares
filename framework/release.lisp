(in-package :marching-squares)

(defgeneric release (context object)
  (:method ((namespace namespace) (_ garbage)))
  (:method ((namespace namespace) (name symbol))
    (map ()
         (curry #'release namespace)
         (resolve name namespace))))

(defclass release-group (named-group
                         global-trigger
                         oneshot)
  ())

(defclass releaser (has-location
                    has-absolute-microstep
                    has-group
                    triggerable-by-predicate
                    transformable)
  ((alpha :accessor alpha :initform 0)
   (counter :accessor counter :initform 0)
   (target :accessor target :initarg :target)))

(defmethod trigger ((releaser releaser))
  (let ((level (level (location releaser)))
        (target (target releaser)))
    (release level target)))

(defmethod transform-model-view ((trigger releaser))
  (gl:translate 0.5 0.5 0.5)
  (gl:rotate (* (counter trigger) #.(/ 180 pi)) 0 0 1))

(defmethod display ((trigger releaser))
  (color `(:alpha ,(alpha trigger) :foreground))
  (csq 0.1))

(defmethod delta-microstep ((trigger releaser) delta)
  (setf (counter trigger)
        (mod (+ (counter trigger)
                (/ delta internal-time-units-per-second 1/5))
             #.(* 2 pi)))
  (setf (alpha trigger)
        (- 1 (abs (/ (sin (/ (counter trigger) 3)) 1.9)))))
