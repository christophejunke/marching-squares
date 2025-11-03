(in-package :marching-squares)

(defclass has-microsteppers ()
  ((microsteppers :initform (make-group) :accessor microsteppers)))

(defclass global-microstepper (invisible) ())

(defmethod restart-level :after ((o has-microsteppers))
  (group-clear (microsteppers o)))

(defmethod incorporate progn
    ((root has-microsteppers)
     (o global-microstepper))
  (group-add o (microsteppers root)))

(defmethod extract-from progn
    ((root has-microsteppers)
     (o global-microstepper))
  (group-remove o (microsteppers root)))

(defmethod microstep ((o has-microsteppers) ratio)
  (dogroup (s (microsteppers o))
    (microstep s ratio))
  (call-next-method))
