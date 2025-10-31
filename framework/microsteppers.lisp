(in-package :marching-squares)

(defclass has-microsteppers ()
  ((microsteppers :initform (make-group) :accessor microsteppers)))

(defclass global-microstepper (invisible) ())

(defmethod incorporate progn
    ((root has-microsteppers)
     (o global-microstepper))
  (group-add o (microsteppers root)))

(defmethod microstep ((o has-microsteppers) ratio)
  (call-next-method)
  (dogroup (s (microsteppers o))
    (microstep s ratio)))
