(in-package :marching-squares)

(defclass rigid-group (and-group
                          active-object
                          has-name
                          global-trigger)
     ())

(defmethod (setf direction) (direction (group rigid-group))
  (dogroup (item group)
    (setf (direction item) direction)))

