(in-package :marching-squares)

(defclass active-object () ())

(defclass has-active-objects ()
  ((active-objects :accessor active-objects :initform (make-group))))

(defmethod incorporate progn
    ((root has-active-objects) (object active-object))
  (group-add object (active-objects root)))

(defmethod reinitialize-instance :after
    ((object has-active-objects) &key &allow-other-keys)
  (group-clear (active-objects object)))

(defclass has-absolute-microstep ()
  ((last-tick :accessor last-tick :initform nil)))

(defgeneric delta-microstep (item dt)
  (:method (item dt)))

(defgeneric microstep (object ratio)
  (:method (_ r))
  (:method ((item has-absolute-microstep) ratio)
    (declare (ignore ratio))
    (let ((past (last-tick item))
          (now (get-internal-real-time)))
      (when past
        (setf (last-tick item) now)
        (delta-microstep item (- now past)))
      (setf (last-tick item) now)))
  (:method ((sequence sequence) ratio)
    (map () (rcurry #'microstep ratio) sequence)))
