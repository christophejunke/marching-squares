(in-package :marching-squares)

(defclass has-dimensions ()
  ((width :accessor width :initarg :width)
   (height :accessor height :initarg :height)))

(defmethod initialize-instance :after ((object has-dimensions)
                                       &key dimensions &allow-other-keys)
  (when dimensions
    (destructuring-bind (height width) dimensions
      (setf (width object) width
            (height object) height))))

(defgeneric build (blueprint parent)
  (:documentation "Build an object from a blueprint and a context"))

(defgeneric incorporate (parent object)
  (:documentation "Incorporate a built object into a parent")
  (:method-combination progn)
  (:method progn (a b)))

(defclass level-blueprint (has-dimensions)
  ((grid :accessor grid :initarg :grid)
   (bindings :accessor bindings :initarg :bindings)))

