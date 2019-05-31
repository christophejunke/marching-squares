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
  (:documentation "Incorporate an OBJECT into a PARENT object")
  (:method-combination progn)
  (:method progn (a b)))

(defgeneric extract-from (parent object)
  (:documentation "Extract incorporated OBJECT from PARENT")
  (:method-combination progn)
  (:method progn (a b)))

(defclass level-blueprint (has-dimensions has-name)
  ((grid :accessor grid :initarg :grid)
   (bindings :accessor bindings :initarg :bindings)
   (class :accessor level-class :initarg :class :initform 'level)
   (palettes :accessor blueprint-palettes :initarg :palettes :initform nil)
   (triggers :accessor blueprint-triggers :initarg :triggers :initform nil)
   (start-hook :accessor start-hook :initarg :on-start :initform nil))
  (:default-initargs :name "anonymous"))

