(in-package :marching-squares)

;;;; DIRECTION and INVERTIBLE DIRECTION

(defclass has-direction nil
  ((direction :accessor direction :initarg :direction)))

(defclass has-invertible-direction (has-direction)
  ((invertedp :initform nil :accessor invertedp :initarg :inverted)))

(defmethod (setf direction)
    (direction (invertible has-invertible-direction))
  (if (invertedp invertible)
      (call-next-method (case direction
                          (:left :right)
                          (:right :left)
                          (t direction))
                        invertible)
      (call-next-method)))

(defun invertiblep (object)
  (typep object 'has-invertible-direction))

(defgeneric invert (invertible)
  (:method (_))
  (:method ((invertible has-invertible-direction))
    (setf (invertedp invertible)
          (not (invertedp invertible)))))

;;;; ANGLES

(defclass has-angle ()
  ((angle :initarg :angle :accessor angle :initform 0)
   (angle-offset :accessor angle-offset :initform 0)))

;;;; RANK

(defclass has-rank ()
  ((rank :initarg :rank :accessor rank)))

;;;; PALETTE

(defclass has-palette ()
  ((palette :accessor palette :initarg :palette)))

(defstruct (rgba (:constructor rgba (r g b &optional (a 1)))) r g b a)

(defgeneric pick-color (palette color &key &allow-other-keys)
  (:method ((palette null) (color null) &key r g b a &allow-other-keys)
    (rgba r g b a)))

(defun gl-pick-color (palette color &rest args)
  (with-accessors ((r rgba-r)
                   (g rgba-g)
                   (b rgba-b)
                   (a rgba-a))
      (apply #'pick-color palette color args)
    (gl:color r g b a)))

(defclass has-group ()
  ((group :accessor group)))

