(in-package :marching-squares)

(defclass has-decoration ()
  ((decoration :accessor decoration
               :initarg :decoration
               :initform nil)))

(defmethod display :after ((decorated has-decoration))
  (display (decoration decorated)))

#+test
(defmethod display ((decoration (eql :test-decoration)))
  (gl:color 0 0 0 1)
  (gl:rect 0.1 0.1 0.3 0.3))

