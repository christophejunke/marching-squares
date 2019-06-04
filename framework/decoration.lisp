(in-package :marching-squares)

(defclass has-decoration ()
  ((decoration :accessor decoration
               :initarg :decoration
               :initform nil)))

(defmethod display :after ((decorated has-decoration))
  (display (decoration decorated)))

