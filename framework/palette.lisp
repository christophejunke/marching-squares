(in-package :marching-squares)

(defparameter *palette* nil)

(defmacro with-palette ((palette) &body body)
  `(let ((*palette* ,palette))
     ,@body))

(use-color :background)

(apply #'gl:color (palette-get :background *palette*))




