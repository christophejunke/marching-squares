(in-package :marching-squares)

(defclass transformable () ())

(defgeneric transform-model-view (transformable)
  (:method (_)))

(defmacro with-location-transform ((&key
                                      (push t)
                                      (row nil rp)
                                      (col nil cp)
                                      (location nil lp)) &body body)
  (assert (and (eql rp cp)
               (eql cp (not lp)))
          ()
          "This macro expects either both :ROW and :COL arguments,~
            or only a :LOCATION argument.")
  (flet ((row-col-code (row col)
           (if push
               `(gl:with-pushed-matrix* (:modelview)
                  . #1=((gl:translate ,col ,row 0) ,@body))
               `(progn . #1#))))
    (if (and rp cp)
        (once-only (row col) (row-col-code row col))
        (once-only (location) (row-col-code `(row ,location)
                                            `(col ,location))))))

(defmethod display :around ((this transformable))
    (gl:with-pushed-matrix* (:modelview)
      (transform-model-view this)
      (call-next-method)))

