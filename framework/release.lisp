(in-package :marching-squares)

(defgeneric release (context object)
  (:method ((namespace namespace) (_ garbage)))
  (:method ((namespace namespace) (name symbol))
    (map ()
         (curry #'release namespace)
         (resolve name namespace))))

