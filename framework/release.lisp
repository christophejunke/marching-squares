(in-package :marching-squares)

(defgeneric release (context object)
  (:method ((namespace namespace) (name symbol))
    (map ()
         (curry #'release namespace)
         (resolve name namespace))))
