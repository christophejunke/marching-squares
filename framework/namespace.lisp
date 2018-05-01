(in-package :marching-squares)

(defclass namespace ()
  ((index :accessor index
          :initform (make-hash-table :test #'eq))))

(defclass has-name ()
  ((name :reader name :initarg :name)))

(defmethod incorporate progn ((namespace namespace)
                              (named has-name))
  (pushnew named (gethash (name named) (index namespace))))

(defgeneric resolve (name namespace)
  (:method (name (namespace namespace))
    (gethash name (index namespace))))

(defmethod reinitialize-instance :after
    ((object namespace) &key &allow-other-keys)
  (clrhash (index object)))
