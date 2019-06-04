(in-package :marching-squares)

(defclass namespace ()
  ((index :accessor index
          :initform (make-hash-table :test #'equalp))))

(defclass has-name ()
  ((name :reader name :initarg :name)))

(defmethod incorporate progn ((namespace namespace)
                              (named has-name))
  (pushnew named (gethash (name named) (index namespace))))

(defmethod extract-from progn ((namespace namespace)
                               (named has-name))
  (let ((key (name named))
        (table (index namespace)))
    (deletef (gethash key table) named)
    (unless (gethash key table)
      (remhash key table))))

(defgeneric resolve (name namespace)
  (:method (name (namespace namespace))
    (gethash name (index namespace))))

(defmethod reinitialize-instance :after ((object namespace)
                                         &key &allow-other-keys)
  (clrhash (index object)))

(defmethod print-object ((object has-name) stream)
  (let ((as-string
          (with-output-to-string (out)
            (call-next-method object out))))
    (princ (ppcre:regex-replace '(:sequence :start-anchor #\# #\<)
                                as-string
                                (format nil "#<[~a] " (name object)))
           stream)))
