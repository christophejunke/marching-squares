(in-package :marching-squares)

(defclass has-location ()
  ((location :reader location
             :writer (setf location%)
             :initarg :location)))

(defstruct (loc (:constructor loc% (level row col))
                (:conc-name))
  level row col)

(defclass has-location-cache ()
  ((loc-cache :accessor loc-cache
              :initarg :loc-cache
              :initform (make-hash-table :test #'equalp))))

(defgeneric loc (level row col)
  (:method (level row col) (loc% level row col)))

(defmethod loc ((level has-location-cache) row col)
  (let ((key (cons row col)))
    (or #1=(gethash key (loc-cache level))
        (setf #1# (call-next-method)))))

(defgeneric add-object-at-level-location (level row col object &key location))

(defun add-object-at-location%% (location object)
  (add-object-at-level-location (level location)
                                (row location)
                                (col location)
                                object
                                :location location))

(defun add-object-at-location% (object)
  (add-object-at-location%% (location object) object))

(defun leave-current-location% (object)
  (when (location object)
    (with-accessors ((level level)
                     (row row)
                     (col col))
        (location object)
      (let* ((array (level-array level))
             (value (aref array row col)))
        (when (and (consp value) (not (rest value)))
          (setf value (first value)))
        (cond
          ((null value))
          ((consp value) (removef (aref array row col) object))
          ((eq value object) (setf (aref array row col) nil))
          (t (error "Object ~a not found at location ~a"
                    object
                    (location object))))))))

(defmethod initialize-instance :after
    ((object has-location) &key &allow-other-keys)
  (when (and (slot-boundp object 'location)
             (location object))
    (when (allow-move-p object (location object))
      (add-object-at-location% object))))

(defgeneric (setf location) (location object)
  (:method :around (location (object has-location))
    (prog1 location
      (unless (equal location (location object))
        (leave-current-location% object)
        (call-next-method))))
  (:method (location (object has-location))
    (setf (location% object) location))
  (:method :after ((location loc) (object has-location))
    (when location
      (add-object-at-location% object)))
  (:method :after ((location (eql :trash)) (object has-location))
    (change-class object 'garbage)))

(defun objects-at (location)
  (ensure-list
   (aref (level-array (level location))
         (row location)
         (col location))))

(defmethod allow-move-p (mobile (location loc))
  (let ((target (objects-at location)))
    (or (null target)
        (allow-move-p mobile target))))

(defun location< (l1 l2)
  (or (< (row l1) (row l2))
      (and (= (row l1) (row l2))
           (< (col l1) (col l2)))))
