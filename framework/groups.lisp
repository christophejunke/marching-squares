(in-package :marching-squares)

(defclass group ()
  ((items :reader items
          :writer (setf items%)
          :initarg :items
          :initform (make-group-vector% nil))))

(defclass or-group (group) ())
(defclass and-group (group trigger) ())

(defclass named-group (has-name group) ())
(defclass named-and-group (and-group named-group) ())

(defun make-group-vector% (elements)
  (let ((size (length elements)))
    (if (plusp size)
        (make-array size
                    :initial-contents elements
                    :fill-pointer size
                    :adjustable t)
        (make-array 32 :fill-pointer 0 :adjustable t))))

(defun group-add (object group &aux (vec (items group)))
  (unless (find object vec)
    (vector-push-extend object vec)))

(defun group-clear (group)
  (setf (items% group) (make-group-vector% nil)))

(defun group-purge (group)
  (setf (items% group)
        (delete-if #'garbagep (items group))))

(defun make-group (&optional items combination)
  (let ((class (find-class
                (case combination
                  (:and 'and-group)
                  (:or 'or-group)
                  ((nil) 'group)
                  (t combination)))))
    (make-instance class :items (make-group-vector% items))))

(defmacro unless-garbagep (item &body body)
  `(unless (garbagep ,item)
     ,@body))

(defmacro with-garbage-handler (group-place &body body)
  (with-gensyms (some-garbage item inner-body)
    `(let (,some-garbage)
       (declare (type boolean ,some-garbage)
                (dynamic-extent ,some-garbage))
       (macrolet ((unless-garbagep (,item &body ,inner-body)
                    `(if (garbagep ,,item)
                         (setf ,',some-garbage t)
                         (progn ,@,inner-body))))
         (unwind-protect (progn ,@body)
           (when ,some-garbage
             (group-purge ,group-place)))))))

(defmacro dogroup ((var group &optional result) &body body)
  (with-gensyms (max idx vec)
    `(with-garbage-handler ,group
       (do* (,var
             (,vec (items ,group))
             (,max (length ,vec))
             (,idx 0 (1+ ,idx)))
            ((>= ,idx ,max) ,result)
         (setf ,var (aref ,vec ,idx))
         (unless-garbagep ,var
           ,@body)))))

(defun named-and-group (name)
  (make-instance 'named-and-group :name name))

(defun group-p (item)
  (typep item 'group))

(defmethod update ((group group))
  (update (items group)))

(defmethod update :before ((group group))
  (group-purge group))

(defmethod triggerable ((group and-group))
  "All must be triggerable before we call TRIGGER"
  (every #'triggerable (items group)))

(defmethod trigger ((group group))
  (dogroup (item group)
    (trigger item)))

(defgeneric ensure-group (name class location &rest args)
  (:method (name class location &rest args)
    (let* ((level (level location))
           (group (first (resolve name level))))
      (unless group
        (setf group (apply #'make-instance
                           (or class 'and-group)
                           :name name
                           :allow-other-keys t
                           args))
        (incorporate location group))
      group)))


