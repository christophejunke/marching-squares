(in-package :marching-squares)

(defun make-group-vector% (elements)
  (let ((size (length elements)))
    (if (plusp size)
        (make-array size
                    :initial-contents elements
                    :fill-pointer size
                    :adjustable t)
        (make-array 32 :fill-pointer 0 :adjustable t))))

(defclass group ()
  ((items :reader items
          :writer (setf items%)
          :initarg :items
          :initform (make-group-vector% nil))
   (combination :reader group-combination
                :writer (setf group-combination%)
                :initarg :combination
                :initform :and)))

(defmethod print-object ((group group) stream)
  (print-unreadable-object (group stream :type nil :identity nil)
    (format stream
            "~@<~2I~a ~@[[~a] ~]~@:_~S~:>"
           (class-name (class-of group))
            (and (typep group 'has-name)
                 (name group))
            (items group))))

(defclass named-group (has-name group) ())

(defun group-add (object group &aux (vec (items group)))
  (if (find object vec)
      (cerror "OK" "Object ~a already exists in group ~a" object group)
      (vector-push-extend object vec)))

(defun group-clear (group)
  (setf (items% group) (make-group-vector% nil)))

(defun group-purge (group)
  (setf (items% group)
        (delete-if #'garbagep (items group))))

(defun make-group (&optional items (combination :and))
  (make-instance 'group
                 :items (make-group-vector% items)
                 :combination (ecase combination
                                ((:and :or) combination))))

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

(defun group-p (item)
  (typep item 'group))

(defmethod update ((group group))
  (dogroup (item group)
    (update item)))

;; (defmethod update :before ((group group))
;;   (group-purge group))

(defmethod triggerable ((group group))
  (funcall (ecase (group-combination group)
             (:and #'every)
             (:or #'some))
           #'triggerable
           (items group)))

(defmethod trigger ((group group))
  (dogroup (item group)
    (trigger item)))

(defgeneric ensure-group (name class location &rest args)
  (:method (name class location &rest args)
    (let* ((level (level location))
           (group (first (resolve name level))))
      (unless group
        (setf group (apply #'make-instance
                           class
                           :name name
                           :allow-other-keys t
                           args))
        (incorporate location group))
      group)))

(defmethod initialize-instance :after ((object has-group)
                                       &key
                                         group-class
                                         combination
                                         name
                                         action
                                         location &allow-other-keys)
  (let ((group (ensure-group name
                             group-class
                             location
                             :action action
                             :combination combination)))
    (setf (group object) group)
    (group-add object group)))
