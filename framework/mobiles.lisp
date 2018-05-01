(in-package :marching-squares)

(defclass mobile (has-location has-layer)
  ((next-move :accessor next-move :initform nil))
  (:default-initargs :layer :mobiles))

(defgeneric compute-next-move (mobile))
(defgeneric authorize-move (mobile move-type location))

(defun move-object (intent) (car intent))
(defun move-type (intent) (cdr intent))

(defclass move-arbiter ()
  ((%inside-arbiter-p :accessor inside-arbiter-p :initform nil)
   (move-intents :initform (make-hash-table :test #'equal)
                 :reader move-intents)
   (mobiles :initform (make-group)
             :reader mobiles
             :writer (setf mobiles%))))

(defmethod incorporate progn ((arbiter move-arbiter) (object mobile))
  (group-add object (mobiles arbiter)))

(defmethod reinitialize-instance :after
    ((object move-arbiter) &key &allow-other-keys)
  (group-clear (mobiles object)))

(defgeneric arbiter-compare (arbiter location first second))

(defgeneric pick-winner (arbiter candidate-moves location)
  (:method (arbiter candidate-moves location)
    "Default"
    (cond
      ((not (cdr candidate-moves))
       (first candidate-moves))
      (t (flet ((compare (x y) (arbiter-compare arbiter location x y)))
           (let* ((sorted (sort candidate-moves #'compare))
                  (head (first sorted))
                  (end (position-if (lambda (u) (compare head u)) sorted)))
             (when end
               (setf (cdr (nthcdr (1- end) sorted)) nil))
             (random-elt sorted)))))))

(defgeneric add-move-intent (arbiter mobile move-type location)
  (:method (arbiter (mobile mobile) move-type location)
    (when move-type
      (push (cons mobile move-type)
            (gethash location (move-intents arbiter))))))

(defgeneric gather-intent (arbiter mobile)
  (:method ((arbiter move-arbiter) (mobile mobile))
    (multiple-value-bind (move-type location)
        (compute-next-move mobile)
      (add-move-intent arbiter mobile move-type location))))

(defgeneric arbiter-intents (arbiter location intents)
  (:method ((arbiter move-arbiter) location (candidates sequence))
    (let ((winner (pick-winner arbiter candidates location)))
      (destructuring-bind (object . move-type) winner
        (authorize-move object move-type location)))))

(defmacro within-arbiter-context (arbiter &body body)
  (once-only (arbiter)
    `(progn
       (clrhash (move-intents ,arbiter))
       (setf (inside-arbiter-p ,arbiter) t)
       (unwind-protect (progn ,@body)
         (clrhash (move-intents ,arbiter))
         (setf (inside-arbiter-p ,arbiter) nil)))))

(defgeneric arbiter-moves (arbiter mobiles)
  (:method ((arbiter move-arbiter) (group group))
    (dogroup (mobile group)
      (gather-intent arbiter mobile)))
  (:method :around ((arbiter move-arbiter) object)
    (if (inside-arbiter-p arbiter)
        (call-next-method)
        (within-arbiter-context arbiter
          (call-next-method)
          (maphash (lambda (location candidates)
                     (arbiter-intents arbiter location candidates))
                   (move-intents arbiter)))))
  ;; (:method ((arbiter move-arbiter) (groups sequence))
  ;;   (dolist (group groups)
  ;;     (dogroup (mobile group)
  ;;       (gather-intent arbiter mobile))))
  (:method ((arbiter move-arbiter) (groups sequence))
    (dolist (group groups)
      (dogroup (mobile group)
        (gather-intent arbiter mobile)))))

