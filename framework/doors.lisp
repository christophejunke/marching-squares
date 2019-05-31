(in-package :marching-squares)

(deftype door-state ()
  '(member :close :open :closing :opening))

(deftype openness ()
  '(real 0 1))

(defmethod initialize-instance :after
    ((object has-group) &key group &allow-other-keys)
  (when group
    (group-add object group)))

(defclass door (has-location
                has-group)
  ((state :accessor state
          :type symbol
          :initarg :state
          :initform :close)
   (openness :accessor openness
             :type openness
             :initform 0)))

(defclass door-group (group
                      invisible
                      active-object
                      has-name)
  ())

(defclass press-door (door button) ())
(defclass press-door-group (door-group global-trigger) ())

(defgeneric door-blocked-open-p (door)
  (:method ((door door))
    (find-if #'solidp
             (remove door (objects-at (location door))))))

(defun make-door (name &key location pressp (state :close) (combine :and))
  (make-instance (if pressp 'press-door 'door)
                 :group-class (if pressp 'press-door-group 'door-group)
                 :combination combine
                 :group-name name
                 :location location
                 :state state))

(defmethod trigger ((door door))
  (setf (state door)
        (case (state door)
          (:open :open)
          (:closing :open)
          (t :opening))))

(defmethod post-move-update ((door press-door))
  (when (and (eq (state door) :open)
             (not (triggerable door)))
    (setf (state door) :closing)))

(defmethod update ((door door))
  (call-next-method)
  (setf (state door)
        (case (state door)
          (:opening :open)
          (:open (if (door-blocked-open-p door)
                     :open
                     :closing))
          (:closing :close)
          (t (state door)))))

(defmethod triggerable ((door press-door))
  (or (pressedp door)
      (door-blocked-open-p door)))

(declaim (inline openness-ratio))
(defun openness-ratio (openness)
  (float (/ (- 1 openness) 2)))

(defun draw-door (dx y height ratio)
  (let ((min-x dx)
        (max-x (max dx (- ratio dx)))
        (min-y y)
        (max-y (+ y height)))
    (gl:rect min-x min-y max-x max-y)
    (gl:rect (- 1 min-x) min-y (- 1 max-x) max-y)))

(defmethod display ((door press-door)))
(let ((ratio (openness-ratio (openness door))))
  (prog1 ratio
    (color :wall)
    (draw-door 0 0 6/20 ratio)
    (color (if (pressedp door)
               :flash/feedback
               :foreground))
    (draw-door 5/100 0 3/20 ratio)))

(defmethod display ((door door))
  (let ((ratio (openness-ratio (openness door))))
    (color :wall)
    (draw-door 0 0.0 0.45 ratio)
    (color :door)
    (draw-door 0.15 0.15 0.10 ratio)))

(defmethod microstep ((door door) ratio)
  (setf (openness door)
        (case (state door)
          (:open 1)
          (:close 0)
          (:opening ratio)
          (:closing (- 1 ratio)))))

;; The :opening case is deliberately left out.
(defmethod allow-move-p (object (door door))
  (eq (state door) :open))
