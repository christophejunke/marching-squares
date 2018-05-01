(in-package :marching-squares)

(deftype door-state ()
  '(member :close :open :waiting :closing :opening))

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

(defclass door-group (invisible
                      and-group
                      active-object
                      has-name)
  ())

(defclass press-door (door button) ())

(defclass press-door-group (door-group
                            global-trigger)
  ())

(defun make-door (name &key location pressp)
  (make-instance (if pressp 'press-door 'door)
                 :group-class (if pressp 'press-door-group 'door-group)
                 :name name
                 :location location))

(defmethod trigger ((door door))
  (setf (state door)
        (case (state door)
          (:open :open)
          (:waiting :open)
          (t :opening))))

(defmethod update ((door door))
  (call-next-method)
  (setf (state door)
        (case (state door)
          (:opening :open)
          (:closing :close)
          (:open :waiting)
          (:waiting :closing)
          (t (state door)))))

(defmethod triggerable ((door press-door))
  (or (pressedp door) (call-next-method)))

(defmethod update ((door press-door))
  (case (state door)
    (:waiting (if (pressedp door)
                  (setf (state door) :open)
                  (call-next-method)))
    (t (call-next-method))))

(declaim (inline openness-ratio))
(defun openness-ratio (openness)
  (float (/ (- 1 openness) 2)))

(defun draw (dx y height ratio)
  (let ((min-x dx)
        (max-x (max dx (- ratio dx)))
        (min-y y)
        (max-y (+ y height)))
    (gl:rect min-x min-y max-x max-y)
    (gl:rect (- 1 min-x) min-y (- 1 max-x) max-y)))

(defmethod display ((door press-door))
  (let ((ratio (openness-ratio (openness door))))
    (prog1 ratio
      (set-color #'palette-wall)
      ;; (draw 0 0 0.3)
      (draw 0 0 0.3 ratio)
      (set-color (if (pressedp door)
                 #'palette-flash/feedback
                 #'palette-foreground))
      ;; (draw  0.05 0.1 0.05)
      (draw  0.05 0 0.15 ratio))))

(defmethod display ((door door))
  (let ((ratio (openness-ratio (openness door))))
    (gl:color 0 0 0 1)
    (draw 0 0.0 0.25 ratio)
    (set-color #'palette-foreground)
    (draw 0.15 0.05 0.15 ratio)))

(defmethod microstep ((door door) ratio)
  (setf (openness door)
        (case (state door)
          ((:open :waiting) 1)
          (:close 0)
          (:opening ratio)
          (:closing (- 1 ratio)))))

(defmethod allow-move-p (object (door door))
  (member (state door) '(:open :waiting)))
