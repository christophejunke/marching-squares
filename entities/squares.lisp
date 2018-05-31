(in-package :marching-squares)

(defclass blockable ()
  ((blockedp :initform nil :accessor blockedp :initarg :blockedp)))

;; extract

(defclass square (has-invertible-direction
                  has-name
                  has-angle
                  blockable
                  mobile
                  transformable)
  ((pivot-x :initform 0 :accessor pivot-x)
   (pivot-y :initform 0 :accessor pivot-y)
   (offset-y :initform 0 :accessor offset-y)
   (state :accessor state :initform nil))
  (:default-initargs :name 'square))

(defun activate-square (location &optional (square nil sp))
  (unless (and (not sp) (some #'squarep (objects-at location)))
    (incorporate location
                 (or square (make-instance 'square :location location)))))

(defmethod release ((level level) (square square))
  (setf (blockedp square) nil)
  (activate-square (location square) square))

(defmethod invert ((invertible square))
  (call-next-method)
  (case (state invertible)
    ((:lefting :righting)
     (setf (state invertible) :staying
           (next-move invertible) nil))))

(defun squarep (object)
  (typep object 'square))

;; TODO origin at square center (simplifies)
(defmethod transform-model-view ((square square))
  ;; translate to pivot point (and add Y offset)
  (gl:translate (float (pivot-x square))
                (float (+ (offset-y square)
                          (pivot-y square)))
                0)
  ;; rotate around pivot according to angle-offset (pivot)
  (gl:rotate (angle-offset square) 0 0 1)

  ;; go back to top left corner, then to center
  (gl:translate (float (+ (- (pivot-x square)) 1/2))
                (float (+ (- (pivot-y square)) 1/2))
                0)
  ;; rotate around center according to angle
  (gl:rotate (angle square) 0 0 1)
  ;; go back to top left corner
  (gl:translate -0.5 -0.5 0))

(defmethod microstep ((square square) ratio)
  (with-accessors ((da angle-offset)
                   (px pivot-x)
                   (py pivot-y)
                   (dy offset-y))
      square
    (case (state square)
      (:falling
       (setf da 0 py 0 px 0 dy ratio))
      (:staying (setf da 0 px 0 py 0 dy 0))
      (:lefting
       (setf da (* ratio -90)
             px 0
             py 1
             dy 0))
      (:righting
       (setf da (* ratio 90)
             px 1
             py 1
             dy 0)))))

(defmethod display ((square square))
  (cond
    ((blockedp square)
     (colrect #'palette-wall
              0 0 1 1.1)
     (colrect #'palette-blocked-square
              0.1 0.1 0.9 0.9))
    ((invertedp square)
     (set-color #'palette-wall)
     (gl:rect 0 0 1 1)
     (set-color #'palette-square)
     (gl:rect 0.1 0.1 0.9 0.9)
     (set-color #'palette-wall)
     (gl:rect 0.15 0.15 0.85 0.85)
     (set-color #'palette-inverted-square)
     (gl:rect 0.25 0.25 0.75 0.75))
    (t
     (set-color #'palette-wall)
     (gl:rect 0 0 1 1)
     (set-color #'palette-square)
     (gl:rect 0.1 0.1 0.9 0.9))))

(defmethod authorize-move ((square square) move-type target)
  (multiple-value-bind (state delta)
      (case move-type
        (:fall  (values :falling  +0))
        (:right (values :righting +90))
        (:left  (values :lefting  -90)))
    (setf (next-move square)
          (list target (+ (angle square) delta)))
    (setf (state square) state)))

(defmethod update ((square square))
  (when (next-move square)
    (destructuring-bind (target angle) (next-move square)
      (setf (next-move square) nil)
      (setf (location square) target)
      (setf (angle square) angle)
      (setf (state square) :staying)
      (setf (offset-y square) 0)
      (setf (angle-offset square) 0))))
