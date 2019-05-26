(in-package :marching-squares)

(defclass blockable ()
  ((blockedp :initform nil :accessor blockedp :initarg :blockedp)))

;; extract

(defclass abstract-square (has-invertible-direction
                           has-name
                           has-angle
                           blockable
                           mobile
                           solid
                           transformable)
  ((pivot-x :initform 0 :accessor pivot-x)
   (pivot-y :initform 0 :accessor pivot-y)
   (offset-y :initform 0 :accessor offset-y)
   (state :accessor state :initform :staying))
  (:default-initargs :name 'square))

(defclass square (abstract-square) ())

(defun squarep (object)
  (typep object 'square))

(defmethod is-pressed-by ((button button) (square abstract-square))
  (member (state square) '(:staying :falling)))

(defun activate-square (location &optional (square nil))
  (unless (some #'squarep (objects-at location))
    (incorporate location
                 (or square (make-instance 'square
                                           :location location)))))

(defmethod release ((level level) (square square))
  (setf (blockedp square) nil)
  (activate-square (location square) square))

(defmethod invert ((square square))
  (setf (slot-value square 'direction)
        (case (direction square)
          (:left :right)
          (:right :left)))
  (call-next-method))

;; TODO origin at square center (simplifies)
(defmethod transform-model-view ((square abstract-square))
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

(defmethod microstep ((square abstract-square) ratio)
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
     (colrect :wall 0 0 1 1.1)
     (colrect :blocked-square 0.1 0.1 0.9 0.9))
    ((invertedp square)
     (color :wall)
     (gl:rect 0 0 1 1)
     (color :square)
     (gl:rect 0.10 0.10 0.9 0.9)
     ;; (color :wall)
     ;; (gl:rect 0.15 0.15 0.85 0.85)
     (color :inverted-square)
     (gl:rect 0.30 0.30 0.70 0.70))
    (t
     (color :wall)
     (gl:rect 0 0 1 1)
     (color :square)
     (gl:rect 0.1 0.1 0.9 0.9))))

;; debug
;; (defmethod display :after ((square square))
;;   (when (invertedp square)
;;     (gl:color 1 0 0 1)
;;     (gl:rect 0 0 0.3 0.3))
;;   (case (state square)
;;     (:lefting (gl:color 0 0 1 1))
;;     (:righting (gl:color 0 1 0 1))
;;     (:staying (gl:color 1 1 0 1)))
;;   (gl:rect 0.7 0 1.0 0.3))

(defmethod authorize-move ((square abstract-square) move-type target)
  (multiple-value-bind (state delta)
      (case move-type
        (:fall  (values :falling  +0))
        (:right (values :righting +90))
        (:left  (values :lefting  -90)))
    (setf (next-move square)
          (list target (+ (angle square) delta)))
    (setf (state square) state)))

(defmethod update ((square abstract-square))
  (when (next-move square)
    (destructuring-bind (target angle) (next-move square)
      (setf (next-move square) nil)
      (setf (location square) target)
      (setf (angle square) angle)
      (setf (state square) :staying)
      (setf (offset-y square) 0)
      (setf (angle-offset square) 0))))

