(in-package :marching-squares)

(defclass button (solid
                  has-location
                  has-group)
  ((pressed-by :initform nil
               :reader pressedp
               :accessor pressed-by)))

(defmethod update ((button button))
  (call-next-method)
  (destructuring-bind (north) (neighbours button :n)
    (setf (pressed-by button)
          (find-if #'squarep (objects-at north)))))

(defmethod triggerable ((button button))
  (pressed-by button))

(defclass press-button (button) ())

(defmethod display ((button button))
  (colrect #'palette-wall 0 0 1 1)
  (colrect (if (pressedp button)
               #'palette-flash/feedback
               #'palette-blocked-square)
           0.1 (if (pressedp button) 0.2 0.1)
           0.9 0.3)
  (when (group button)
    (cond
      ((triggerable (group button))
       (gl:color 1 1 0 0.9)
       
       ;; (loop for y from 1/2 upto 4/5 by 1/10
       ;;       do (gl:rect 1/4 y 3/4 (+ y 1/20)))
       (gl:rect 0.25 0.55 0.75 0.75)
       )
      (t 
       (gl:color 1 1 0 0.2)
       (gl:rect 0.25 0.55 0.75 0.75)
       ))))

(defmethod display ((button press-button))
  (call-next-method)
  (when (and (group button) (firedp (group button)))
    (gl:color 0 0 0 1)
    (gl:rect 0.1 0.5 0.9 0.9)
    (gl:color 1 1 0 0.9)
    (gl:rect 0.25 0.55 0.75 0.75)))

(defclass button-group (active-object
                        has-name
                        invisible
                        lambda-trigger)
  ())

(defclass press-button-group (latch and-button-group)
  ())

(defmethod update ((group press-button-group))
  (call-next-method)
  (dogroup (button group)
    (unless (pressedp button)
      (setf (firedp group) nil)
      (return))))

(defun make-button (name location action &key latchp (combine :and))
  (make-instance (if latchp 'press-button 'button)
                 :name name
                 :group-class (if latchp
                                  'press-button-group
                                  'button-group)
                 :combination combine
                 :location location
                 :action action))

(defmethod trigger :after ((group button-group))
  (if (action group)
      (funcall (action group))
      (call-next-method)))

