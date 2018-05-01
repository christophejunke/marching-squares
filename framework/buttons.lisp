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
       (gl:color 0 1 0 0.9)
       (loop for y from 0.50 upto 0.8 by 1/10
             do (gl:rect 0.25 y 0.75 (+ y 1/20))))
      (t 
       (gl:color 1 0 0 0.9)
       (gl:rect 0.25 0.55 0.75 0.75)))))

(defmethod display ((button press-button))
  (call-next-method)
  (when (and (group button) (firedp (group button)))
    (gl:color 0 0 0 1)
    (gl:rect 0.1 0.5 0.9 0.9)
    (gl:color 1 1 0 0.9)
    (gl:rect 0.25 0.55 0.75 0.75)))

(defmethod initialize-instance :after
    ((object has-group) &key
                          group-class
                          name
                          action
                          location &allow-other-keys)
  (let ((group (ensure-group name group-class location :action action)))
    (setf (group object) group)
    (group-add object group)))

(defclass button-group (and-group
                        active-object
                        has-name
                        invisible
                        lambda-trigger)
  ())

(defclass press-button-group (latch button-group)
  ())

(defmethod update ((group press-button-group))
  (call-next-method)
  (dogroup (button group)
    (unless (pressedp button)
      (setf (firedp group) nil)
      (return))))

(defun make-button (name location action &key latchp)
  (make-instance (if latchp 'press-button 'button)
                 :name name
                 :group-class (if latchp 'press-button-group 'button-group)
                 :location location
                 :action action))

(defmethod trigger :after ((group button-group))
  (if (action group)
      (funcall (action group))
      (call-next-method)))

