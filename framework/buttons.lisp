(in-package :marching-squares)

(defclass button (solid
                  has-location
                  has-group)
  ((pressed-by :initform nil
               :reader pressedp
               :accessor pressed-by)))

(defgeneric is-pressed-by (button object)
  (:method (any-button any-object) nil))

(defgeneric button-pressed-p (button)
  (:method ((button button))
    (destructuring-bind (north) (neighbours button :n)
      (find-if (lambda (o) (is-pressed-by button o))
               (objects-at north)))))

(defmethod update ((button button))
  (call-next-method)
  (setf (pressed-by button)
        (button-pressed-p button)))

(defmethod triggerable ((button button))
  (pressed-by button))

(defclass press-button (button) ())

(defmethod display ((button button))
  (colrect :button/background 0 0 1 1)

  ;; top rectangle (press status)
  (colrect (if (triggerable button)
               :button/pressed
               :button/unpressed)
           0.1
           (if (pressedp button) 0.2 0.1)
           0.9
           0.3)
  (when (group button)
    (cond
      ((triggerable (group button))
       (color :button/fired)
       (gl:rect 0.25 0.55 0.75 0.75))
      (t
       (color :button/inert)
       (gl:rect 0.25 0.55 0.75 0.75)))))

(defclass button-group (group
                        active-object
                        has-name
                        invisible
                        lambda-trigger)
  ())

(defclass press-button-group (latch and-button-group)
  ())

(defmethod update ((group press-button-group))
  (call-next-method)
  (dogroup (button group)
    (unless (triggerable button)
      (setf (firedp group) nil)
      (return))))

(defun make-button (name location action &key latchp (combine :and))
  (make-instance (if latchp 'press-button 'button)
                 :group-name name
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

