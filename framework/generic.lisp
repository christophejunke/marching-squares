(in-package :marching-squares)

(defgeneric update (object)
  (:method (_))
  (:method ((sequence sequence))
    (map () #'update sequence)))

(defclass garbage () ())

(defgeneric garbagep (item)
  (:method ((g garbage)) t)
  (:method (_) nil))

(defclass invisible () ())

(defgeneric display (element)
  (:method (_))
  (:method ((_ (eql :empty)))
    (color :background)
    (gl:rect 0 0 1 1))
  (:method :around ((invisible invisible)))
  (:method ((stack cons))
    (destructuring-bind (head . tail) stack
      (when tail (display tail))
      (display head))))

(defclass solid () ())
(defclass immaterial () ())

(defun solidp (x) (typep x 'solid))

(defgeneric allow-move-p (object target)
  (:method (item (empty null)) nil)
  (:method (item (solid solid)) nil)
  (:method (any (g garbage)) t)
  (:method (any (_ immaterial)) t)
  (:method (any anywhere) nil)
  (:method (mobile (multi sequence))
    (every (lambda (item)
             (or (null item)
                 (allow-move-p mobile item)))
           multi)))





