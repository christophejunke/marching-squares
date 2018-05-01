(in-package :marching-squares)

(defgeneric update (object)
  (:method (_))
  (:method ((sequence sequence))
    (map () #'update sequence)))

(declaim (type function squarep))

(defclass garbage () ())

(defgeneric garbagep (item)
  (:method ((g garbage)) t)
  (:method (_) nil))

(defclass invisible () ())

(defgeneric display (element)
  (:method (_))
  (:method :around ((invisible invisible)))
  (:method ((stack cons))
    (destructuring-bind (head . tail) stack
      (display tail)
      (display head))))

(defclass solid () ())

(defgeneric allow-move-p (object target)
  (:method (item (empty null)) nil)
  (:method (item (solid solid)) nil)
  (:method (any (g garbage)) t)
  (:method (any anywhere) nil)
  (:method (mobile (multi sequence))
    (every (lambda (item)
             (or (null item)
                 (allow-move-p mobile item)))
           multi)))





