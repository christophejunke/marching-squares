(in-package :marching-squares)

(defparameter *palettes* nil
  "Association list of palettes")

(defparameter *palette*
  '((:background 0.4 0.4 0.5 1)
    (:outside . :wall)
    (:wall 0 0 0 1)
    (:help 1 1 0 0.5)
    (:square 1 1 1 1)
    (:flash/feedback 1 1 1 1)
    (:inverted-square 0 0 0 1)
    (:low-contrast 1 1 1 0.5)
    (:blocked-square . :low-contrast)
    (:foreground 1.0 1.0 1.0 0.7)
    (:inverter 0.8 0.8 0.0 1.)
    (:door 1 1 0 1)
    (:button/background . :wall)
    (:button/pressed . :flash/feedback)
    (:button/unpressed . :low-contrast)
    (:button/fired 1 1 0 0.9)
    (:button/inert :alpha 0.2 :button/fired)))

;; (setf (cdr (assoc :wall *palette*))
;;       (list 0 0 0 0.4))

(defparameter *night-mode* nil)

(defclass has-palettes ()
  ((palettes :accessor palettes
             :initarg :palettes
             :initform nil)
   (current-palette :accessor current-palette
                    :initarg :current-palette
                    :initform nil)))

(defgeneric palette (object)
  (:method-combination append :most-specific-first)
  (:method append (object) *palette*)
  (:method append ((object has-palettes))
    (current-palette object)))

(defun palette-identifiers (object)
  (mapcar #'first (palettes object)))

(defun pick-palette (object &optional (identifier t))
  (let ((entry (assoc identifier (palettes object))))
    (if entry
        (setf (current-palette object) (cdr entry))
        (warn "Unknown palette ~s in ~a" identifier object))))

;; (let ((object (make-instance 'has-palettes
;;                              :palettes `((:dark . ,*dark-palette*)
;;                                          (:light . ,*light-palette*)))))
;;   (pick-palette object :dark)
;;   (palette object))

(defun blender (ratio &aux (complement (- 1 ratio)))
  (lambda (v w) (+ (* v ratio) (* w complement))))

(defun eval-color (color)
  (flet ((n (x) (- 1 x)))
    (etypecase color
      (keyword (eval-color (cdr (assoc color *palette*))))
      (cons
       (etypecase (first color)
         (number (if *night-mode*
                     (destructuring-bind (r g b a) color
                       (list (n r) (n g) (n b) a))
                     color))
         (keyword (optima:ematch color
                    ((list :blend ratio c1 c2)
                     (mapcar (blender ratio)
                             (eval-color c1)
                             (eval-color c2)))
                    ((list :alpha alpha color)
                     (destructuring-bind (r g b _) (eval-color color)
                       (declare (ignore _))
                       (list r g b alpha))))))))))

;; (let ((*palette* (palette nil)))
;;   (eval-color '(:blend 1/2 :wall :inverted-square)))

(defgeneric get-color (object identifier)
  (:method (object (identifier symbol))
    (eval-color
     (cdr (assoc identifier (palette object))))))

(defun set-color (slot &key alpha palette)
  (destructuring-bind (r g b a)
      (cdr
       (assoc slot (or palette
                       (palette *game*))))
    (gl:color r g b (or alpha a))))

(defmethod display :around ((object has-palettes))
  (let ((*palettes* (palettes object))
        (*palette* (current-palette object)))
    (call-next-method)))

(defmethod display :around ((object has-name))
  (if-let (palette (assoc (name object) *palettes*))
    (let ((*palette* (cdr palette)))
      (call-next-method))
    (call-next-method)))

(defmethod call-within-group-context :around ((object has-name)
                                              (function function))
  (if-let (palette (assoc (name object) *palettes* :test #'equalp))
    (let ((*palette* (cdr palette)))
      (call-next-method))
    (call-next-method)))

(defun color (expression)
  (apply #'gl:color (eval-color expression)))

