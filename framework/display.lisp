(in-package :marching-squares)

(defclass layer () ())

(defclass grid-layer (layer)
  ((grid :reader layer-grid :writer (setf layer-grid%))))

(defclass sequence-layer (layer)
  ((sequence :accessor layer-sequence
             :initform nil
             :initarg :sequence)))

(defmethod initialize-instance :after ((layer grid-layer)
                                       &key dimensions &allow-other-keys)
  (setf (layer-grid% layer)
        (make-array dimensions :initial-element nil)))

(defgeneric layer-add (layer object location)
  (:method ((layer null) object location))
  (:method (layer (object null) location))
  (:method ((array array) object (location loc))
    (when #1=(aref array (row location) (col location))
          (error "Non-empty array cell at location ~a when adding ~a:~% ~a"
                 location object array))
    (setf #1# object))

  (:method ((layer grid-layer) object location)
    (layer-add (layer-grid layer) object location))

  (:method ((layer sequence-layer) (object has-location) (location loc))
    (assert (equalp (location object) location))
    (with-accessors ((row row) (col col)) location
      (let ((sequence (delete-if #'garbagep (layer-sequence layer))))
        (setf (layer-sequence layer)
              (merge 'list
                     sequence
                     (list object)
                     #'location<
                     :key #'location))))))

(defun find-layer (stack symbol)
  (cdr (assoc symbol (layers stack))))

(defclass has-layer ()
  ((layer% :initarg :layer :accessor has-layer/layer)))

(defgeneric layer (stack object)
  (:method ((stack layer-stack) (object invisible)) nil)
  (:method ((stack layer-stack) object)
    (cdr (default-layer stack)))
  (:method ((stack layer-stack) (object has-layer))
    (find-layer stack (has-layer/layer object))))

(defgeneric make-layer (name type &key &allow-other-keys)
  (:method (name (type (eql :grid)) &key dimensions &allow-other-keys)
    (cons name (make-instance 'grid-layer
                              :dimensions dimensions)))
  (:method (name (type (eql :list)) &key &allow-other-keys)
    (cons name (make-instance 'sequence-layer))))

(defun make-layer-from-specification (specification dimensions)
  (destructuring-bind (name &optional (type :list)) (ensure-list specification)
    (make-layer name type :dimensions dimensions)))

(defun make-layers (specifications dimensions)
  (mapcar (rcurry #'make-layer-from-specification dimensions)
          specifications))

(defclass layer-stack ()
  ((default-layer :accessor default-layer
                  :initform :background)
   (layers :accessor layers))
  (:default-initargs
   :specifications 
   '((:BACKGROUND :GRID) :STATIC :TRIGGERS :MOBILES :OVERLAY)))

(defmethod incorporate progn ((stack layer-stack) object)
  (layer-add (layer stack object) object *incorporate-location*))

(defmethod initialize-instance :after
    ((stack layer-stack)
     &key specifications dimensions &allow-other-keys)
  (setf (layers stack)
        (make-layers specifications dimensions))
  (let ((default-cell (assoc (default-layer stack) (layers stack))))
    (unless default-cell
      (error "Default layer ~a not found in ~a"
             (default-layer stack)
             (layers stack)))
    (setf (default-layer stack) default-cell)))

;; (defmethod incorporate progn ((stack layer-stack) object)
;;   (layer-add stack (layer stack object) object))

(defparameter *stack-layer-restriction* nil)

(defmethod display ((stack layer-stack))
  "See also *STACK-LAYER-RESTRICTION*"
  (let ((restriction *stack-layer-restriction*))
    (etypecase restriction
      (null (map () #'display (layers stack)))
      (symbol (display (find-layer stack restriction))))))

(defmethod display ((layer sequence-layer) &aux some-garbage)
  (map ()
       (lambda (item)
         (if (garbagep item)
             (setf some-garbage t)
             (with-location-transform (:location (location item))
               (display item))))
       (layer-sequence layer))
  (when some-garbage
    (setf (layer-sequence layer)
          (delete-if #'garbagep (layer-sequence layer)))))

(defmethod display ((layer grid-layer))
  (display (layer-grid layer)))

(defmethod display ((array array))
  (destructuring-bind (rows cols) (array-dimensions array)
    (dotimes (row rows)
      (dotimes (col cols)
        (let ((cell (aref array row col)))
          (when cell
            (with-location-transform (:row row :col col)
              (display cell))))))))

(defun colrect (color x y w h)
  (set-color color)
  (gl:rect x y w h))

;; TODO change how palette works
(defstruct palette
  background
  foreground

  wall
  magnetic-wall

  inverter
  constrainer

  square
  blocked-square
  inverted-square
  constrained-square

  flash/feedback

  alpha
  beta
  gamma
  delta
  epsilon)

