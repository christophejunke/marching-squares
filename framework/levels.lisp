(in-package :marching-squares)

(defclass level (namespace
                 has-dimensions
                 has-name
                 has-location-cache
                 has-palettes
                 layer-stack)
  ((array :accessor level-array :initarg :array)
   (blueprint :accessor blueprint :initarg :blueprint)
   (game :accessor game :initarg :game)))

(defvar *default-bindings*
  '((#\space . nil)
    (#\# . :wall)
    (#\- . :door)))

(loop for (symbols dx dy) in '(((:northwest :nw) -1 -1)
                               ((:north     :n)   0 -1)
                               ((:northeast :ne) +1 -1)
                               ((:west      :w)  -1  0)
                               ((:east      :e)  +1  0)
                               ((:southwest :sw) -1 +1)
                               ((:south     :s)   0 +1)
                               ((:southeast :se) +1 +1)
                               ((:center    :c)   0  0))
      do (dolist (symbol symbols)
           (setf (get symbol 'direction) (cons dx dy))))

(defmethod add-object-at-level-location
    ((level level) row col object &key &allow-other-keys)
  (let* ((array (level-array level))
         (value (aref array row col)))
    (cond
      ((eq value object))
      ((null value) (setf (aref array row col) object))
      ((consp value) (unless (member object value)
                       (setf (aref array row col)
                             (list* object value))))
      (t (setf (aref array row col) (list object value))))))

(defgeneric neighbours (location &rest directions)
  (:method ((object has-location) &rest directions)
    (apply #'neighbours (location object) directions))
  (:method (location &rest directions)
    (let* ((level (level location))
           (array (level-array level))
           (x (col location))
           (y (row location)))
      (flet ((neighbour (direction)
               (destructuring-bind (dx . dy) (get direction 'direction)
                 (let ((x (+ x dx))
                       (y (+ y dy)))
                   (when (array-in-bounds-p array y x)
                     (loc level y x))))))
        (mapcar #'neighbour directions)))))

(defparameter *incorporate-location* nil)

(defmethod incorporate progn ((location loc) object)
  (let ((*incorporate-location* location))
    (incorporate (level location) object)))

(defun augment-palettes (palettes)
  (loop
    for (key . list) in palettes
    collect (cons key (append list *palette*))))

(defmethod build ((blueprint level-blueprint) game)
  (with-accessors ((height height)
                   (width width)
                   (rows grid)
                   (palettes blueprint-palettes)
                   (triggers blueprint-triggers)
                   (class level-class))
      blueprint
    (let* ((dimensions (list height width))
           (array (make-array dimensions :initial-element nil))
           (level (make-instance class
                                 :dimensions dimensions
                                 :game game
                                 :name (name blueprint)
                                 :array array
                                 :blueprint blueprint
                                 :current-palette *palette*
                                 :palettes (augment-palettes palettes))))
      (dolist (entry triggers)
        (incorporate game (build entry game)))
      (prog1 level
        (dotimes (row height)
          (dotimes (col width)
            (let ((location (loc level row col)))
              (incorporate location
                           (build (ignore-errors
                                   (aref (aref rows row) col))
                                  location)))))
        (when-let ((hook (start-hook blueprint)))
          (funcall hook level))
        (set-title (format nil "~a ― ~a" (title game) (name level)))))))

(defmethod build ((item character) (location loc))
  (flet ((retrieve-from (alist)
           (let ((entry (assoc item alist)))
             (and entry
                  (build (cdr entry) location)))))
    (or (retrieve-from (bindings (blueprint (level location))))
        (retrieve-from *default-bindings*))))

(defmethod incorporate progn ((level level) item)
  (incorporate (game level) item))

(defmethod microstep ((level level) ratio)
  (let ((array (level-array level)))
    (dotimes (i (array-total-size array))
      (microstep (row-major-aref array i) ratio))))

(defmethod display ((level level))
  (apply #'gl:clear-color (eval-color :background))
  (gl:clear :color-buffer :depth-buffer)
  (map () #'display (layers level)))

