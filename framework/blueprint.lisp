(in-package :marching-squares)

(defclass has-dimensions ()
  ((width :accessor width :initarg :width)
   (height :accessor height :initarg :height)))

(defmethod initialize-instance :after ((object has-dimensions)
                                       &key dimensions &allow-other-keys)
  (when dimensions
    (destructuring-bind (&optional height width) (ensure-list dimensions)
      (let ((height (or height width (error "unexpected")))
            (width (or width height (error "unexpected"))))
        (setf (width object) width
              (height object) height)))))

(defgeneric build (blueprint parent)
  (:documentation "Build an object from a blueprint and a context"))

(defgeneric incorporate (parent object)
  (:documentation "Incorporate an OBJECT into a PARENT object")
  (:method-combination progn)
  (:method progn (a b)))

(defgeneric extract-from (parent object)
  (:documentation "Extract incorporated OBJECT from PARENT")
  (:method-combination progn)
  (:method progn (a b)))

(defclass level-blueprint (has-dimensions has-name)
  ((grid :accessor grid :initarg :grid)
   (bindings :accessor bindings :initarg :bindings)
   (class :accessor level-class :initarg :class :initform 'level)
   (palettes :accessor blueprint-palettes :initarg :palettes :initform nil)
   (triggers :accessor blueprint-triggers :initarg :triggers :initform nil)
   (start-hook :accessor start-hook :initarg :on-start :initform nil)
   (on-winning :accessor on-winning :initarg :on-winning :initform nil))
  (:default-initargs :name "anonymous"))

(defun ensure-blueprint (blueprint)
  (typecase blueprint
    (symbol (funcall blueprint))
    (function (funcall blueprint))
    (cons (destructuring-bind (_ level) blueprint
            (declare (ignore _))
            (funcall level)))
    (t blueprint)))

(defun map-blueprint-grid (function blueprint)
  (with-accessors ((height height)
                   (width width)
                   (rows grid))
      blueprint
    (dotimes (row height)
      (dotimes (col width)
	(funcall function
		 (ignore-errors
		   (aref (aref rows row) col))
		 :col col
		 :row row)))))

(defmethod initialize-instance :after ((blueprint level-blueprint)
				       &key &allow-other-keys)
  (let ((unknowns)
	(unused)
	(chars (alist-hash-table
		(mapcar (lambda (alist) (cons (car alist) 0))
			(bindings blueprint)))))
    (flet ((cell (char &key &allow-other-keys)
	     (when char
	       (multiple-value-bind (value ok) (gethash char chars 0)
		 (if (or ok (assoc char *default-bindings*))
		     (setf (gethash char chars) (1+ value))
		     (pushnew char unknowns)))))
	   (entry (k v)
	     (when (= 0 v)
	       (pushnew k unused))))
      (map-blueprint-grid #'cell blueprint)
      (maphash #'entry chars))
    (when unknowns
      (cerror "IGNORE" "Unknown characters: ~s" unknowns))
    (when unused
      (warn "Unused characters: ~a" unused))))

(defvar *blueprints-pathname*
  (ensure-directories-exist
   (asdf:system-relative-pathname :marching-squares "blueprints/*.txt")))

(defun grid-from-file (pathname)
  (coerce
   (with-open-file (in (merge-pathnames pathname *blueprints-pathname*))
     (loop for line = (read-line in nil nil) while line collect line))
   '(vector string)))
