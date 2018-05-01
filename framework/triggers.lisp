;;;; TRIGGERS API: don't call triggerable before trigger, just call
;;;; trigger, which checks the test function anyway. But do specialize
;;;; on triggerable for each kind of object.

;; FIXME too many different classes of triggers

(in-package :marching-squares)

(defclass has-triggers ()
  ((triggers :accessor triggers :initform (make-group))))

(defmethod reinitialize-instance :after
    ((object has-triggers) &key &allow-other-keys)
  (group-clear (triggers object)))

(defclass trigger (has-layer) ()
  (:default-initargs :layer :triggers))

(defclass global-trigger (trigger) ())

(defgeneric triggerable (trigger)
  (:method (_) t))

(defgeneric trigger (trigger)
  (:method (anything))
  (:method ((triggers sequence))
    (map () #'trigger triggers))
  (:method :around ((trigger trigger))
    (when (triggerable trigger)
      (call-next-method))))

;; Integration with namespace

(defclass named-trigger (trigger has-name) ())

(defgeneric trigger-by-name (name context)
  (:method ((name symbol) (context namespace))
    (map () #'trigger (ensure-list (resolve name context)))))

;; Oneshot triggers

(defclass oneshot () ())
(defmethod trigger :after ((trigger oneshot))
  (leave-current-location% trigger)
  (change-class trigger 'garbage))

;; Lambda triggers hold a callback function

(defclass lambda-trigger (global-trigger has-location)
  ((action :accessor action :initarg :action)))

(defmethod trigger ((trigger lambda-trigger))
  (funcall (action trigger)))

;; Global triggers are incorporated

(defmethod incorporate progn ((root has-triggers)
                              (trigger global-trigger))
  (group-add trigger (triggers root)))

(defmethod display ((trigger trigger))
  (gl:color 0 0 0 0.3)
  (gl:rect 0.4 0.4 0.6 0.6))

(defmethod allow-move-p (mobile (trigger trigger)) t)

;;;

(defclass latch (trigger)
  ((fired :accessor firedp :initform nil)))

(defmethod triggerable ((trigger latch))
  (and (not (firedp trigger))
       (call-next-method)))

(defmethod trigger ((trigger latch))
  (unless (firedp trigger)
    (setf (firedp trigger) t)
    (call-next-method)))

