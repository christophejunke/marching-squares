(in-package :marching-squares)

(defclass has-keymap ()
  ((keymap :accessor keymap
           :initarg :keymap
           :initform (make-hash-table))))

(defun code-value (scancode)
  (or (scancode-key-to-value scancode)
      (error "Unknown scancode key ~S" scancode)))

(defgeneric keybind (key keymap)
  (:method ((key symbol) keymap)
    (keybind (code-value key) keymap))
  (:method ((key null) keymap))
  (:method ((key number) (hash hash-table))
    (gethash key hash))
  (:method (key (item has-keymap))
    (keybind key (keymap item))))

(defgeneric (setf keybind) (value key keymap)
  (:method (value (key symbol) keymap)
    (setf (keybind (code-value key) keymap) value))
  (:method ((value null) key (hash hash-table))
    (remhash key hash))
  (:method (value (key number) (hash hash-table))
    (setf (gethash key hash) value))
  (:method (value key (item has-keymap))
    (setf (keybind key (keymap item)) value)))

(defgeneric clear-keymap (keymap)
  (:method ((object has-keymap))
    (clear-keymap (keymap object)))
  (:method ((hash hash-table))
    (clrhash hash)))
