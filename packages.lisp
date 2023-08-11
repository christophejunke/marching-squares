(defpackage :marching-squares
  (:use
   :cl
   :bricabrac.sdl2.event-loop
   ;; :bricabrac.docstrings
   :optima
   :alexandria)
  (:import-from :sdl2
                #:set-render-draw-color
                #:scancode-key-to-value
                #:scancode-value
                #:with-init
                #:with-window
                #:with-gl-context
                #:with-renderer
                #:with-event-loop
                #:gl-make-current))


