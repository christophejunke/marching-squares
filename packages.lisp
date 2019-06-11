(defpackage :marching-squares
  (:use
   :bricabrac.sdl2.event-loop
   :cl
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


