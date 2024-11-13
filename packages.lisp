(defpackage :marching-squares
  (:use
   :cl
   :sdl2-event-loop
   :optima
   :alexandria)
  (:use :sdl2-event-loop.events)
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


