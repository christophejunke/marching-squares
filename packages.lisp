(defpackage :marching-squares
  (:use
   :cl
   :alexandria)
  (:import-from :sdl2
                #:set-render-draw-color
                #:scancode-key-to-value
                #:scancode-value
                #:with-everything
                #:with-renderer
                #:with-event-loop
                #:gl-make-current)
  (:import-from :trivia
                #:match))


