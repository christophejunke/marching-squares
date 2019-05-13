(in-package :marching-squares)

(defparameter *jumping-level*
  (make-instance
   'level-blueprint
   :width 31
   :height 31
   :grid #("             #           #####      "
           "             #           #####      "
           "             #           #####      "
           "             # V        §         "
           "             #         §          "
           "             #          §         "
           "             #         §          "
           "             a          §         "
           "                       §          "
           "     Y################/  ####     "
           "       ####       ####T  ####   "
           "       ####V      #####  ####    "
           "       #######^########^^####    "
           "                 ######  ####       "
           "              A  ######  ####       "
           "     Y#################  #### "
           "                 ######  ####                 "
           "                 ######++####     "
           "                                  "
           "                                  "
           "   ########## #########  ####          "
           "   ####################  ####         "
           " ########@###@#####@###  ##### ## "
           "XX#####################################")
   :on-start (lambda ()
               (setf (palette-background (palette *game*))
                     '(0.8 .8 0.1 0.1)))
   :bindings '((#\@ . (:trigger :win))
               (#\8 . (:trigger :invert))

               (#\a . (:blocked-square blocked-0))
               (#\A . (:trigger :release blocked-0))

               (#\V . :start)
               (#\H . (:help "Press Esc to restart level"))

               (#\^ . (:gate gate-1))
               (#\+ . (:gate gate-2))

               (#\T . (:button b1 (:trigger gate-1)))
               (#\Y . (:button b2 (:trigger gate-2)))
               (#\Z . (:button master (:trigger gate-1 gate-2)))

               (#\§ . (:class/loc wall-square))
               
               (#\/ . (:invisible-blocker))
               (#\T . (:button nil (:trigger gate-1)))
               (#\- . (:door door-1))
               (#\= . (:door door-2))
               (#\~ . (:door door-3))
               (#\^ . (:door door-4))
               (#\: . (:door door-5))

               ;; (t . (:or-group switch (:release gate-1 gate-2)))
               )))

(defclass master-button (button)
  ((open :initarg :open :accessor master-open)))

(setf (level-blueprint *game*) *jumping-level*)

*game*
(alexandria:once-only)
