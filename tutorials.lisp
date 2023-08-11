(in-package :marching-squares)

(defun tut-0 ()
  (make-instance
   'level-blueprint
   :palettes '((:default . ((:background 0.3 0.5 0.6 1.0))))
   :name "Move with left and right arrows"
   :width 10
   :height 10
   :grid #("#   V    #"
           "#        #"
           "########M#"
           "####     #"
           "####     #"
           "#### #####"
           "#        #"
           "#        #"
           "#E###### #"
           "########@#")
   :on-start (lambda (level) (pick-palette level :default))
   :on-winning (next 'tut-1)
   :bindings '((#\X . (:trigger :lose))
               (#\@ . (:trigger :win))
               (#\V . :start)
               (#\M . (:help! "Reach glowing exit (bottom right)"))
               (#\E . (:help! "Press Esc to restart level"))
	       (#\- . (:door door-1)))))

(defun tut-1 ()
  (make-instance
   'level-blueprint
   :palettes '((:default . ((:background 0.2 0.3 0.2 1.0)
                            (:help 0.2 0.9 0.2 1.0))))
   :name "All square move simultaneously"
   :width 11
   :height 11
   :grid #("# V ### V #"
           "#   ###   #"
           "# ####### #"
           "# ####### #"
           "#         #"
           "#    M    #"
           "### #######"
           "#         #"
           "#         #"
           "######@#@##"
           "###########")
   :on-start (lambda (level) (pick-palette level :default))
   :on-winning (next 'tut-2)
   :bindings '((#\X . (:trigger :lose))
               (#\@ . (:trigger :win))
               (#\V . :start)
               (#\M . (:help! "All exits must contain a square"))
               (#\E . (:help! "Press Esc to restart level"))
	       (#\- . (:door door-1)))))


(defun tut-2 ()
  (make-instance
   'level-blueprint
   :palettes '((:default . ((:background 0.4 0.1 0.1 1.0)
                            (:help 1.0 0.0 0.0 1.0))))
   :name "Squares can be blocked forever"
   :width 13
   :height 13
   :grid #("##VVV VVV ###"
           "#         ###"
           "#           #"
           "# ######### #"
           "#           #"
           "#           #"
           "#           #"
           "##########  #"
           "#           #"
           "#   M       #"
           "## ######   #"
           "## #######  #"
           "##@##########")
   :on-start (lambda (level) (pick-palette level :default))
   :on-winning (next 'tut-3)
   :bindings '((#\X . (:trigger :lose))
               (#\@ . (:trigger :win))
               (#\V . :start)
               (#\M . (:help! "Here only one square is required to exit"))
	       (#\- . (:door door-1)))))


(defun tut-3 ()
  (make-instance
   'level-blueprint
   :palettes '((:default . ((:background 0.2 0.2 0.3 1.0)
                            (:help 1.0 1.0 0.0 1.0))))
   :name "Doors open when pressed"
   :width 11
   :height 11
   :grid #("##### V ### "
           "##      ### "
           "##      ### "
           "##-######## "
           "##           "
           "##        # "
           "##M          "
           "#####=### ###"
           "#####E### ###"
           "######### ###"
           "#########@###"
           "#           #"
           "##          #"
           "##@##########")
   :on-start (lambda (level) (pick-palette level :default))
   :on-winning (next 'tut-4)
   :bindings '((#\X . (:trigger :lose))
               (#\@ . (:trigger :win))
               (#\V . :start)
               (#\M . (:help! "If you don't stop, you don't fall"))
               (#\E . (:help "Hint: keep the arrow button down (press Esc to restart)"))
	       (#\- . (:door door-1))
               (#\= . (:door door-2)))))

(defun tut-4 ()
  (make-instance
   'level-blueprint
   :palettes '((:default . ((:background 0.2 0.4 0.3 1.0)
                            (:help .5 1.0 0.0 1.0))))
   :name "Spinning triggers release new squares"
   :width 13
   :height 13
   :grid #("      V      "
           "            "
           "#b       #      "
           "###B######"
           "###           "
           "###M          "
           "###           "
           "### #     ###   "
           "###-#####-###          "
           "### #####  ##"
           "                "
           "                "           
           "###@#####@###"

           )
   :on-start (lambda (level) (pick-palette level :default))
   :on-winning (next 'tut-5)
   :bindings '((#\X . (:trigger :lose))
               (#\@ . (:trigger :win))
               (#\V . :start)
               (#\M . (:help! "Doors of the same group open when all pressed"))
               (#\b . (:trigger :release s0))
               (#\B . (:blocked-square s0))               
	       (#\- . (:door door-1))
               (#\= . (:door door-2)))))


(defun tut-5 ()
  (make-instance
   'level-blueprint
   :palettes '((:default . ((:background 0.6 0.2 0.4 1.0)
                            (:help .5 1.0 0.0 1.0))))
   :name ""
   :width 13
   :height 13
   :grid #("      V      "
           "#  8        #"
           "#########-###"
           "             "
           "#        b ##"
           "##B####### ##"
           "#   #########   "
           ""
           "#           #"
           "# #         #  "
           "# ###=###=###            "           
           "# #         #" 
           "#####@###@###"

           )
   :on-start (lambda (level) (pick-palette level :default))
   :on-winning (next 'tut-5)
   :bindings '((#\X . (:trigger :lose))
               (#\@ . (:trigger :win))
               (#\V . :start)
               (#\8 . (:trigger :invert))               
               (#\M . (:help! "Doors of the same group open when all pressed"))
               (#\b . (:trigger :release s0))
               (#\B . (:blocked-square s0))               
	       (#\- . (:door door-1))
               (#\= . (:door door-2)))))
