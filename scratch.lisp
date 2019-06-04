(in-package :marching-squares)

(defparameter *game*
  (make-instance 'marching-squares
                 :level-blueprint 'intro-level))

(setf (level-blueprint *game*) 'intro-level)
(setf (level-blueprint *game*) *chicken-level*)
(setf (level-blueprint *game*) 'button-intro-level)
(setf (level-blueprint *game*) 'button-intro-level/bis)
(setf (level-blueprint *game*) *ramping-level*)
(setf (level-blueprint *game*) 'level/lateral-thinking)

;;!!!!
(setf (palette-inverted-square (palette *game*))
      (list 1.0 0 0 0.5))

(setf (palette-inverted-square (palette *game*))
      (list 0 0 1 0.5))

(setf (palette-inverter (palette *game*))
      (list 1.0 0 0 0.5))

(start-game *game*)

(setf (input-state *game*) (make-square-input))

;; (allow-move-p 
;;  (first (objects-at (loc (game-level *game*) 28 21)))
;;  (first (objects-at (loc (game-level *game*) 28 22))))

(setf (palette-background (palette *game*)) '(1 0.9 0.9 1.0 )
      (palette-wall (palette *game*)) '(0.3 0 0 0.9))

(setf (palette-wall (palette *game*)) '(0 0 0 1))

(shake 0.1 0 3)

;; NOTE : alexandria gaussian random pull request!!! when zero zero

(resolve 'door-1 *game*)

(first (resolve 'door-3 *game*))


(let ((loc (copy-loc
            (location (dogroup (trigger (triggers *game*))
                        (when (typep trigger 'level-1/check-alternative-solution)
                          (return trigger)))))))
  (decf (row loc))
  (activate-square loc))

;; 0.03
;; 1.48
;; (trigger (first (resolve 'shake-destroy *game*)))

;; (decf *size* 11)

(let ((out *standard-output*)
      (first t))
  (defmethod update :around (any)
    (when first
      (print
       (compute-applicable-methods #'update (list any))
       out)
      (setf first nil))
    (call-next-method)))

(setf (palette-background (palette *game*))
      '(0.1 0.2 0.3 1.0 ))

(setf (palette-blocked-square (palette *game*))
      '(1 1 1 0.25))

(setf (palette-inverted-square (palette *game*))
      (list 0.7 0.7 1.0 0.25))

(defparameter *chicken-level*
  (setf (level-blueprint *game*)
        (make-instance
         'level-blueprint
         :width 33
         :height 33
         :grid #("                                   "
                 "                                   "
                 "                                   "
                 "                                   "
                 "         #######                   "
                 "        #   V      V               "
                 "                                    "
                 "                                    "
                 "            #  ### #               "
                 "                                   "
                 "           ###    ###              "
                 "                                   "
                  "                                   "
                 "                                   "
                 "          #####  #####             "
                 "                                   "
                 "                                   "
                 "               #      #            "
                 "        ##     ##     ##           "
                 "                                   "
                 "                                   "
                 "                                   "
                 "XXXXXXX######-########-##XXXXXXXXXX"
                 "                                   "
                 "                                   "
                 "            #@#      #@#           "
                 "            # #      # #           "
                 "                                   "
                 "                                   "
                 "                                   "
                 "                                   "
                 "                                   "
                 "                                   "
                 "                                   "
                 "                                   "
                 "                                   "
                 )
         :on-start (lambda (level) (pick-palette level :yellow))
         :name "Chicken"
         :palettes '((:yellow (:background 0.6 0.5 0.0 1.0 )))
         :bindings `((#\b . (:trigger :release x))
                     (#\B . (:blocked-square x))
                     (#\e . (:trigger :release y))
                     (#\E . (:blocked-square y))
                     (#\f . (:trigger :release z))
                     (#\F . (:blocked-square z))
                     (#\@ . (:trigger :win))
                     (#\> . (:blocked-square exit))
                     (#\< . (:trigger :release exit))
                     (#\8 . (:trigger :invert))
                     (#\V . :start)

                     (#\W . (:start :inverted))
                     (#\H . :help)
                     (#\% . :vanisher)
                     (#\u . (:spawn spawn-1))
                     (#\U . (:press-button button-group-1 (:trigger spawn-1)))
                     (#\Z . (:button button-group-1 (:trigger gate-3)))
                     (#\S . (:button button-group-3 (:trigger gate-4)))
                     (#\L . (:button button-group-2 (:trigger gate-5)))
                     (#\$ . (:button $ (:trigger unlock-right-square)))
                     (#\+ . (:door final))
                     (#\, . (:door bye))

                     (#\* . (:gate must-close :state :open))
                     (#\! . (:button button-group-5 (:trigger must-close)))

                     (#\_ . (:gate unlock-right-square))
                     (#\- . (:door door-1))
                     (#\= . (:door door-2))
                     (#\~ . (:gate gate-3))
                     (#\^ . (:gate gate-4))
                     (#\: . (:gate gate-5))))))

(setf (level-blueprint *game*) 'level/lateral-thinking)

(defun level/lateral-thinking ()
  (make-instance
   'level-blueprint
   :name "Gravity"
   :width 31
   :height 31
   :grid #("                               "
           "                              " 
           "               V                "
           "               #               "
           "        #             #        "
           "        #             #        "
           " V      #             #      W  "
           " ###### #             # ###### "
           "  #####-## ####-#### ##-#####  "
           "      # ## #### #### ## #      "
           "      #    ####8####    #      "
           "      #    #### ####    #      "
           "           ####B####           "
           "           #       #             "
           "     #####^#       #^#####     "
           "XXX  ##### #       # #####b XXX  "
           "     ##### #       # #####       "
           "   # #####           ##### #     "
           "   # #####           ##### #     "
           "   # #                   # #     "
           "   # #         8         # #      "
           "   #          ###          #      "
           "   #           #           #      "
           "   #S####==### # ###==####S#      "
           "   ####        #        ####      "
           "        #@@#   #   #@@#          "
           "        ####   #   ####          "
           "         ##    #    ##           "
           "         ##    #    ##           "
           "         ##    #    ##           "
           "         ##    #    ##           "
           "         ##    #    ##           "
           "                                 ")
   :on-start #'pick-palette
   :palettes `((t . ((:wall 0 0 0.2 .5)
                     (:blocked-square 0.3 1 0.3 0.5)
                     (:background 0.3 0.5 0.3 1)
                     (:foreground 0 1 0 1)
                     (:door 0 1 0 0.35)))) 
   :bindings `((#\b . (:trigger :release x))
               (#\B . (:blocked-square x))
               (#\e . (:trigger :release y))
               (#\E . (:blocked-square y))
               (#\f . (:trigger :release z))
               (#\F . (:blocked-square z))
               (#\@ . (:trigger :win))
               (#\> . (:blocked-square exit))
               (#\< . (:trigger :release exit))
               (#\8 . (:trigger :invert))
               (#\V . :start)
               (#\W . (:start :inverted))
               (#\H . :help)
               (#\% . :vanisher)
               (#\u . (:spawn spawn-1))
               (#\U . (:press-button button-group-1 (:trigger spawn-1)))
               (#\Z . (:button button-group-1 (:trigger gate-3)))
               (#\S . (:button button-group-3 (:trigger gate-4)))
               (#\L . (:button button-group-2 (:trigger gate-5)))
               (#\$ . (:button $ (:trigger unlock-right-square)))
               (#\+ . (:door final))
               (#\, . (:door bye))
               (#\* . (:gate must-close :state :open))
               (#\! . (:button button-group-5 (:trigger must-close)))
               (#\_ . (:gate unlock-right-square))
               (#\- . (:door door-1))
               (#\= . (:door door-2))
               (#\~ . (:gate gate-3))
               (#\^ . (:gate gate-4))
               (#\: . (:gate gate-5))
               ;; (#\X . (:trigger :lose))
               )))

(defun button-intro-level ()
  (make-instance
   'level-blueprint
   :name "Buttons"
   :width 33
   :height 33
   :grid #(""
           ""
           ""
           "             #####              "
           "             ##O##               "
           "                                "
           " #8#                       #8#   "
           "  #                         #   "
           "  #            V            #    "
           "  #########-#######=#######+#    "
           "  ######### ####### #########   "
           "     #                  #       "
           "     #                 W#       "
           "     ########~#*#~#######       "
           "              ###               "
           "              ###               "
           "              ###               "
           "     ##/###############/##      " 
           "     ##################### "
           "     ####             ####      "
           "     ####             ####      "
           "  ## ####             #### ##   "
           "  #######             #######   "
           "                                "
           "                                "
           "                                "
           "                                "
           )
   :on-start #'pick-palette
   :palettes '((t
                (:background 0.2 0.2 0.2 3)))
   :bindings `((#\V . :start)
               (#\W . (:start :inverted))
               (#\8 . (:trigger :invert))

               (#\+ . (:button button-0 (:trigger gate-0)))
               (#\= . (:gate gate-0))

               (#\* . (:button button-1 (:trigger gate-1)))
               (#\- . (:gate gate-1))
               
               (#\/ . (:button releaser-button (:release blocked-0)))

               (#\O . (:blocked-square blocked-0))
               (#\o . (:trigger :release blocked-0))

               (#\~ . (:door door-0))
               
               (#\b . (:trigger :release x))
               (#\B . (:blocked-square x))
               (#\e . (:trigger :release y))
               (#\E . (:blocked-square y))
               (#\f . (:trigger :release z))
               (#\F . (:blocked-square z))
               (#\@ . (:trigger :win))
               (#\> . (:blocked-square exit))
               (#\< . (:trigger :release exit))
               (#\8 . (:trigger :invert))
               (#\W . (:start :inverted))
               (#\H . :help)
               (#\% . :vanisher)
               (#\u . (:spawn spawn-1))
               (#\U . (:press-button button-group-1 (:trigger spawn-1)))
               (#\§ . (:button button-group-4 (:trigger gate-6)))
               (#\Z . (:button button-group-1 (:trigger gate-3)))
               (#\S . (:button button-group-3 (:trigger gate-4)))
               (#\L . (:button button-group-2 (:trigger gate-5)))
               (#\$ . (:button $ (:trigger unlock-right-square)))
               (#\+ . (:button single-wall-release (:trigger gate-for-wall)))
               (#\, . (:door bye))
               (#\* . (:gate must-close :state :open))
               (#\! . (:button button-group-5 (:trigger must-close)))
               (#\_ . (:gate unlock-right-square))
               (#\- . (:door door-1))
               (#\= . (:gate gate-for-wall))
               (#\~ . (:gate gate-3))
               (#\^ . (:gate gate-4))
               (#\: . (:gate gate-5))
               (#\j . (:gate gate-6))
               (#\Q . (:class/loc visibly-wall-square)))))

(defun button-intro-level/bis ()
  (make-instance
   'level-blueprint
   :name "Buttons"
   :width 33
   :height 33
   :grid #(" "
           " "
           " "
           " "
           ""
           "######  V         V  V         "
           "######        #              ##"
           "######Z## #####~########## #### "
           "########              ####j#### "
           "#######                       # "
           "####                     #    # "
           "####   ######L######L##  ##   # "
           "####   #    ###          #    # "
           "####V    ##              #    # "
           "####:############## # #### +### "
           "####              #-#-#### #### "
           "####          ##    #    B  ### "
           "#########     ##    #       ### "
           "#####QQ##-######^####S###   ### "
           "##   QQ         b           ### "
           "##   QQ                     ### "
           "##  #QQ§§§####S######## ### ###"
           "##  #QQ################j### ###"
           "##  #==################ ### ### "
           "##  #  ################ ### ### "
           "##  #  ################ ### ### "
           "##  ################### ### ### "
           "##  ################### ### ### "
           "##                          ### "
           "##                          ### "
           "#####################@@@@@##### "
           "############################### "
           "                                "
           )
   :on-start #'pick-palette
   :palettes '((t . ((:background 0.7 0.65 0.40 1)))
               (single-wall-release
                (:button/fired 1 0 0 1)
                (:button/inert :alpha 0.3 :button/fired))
               (button-group-4
                (:button/fired 0.6 0.6 1 1)
                (:button/inert 0.3 0.3 0.5 1))
               (gate-6 . ((:door 0.4 0.4 1 1)
                          (:wall 0 0 0.3 1)))
               (gate-for-wall . ((:door 1 0 0 1)
                                 (:wall 0.3 0 0 1))))
   :bindings `((#\b . (:trigger :release x))
               (#\B . (:blocked-square x))
               (#\e . (:trigger :release y))
               (#\E . (:blocked-square y))
               (#\f . (:trigger :release z))
               (#\F . (:blocked-square z))
               (#\@ . (:trigger :win))
               (#\> . (:blocked-square exit))
               (#\< . (:trigger :release exit))
               (#\8 . (:trigger :invert))
               (#\V . :start)
               (#\W . (:start :inverted))
               (#\H . :help)
               (#\% . :vanisher)
               (#\u . (:spawn spawn-1))
               (#\U . (:press-button button-group-1 (:trigger spawn-1)))
               (#\§ . (:button button-group-4 (:trigger gate-6)))
               (#\Z . (:button button-group-1 (:trigger gate-3)))
               (#\S . (:button button-group-3 (:trigger gate-4)))
               (#\L . (:button button-group-2 (:trigger gate-5)))
               (#\$ . (:button $ (:trigger unlock-right-square)))
               (#\+ . (:button single-wall-release (:trigger gate-for-wall)))
               (#\, . (:door bye))
               (#\* . (:gate must-close :state :open))
               (#\! . (:button button-group-5 (:trigger must-close)))
               (#\_ . (:gate unlock-right-square))
               (#\- . (:door door-1))
               (#\= . (:gate gate-for-wall))
               (#\~ . (:gate gate-3))
               (#\^ . (:gate gate-4))
               (#\: . (:gate gate-5))
               (#\j . (:gate gate-6))
               (#\Q . (:class/loc visibly-wall-square)))))

(defun button-intro-level ()
  (make-instance
   'level-blueprint
   :name "Buttons"
   :width 33
   :height 33
   :grid #("        V         V         "
           "     #        #             "
           "     #Z## #####~##############"
           "     ###                         "
           "                                 "
           "       ######L######L##         "
           "       #    ###                 "
           " ###V    ##              #####  "
           " ###:############## # ########  "
           " ###              #-#-########  "
           " ###          ##    #    B####"
           " ###          ##    #               "
           " ############-##^####S###    "
           "                b               "
           "                                "
           " #############S######## ####    "
           " ###########E              #     "
           "                           #    "
           "             #=#=#=#=#######     "
           "             # #e# #            "
           "             # # # #            "
           "           ### #+# ###_#####    "
           "                 #              "
           "              8  #8             "
           " ###8########### ####F#$*###8   "
           " #          #### #### ##   #    "
           " #      F   #### #### ##   # #  "
           " #     #!#            ######+#  "
           " #                           #  "
           "        f                    #  "
           " # #######,#,#,#,#,#,#,#######  "
           " #########@#@#@#@#@#@#@#######  "
           )
   :on-start #'pick-palette
   :palettes '((t . ((:background 0.7 0.5 0 1))))
   :bindings `((#\b . (:trigger :release x))
               (#\B . (:blocked-square x))
               (#\e . (:trigger :release y))
               (#\E . (:blocked-square y))
               (#\f . (:trigger :release z))
               (#\F . (:blocked-square z))
               (#\@ . (:trigger :win))
               (#\> . (:blocked-square exit))
               (#\< . (:trigger :release exit))
               (#\8 . (:trigger :invert))
               (#\V . :start)
               (#\W . (:start :inverted))
               (#\H . :help)
               (#\% . :vanisher)
               (#\u . (:spawn spawn-1))
               (#\U . (:press-button button-group-1 (:trigger spawn-1)))
               (#\Z . (:button button-group-1 (:trigger gate-3)))
               (#\S . (:button button-group-3 (:trigger gate-4)))
               (#\L . (:button button-group-2 (:trigger gate-5)))
               (#\$ . (:button $ (:trigger unlock-right-square)))
               (#\+ . (:door final))
               (#\, . (:door bye))
               (#\* . (:gate must-close :state :open))
               (#\! . (:button button-group-5 (:trigger must-close)))
               (#\_ . (:gate unlock-right-square))
               (#\- . (:door door-1))
               (#\= . (:door door-2))
               (#\~ . (:gate gate-3))
               (#\^ . (:gate gate-4))
               (#\: . (:gate gate-5)))))

(defparameter *new-level*
  (setf (level-blueprint *game*)
        (make-instance
         'level-blueprint
         :width 31
         :height 32
         :grid #("#  V                     V   #            "
                 "#                           #             "
                 "########-################-##             "
                 "#        #                 #              "
                 "#        #                 #              "
                 "####~###########Z###########              "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 "                                          "
                 )
         :bindings `((#\b . (:trigger :release x))
                     (#\B . (:blocked-square x))
                     (#\e . (:trigger :release y))
                     (#\E . (:blocked-square y))
                     (#\f . (:trigger :release z))
                     (#\F . (:blocked-square z))
                     (#\@ . (:trigger :win))
                     (#\> . (:blocked-square exit))
                     (#\< . (:trigger :release exit))
                     (#\8 . (:trigger :invert))
                     (#\V . :start)
                     (#\W . (:start :inverted))
                     (#\H . :help)
                     (#\% . :vanisher)
                     (#\u . (:spawn spawn-1))
                     (#\U . (:press-button button-group-1 (:trigger spawn-1)))
                     (#\Z . (:button button-group-1 (:trigger gate-3)))
                     (#\S . (:button button-group-3 (:trigger gate-4)))
                     (#\L . (:button button-group-2 (:trigger gate-5)))
                     (#\$ . (:button $ (:trigger unlock-right-square)))
                     (#\+ . (:door final))
                     (#\, . (:door bye))

                     (#\* . (:gate must-close))
                     (#\! . (:button button-group-5 (:trigger must-close)))

                     (#\_ . (:gate unlock-right-square))
                     (#\- . (:door door-1))
                     (#\= . (:door door-2))
                     (#\~ . (:gate gate-3))
                     (#\^ . (:gate gate-4))
                     (#\: . (:gate gate-5))))))


(defparameter *test-level*
  (setf (level-blueprint *game*)
        (make-instance
         'level-blueprint
         :width 31
         :height 31
         :grid #("    L         L         u   "
                 "                            "
                 "                            "
                 "                            "
                 "                                "
                 "##U#Z#######U#Z####-##-#~##~# #####    "
                 ""
                 "                  #%##%#%##%#     "
                 "                  # ## # ## #     "
                 ""
                 "    8    8   8   8 WV 8   8           "
                 " #################################")
         :bindings `((#\b . (:trigger :release x))
                     (#\B . (:blocked-square x))
                     (#\e . (:trigger :release y))
                     (#\E . (:blocked-square y))
                     (#\f . (:trigger :release z))
                     (#\F . (:blocked-square z))
                     (#\L . :start)
                     ;; (#\X . (:trigger :lose))
                     (#\@ . (:trigger :win))
                     (#\8 . (:trigger :invert))
                     (#\V . :start)
                     (#\W . (:start :inverted))
                     (#\H . :help)
                     (#\% . :vanisher)
                     (#\u . (:spawn spawn-1))
                     (#\U . (:press-button button-group-1 (:trigger spawn-1)))
                     (#\Z . (:button button-group-2 (:trigger gate-3)))
                     (#\- . (:door door-1))
                     (#\= . (:door door-2))
                     (#\~ . (:gate gate-3))
                     (#\^ . (:door door-4))
                     (#\: . (:door door-5))))))


(trace 'incorporate)

(setf (palette-background (palette *game*))
      (list 0.4 0.6 0.4 1))

(setf (palette-background (palette *game*))
      (list 0.8 0.7 0.2 1))

(setf (palette-background (palette *game*))
      (list 0.6 0.2 0.2 1))

(invert (elt (items (mobiles *game*)) 2))

(setf (location (elt (items (mobiles *game*)) 2)) :trash)

(map ()
     (lambda (u)
       (setf (decoration u)
             :test-decoration))
     (items (mobiles *game*)))

(let ((location (location (elt (mobiles (game-level *game*)) 2))))
  (setf (location (elt (mobiles (game-level *game*)) 2))
        (loc (game-level *game*) (- (row location) 4) (- (col location) 2))))

(defun dbg ()
  (loop
    with array = (items (active-objects *game*))
    with limit = (fill-pointer array)
    for i below (array-total-size array)
    when (= i limit) do (format t "~&--------------------~%")
      do (format t "~&~3,,'0d ~S~%" i (aref array i))
    finally (return array)))

(loop
  (dotimes (i 10) (terpri))
  (dbg)
  (sleep 0.5))

(trigger-by-name :start (game-level *game*))

(setf (location (find-if #'squarep (items (mobiles *game*)))) :trash)

(trigger-by-name 'spawn-1 *game*)

;; test-level, one door says open, the other one closes
;; (expected behaviour)
(progn
  (activate-square (loc (game-level *game*) 1 22))
  (loop repeat 30
        do (activate-square (loc (game-level *game*) 1 19))
        (sleep 0.1)))

(defclass test-game (game) ())
(defmethod game-idle ((game test-game)))

(defmethod game-setup progn ((game test-game))
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:blend-equation :func-add)
  (gl:enable :depth-test))

(defmethod game-idle ((game test-game))
  (gl:clear-color 1 1 0 1)
  (gl:clear :color-buffer)
  (gl:clear :depth-buffer)

  (gl:with-pushed-matrix
    (gl:color 1 1 1 0.5)
    (gl:translate 0 0 -0.2)
    (gl:rect -0.5 -0.5 0.5 0.5))
  (gl:with-pushed-matrix
    (gl:color 0 0 0 0.5)
    (gl:rect 0 0 1 1))

  (gl:flush)
  (sdl2:gl-swap-window *window*)
  (sleep 0.1))

(start-game (make-instance 'test-game :width 300 :height 300))

(loop
  for i from 0 below 31
  for loc = (loc (game-level *game*) 6 i)
  unless (objects-at loc)
    do (activate-square loc))

(loop
  for i from 0 below 31
  for loc = (loc (game-level *game*) 29 i)
  do (map () (lambda (u) (change-class u 'garbage))
          (remove-if-not #'squarep (objects-at loc))))
