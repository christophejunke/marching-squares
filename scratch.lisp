(in-package :marching-squares)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (defparameter *game*
    (make-instance 'marching-squares::marching-squares
                   :microsteps 11
                   :microsteps-duration 0.1
                   :sleep-delay 0.21
                   :level-blueprint '(:level tut-mirror)))
  (start-game *game*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setf (level-blueprint *game*) 'marching-squares::tut-0)
(setf (level-blueprint *game*) 'tut-chute)


;; (defparameter *ramping-level*
;;   (setf (level-blueprint *game*)
;; 	(make-instance
;; 	 'level-blueprint
;; 	 :width 31
;; 	 :height 31
;; 	 :grid #("         V   W     V                     "
;; 		 "                       ###########       "
;; 		 "############# ######################    "
;; 		 "#######              ######################    "
;; 		 "########             #####################    "
;; 		 "#######              ##############      "
;; 		 "####MMM####=##=##=###MMM###########         "
;; 		 "####MM     V  V  V    MM###########         "
;; 		 "####Mb                 M########  "
;; 		 "           8  8  8                "
;; 		 "              8                   "
;; 		 "                                  "
;; 		 "               "
;; 		 "          ############### ###########       "
;; 		 "    #####    #            ###########               "
;; 		 "     #                    ###########               "
;; 		 " #           #            ###########               "
;; 		 "### ## ### # ########## # ###########               "
;; 		 "### ##^###^# # ########^# ###########               "
;; 		 "### ##     # # ###      # ###########               "
;; 		 "### ##     # # #E#      # ###########               "
;; 		 "### ##     # #         e# ###########               "
;; 		 "### ##     # #          # ###########               "
;; 		 "###^##     ###          #^###########               "
;; 		 "                                                    "
;; 		 "%%%%%%%%%%%%%%%# #%%%%%%%%%%%%%%%%%%%%%%%%%%"
;; 		 "               # #                                  "
;; 		 "               #                                     "
;; 		 "               #                                     "
;; 		 "               #####                                 "
;; 		 "                 @                                   ")
;; 	 :bindings '((#\b . (:trigger :release x))
;; 		     (#\B . (:blocked-square x))
;; 		     (#\e . (:trigger :release y))
;; 		     (#\E . (:blocked-square y))
;; 		     (#\@ . (:trigger :win))
;; 		     (#\8 . (:trigger :invert))
;; 		     (#\V . :start)
;; 		     (#\W . (:start :inverted))
;; 		     (#\% . :vanisher)
;; 		     (#\= . (:door door-2))
;; 		     (#\^ . (:door door-4))))))

;; (setf (level-blueprint *game*) 'zeroth)
;; (setf (level-blueprint *game*) 'test)
;; (setf (level-blueprint *game*) 'intro-level)
;; (setf (level-blueprint *game*) *chicken-level*)
;; (setf (level-blueprint *game*) 'button-intro-level)
;; (setf (level-blueprint *game*) 'button-intro-level/hard)
;; (setf (level-blueprint *game*) 'button-intro-level/bis)
;; (setf (level-blueprint *game*) *ramping-level*)
;; (setf (level-blueprint *game*) 'level/lateral-thinking)

;;!!!!
(setf (palette-inverted-square (palette *game*))
      (list 1.0 0 0 0.5))

(setf (palette-inverted-square (palette *game*))
      (list 0 0 1 0.5))

(setf (palette-inverter (palette *game*))
      (list 1.0 0 0 0.5))


(let ((*game* (make-instance 'marching-squares :blueprint 'tut-0)))
  (start-game *game*))

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
                 "            V      V                "
                 "                                   "
                 "                                   "
                 "         ### ###                   "
                 "        #                           "
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
         :name ""
         :palettes '((:yellow (:background 0.6 0.5 0.0 1.0 )))
         :bindings `((#\b . (:trigger :release x))
                     (#\B . (:blocked-square x))
                     (#\e . (:trigger :release y))
                     (#\E . (:blocked-square y))
                     (#\f . (:trigger :release z))
                     (#\F . (:blocked-square z))
                     (#\@ . (:trigger :win))
		     (#\X . (:trigger :loose))
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

(defun test ()
  (make-instance
   'level-blueprint
   :name "test"
   :width 11
   :height 11
   :grid #("##V      ##"
	   "##       ##"
	   "######## ##"
	   "##       ##"
	   "##       ##"
	   "##-########"
	   "##       ##"
	   "##       ##"
	   "######## ##"
	   "######## ##"
	   "########@##"
	   "")
   :bindings '((#\@ . (:trigger :win))
               (#\V . :start)
               (#\- . (:door a)))
   :on-winning (next 'zeroth)))

(defun zeroth ()
  (make-instance
   'level-blueprint
   :name "first steps"
   :width 31
   :height 31
   :grid #("#######      V       # # # ####"
           "#######             ## ########"
           "#######              #   #  ###"
           "#######             ###########"
           "#######                  #   ##"
           "####### ################ ######"
	   "#######             #### # # ##"
	   "#######        b  # #### # ####"
	   "###########B#######B#### # # ##"
	   "         ## ####### ##   "
	   ""
	   "#######                 #######"
	   " # # #                  #######    "
	   "   #                    #######      "
           " #    # ####### ####### #######"
	   "### # #-#######-#######-#######"
	   "  # # # ####### ####### #######"
	   "# ##### ####### ####### #######"
	   "#       ####### ####### #######"
	   "####### ####### ####### #######"
	   "    ### ####### ####### ###"
	   "    ### ####### ####### ###"
	   "        ####### ####### "
	   "        ####### ####### "
	   ""
	   "      #@#     #@#     #@#"
	   "      ###     ###     ###"
	   "      ###     ###     ###"
	   "      ###     ###     ###"
	   "      ###     ###     ###"
	   "      ###     ###     ###"
	   )
   :on-start #'pick-palette
   :on-winning (next 'intro-level)
   :palettes `((t . ((:wall 0 0 0 .55)
                     (:background 0.3 0.5 0.6 1)
                     (:foreground 1 1 1 1))))
   :bindings `((#\b . (:release x))
               (#\B . (:blocked-square x))
               (#\@ . (:trigger :win))
               (#\V . :start)
               (#\- . (:door door-1)))))

(defun level/lateral-thinking ()
  (make-instance
   'level-blueprint
   :name "secret garden"
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
           "     ##### #       # #####     "
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
   :bindings `((#\b . (:release x))
               (#\B . (:blocked-square x))
               (#\@ . (:trigger :win))
               (#\8 . :invert)
               (#\V . :start)
               (#\W . (:start :inverted))
               (#\S . (:button button-group-3 (:trigger gate-4)))
               (#\- . (:door door-1))
               (#\= . (:door door-2))
               (#\^ . (:gate gate-4))
               (#\X . (:trigger :lose))
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
	       (#\X . (:trigger :lose))
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
   :width 31
   :height 32
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
   :name "ocean's seven"
   :width 33
   :height 33
   :grid #(""
	   "     #  V     #     V #      "
           "     #Z### ####~#######"
           "     ###                         "
           "                                 "
           "       ######L######L##         "
           "       #    #######                 "
           " ###V    ##              #####  "
           " ###:##############-#-########  "
           " ###                #    B####  "
           " ###          ##    #     ####"
           " ####-##########^####S###    "
           "                                "
           "                                "
           " #############S######## ####    "
           " ############              #     "
	   "           #E              #"
           "                       b   #    "
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
   :palettes `((t . ((:wall 0 0 0 .85)
		     (:square 0.8 1 1 1)
		     (:inverter 0 0 0 1)
                     (:blocked-square 1 1 1 0.45)
                     (:background 0.25 0.25 0.25 1)
                     (:foreground 1 1 1 0.5)
                     (:door 1 1 1 0.65))))
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


(defclass button-intro-level/hard/destroy
    (global-trigger has-location oneshot transformable)
  ())

(defmethod transform-model-view
    ((tt button-intro-level/hard/destroy))
  (gl:translate 0.5 0.5 0.5))

(defmethod display ((tt button-intro-level/hard/destroy))
  (color :wall)
  (csq (+ 0.1 (random 0.02))))

(defmethod triggerable ((trigger button-intro-level/hard/destroy))
  (some-square-p (location trigger)))

(defclass blocked-wall-square (wall-square blockable)
  ()
  (:default-initargs :blockedp t))

(defmethod display ((square blocked-wall-square))
  (color '(:alpha 0.75 :wall))
  (gl:rect 0 0 1 1)
  (color :wall)
  (gl:rect 0.1 0.1 0.9 0.9))

(defmethod trigger ((trigger button-intro-level/hard/destroy))
  (dogroup (item (mobiles (game (level (location trigger)))))
    (when (typep item 'blocked-wall-square)
      (setf (blockedp item) nil))))

(defun button-intro-level/hard ()
  (make-instance
   'level-blueprint
   :name "lockpicking"
   :width 31
   :height 32
   :grid #("#############"
	   "############E                   "
           "############           VVVV#    "
           "             #=#=#=#=#######     "
           "             # #e# #            "
           "             # # # #            "
           "           ### #+# ###_#####    "
           "                 #              "
           "              8  #8      g      "
           "    8########### ####G#$*##8    "
           "           ##### #### ##  #     "
           "        F  ##### #### ##  # #  "
           "       #!# ###   #    #####+#  "
           "           ###                  "
           "        f  ###   #              "
           "   #######+###,# #,#:#:#####   "
           "   #######   # #,# # # #####    "
	   "##             #   #   ##T#T     "
	   "##                     #TTTT     "
	   "##                     TTTTT     "
	   "###################### TTTTT     "
	   "######################/TTTT   # #   "
	   "##                    /      ## # "
	   "##                    /      ## # "
	   "##                   Q #L    ## # "
	   "##                   Q ##    ## ##"
	   "##                   Q ##    ## ##"
	   "##                   Q ##    ## ##"
	   "##                  ?Q:##    ## ##"
	   "##                  qQ ######## "
	   "##@@@@@@@@@@@@@################ "
	   "############################### "
	   "############################### "
	   "############################### "
	   "############################### "
           )
   :on-start #'pick-palette
   :palettes `((t . ((:wall 0 0 0 .85)
		     (:square 0.8 1 1 1)
		     (:inverter 0 0 0 1)
                     (:blocked-square 1 1 1 0.45)
                     (:background 0.15 0.15 0.15 1)
                     (:foreground 1 1 1 0.5)
                     (:door 1 1 1 0.65)))


	       (unlock-right-square
		(:door 0.3 0.3 1 1))
	       ($ (:button/fired 0.3 0.3 1 1)
		  (:button/inert 0.3 0.3 1 0.5))

	       (z (:blocked-square 0.1 1 0.1 0.35))
	       ((:releaser-for z) (:foreground 0 1 0 1))

	       (w (:blocked-square 0.6 0.6 1 0.35))
	       ((:releaser-for w) (:foreground .5 .5 1 1))

	       (q (:blocked-square 1 0.7 0.7 0.15))
	       ((:releaser-for q) (:foreground 0.9 0.4 0.4 0.5))
	       (bye (:flash/feedback 1 1 0.3 1)
		    (:foreground     1 1 0.3 0.45))
	       (final (:flash/feedback 1 0.3 0.3 1)
		      (:foreground     1 0.3 0.3 0.65)))

   :bindings `((#\b . (:trigger :release x))
               (#\B . (:blocked-square x))
               (#\e . (:trigger :release y))
               (#\E . (:blocked-square y))
               (#\f . (:trigger :release z))
               (#\F . (:blocked-square z))
               (#\g . (:trigger :release w))
               (#\G . (:blocked-square w))
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
	       (#\Q . (:blocked-square q))
	       (#\q . (:trigger :release q))
	       (#\T . (:class/loc blocked-wall-square))
	       (#\/ . (:invisible-blocker))
	       (#\? . (:class/loc button-intro-level/hard/destroy)))))

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


;; (trace 'incorporate)

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


;; NO: e.g. prepare next blueprint while level is playing
;;
;; (defmethod (setf level-blueprint) :after ((blueprint has-dimensions)
;;                                           (game game))
;;   (setf (width game) (width blueprint)
;;         (height game) (height blueprint)))


(map 'list #'next-move (items (mobiles *game*)))

(setf (input-state *game*)
      (make-instance 'square-input))

(setf (input-state *game*)
      (make-instance 'input-sequence
                     :inputs
                     '(:left
                       :left
                       :left
                       :wait
                       :wait
                       (:right 8)
                       :wait
                       (:right 3)
                       (:left 5)
                       :wait
                       (:right 5)
                       (:right 6)
                       :wait
                       :right
                       (:left 4)
                       nil
                       nil)))

(setf *grid* nil)

(setf (input-state *game*)
      (make-instance 'input-sequence
                     :inputs
                     '(:left
                       :left
                       :left
                       :wait
                       nil
                       :wait
                       (:right 8)
                       :wait
                       (:left 11)
                       (:right 29)
                       (:left 7)
                       (nil 3)
                       :left
                       nil
                       :left
                       nil
                       :left
                       nil
                       :left
                       (nil 4)
                       (:left 6)
                       (nil 3)
                       :left
                       (:right 4)
                       (nil 4)
                       (:right 7)
                       :wait
                       nil
                       :wait
                       :left)))

;; TESTS
;;
;; (defmethod display ((surface sdl2-ffi:sdl-surface))
;;   (sdl2:with-rects ((rect 0
;;                           0
;;                           (sdl2:surface-width surface)
;;                           (sdl2:surface-height surface)))
;;     (sdl2:blit-surface surface rect *window* rect)))
