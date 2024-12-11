(in-package :marching-squares)

(defun tut ()
  (make-instance
   'level-blueprint
   :palettes '((:default . ((:background 0.3 0.5 0.6 1.0))))
   :name "← Marching square →"
   :dimensions 12
   :grid #("     V      "
           "            "
           "            "
           "            "
           "#          #"
           "#         ##"
           "##        ##"
           "##        ##"
           "##        ##"
           "##        ##"
           "##        ##"
           "##   @    ##")
   :on-start (lambda (level) (pick-palette level :default))
   :on-winning (next 'tut-0)
   :bindings '((#\X . (:trigger :lose))
               (#\@ . (:trigger :win))
               (#\V . :start)
               (#\M . (:help! "Use left and right arrows"))
               (#\E . (:help "Press Esc to restart level"))
	       (#\- . (:door door-1)))))

(defun tut-0 ()
  (make-instance
   'level-blueprint
   :palettes '((:default . ((:background 0.3 0.5 0.6 1.0))))
   :name "← Marching square →"
   :dimensions 12
   :grid #("#### V    ##"
           "####      ##"
           "###       ##"
           "##        ##"
           "######### ##"
           "#####     ##"
           "#####     ##"
           "##### ######"
           "##        ##"
           "##        ##"
           "##E###### ##"
           "#########@##")
   :on-start (lambda (level) (pick-palette level :default))
   :on-winning (next 'tut-marching-square)
   :bindings '((#\X . (:trigger :lose))
               (#\@ . (:trigger :win))
               (#\V . :start)
               (#\M . (:help! "Use left and right arrows"))
               (#\E . (:help "Press Esc to restart level"))
	       (#\- . (:door door-1)))))

(defun tut-marching-square ()
  (make-instance
   'level-blueprint
   :palettes '((:default . ((:background 0.2 0.3 0.2 1.0)
                            (:help 0.2 0.9 0.2 1.0))))
   :name "Marching squares"
   :dimensions 12
   :grid #("#########V##"
           "#   ####   #"
           "# V ####   #"
           "# ######## #"
           "# ######## #"
           "#          #"
           "#          #"
           "### ########"
           "#          #"
           "#          #"
           "#          #"
           "######@#@###")
   :on-start (lambda (level) (pick-palette level :default))
   :on-winning (next 'tut-marching-squares)
   :bindings '((#\X . (:trigger :lose))
               (#\@ . (:trigger :win))
               (#\V . :start)
               (#\M . (:help! "All exits must contain a square"))
               (#\E . (:help! "Press Esc to restart level"))
	       (#\- . (:door door-1)))))


(defun tut-marching-squares ()
  (make-instance
   'level-blueprint
   :palettes '((:default . ((:background 0.4 0.1 0.1 1.0)
                            (:help 1.0 0.0 0.0 1.0))))
   :name "More marching squares"
   :dimensions 22
   :grid #("######V#V#############"
           "##                 ###"
           "##                 ###"
           "##  VV V VVVVV     ###"
           "#################  ###"
           "##                 ###"
           "##                 ###"
           "##                 ###"
           "##                 ###"
           "## ########### #######"
           "## ###################"
           "##           #########"
           "##           #########"
           "##           #########"
           "##  ######## #########"
           "############ #########"
           "##                 ###"
           "##                 ###"
           "### ######   ###   ###"
           "### ######   ###   ###"
           "### ##################"
           "##O@O#################"
           )
   :on-start (lambda (level) (pick-palette level :default))
   :on-winning (next 'tut-door)
   :bindings '((#\X . (:trigger :lose))
               (#\@ . (:trigger :win))
               (#\O . (:fake :win))
               (#\V . :start)
               (#\M . (:help! "Here only one square is required to exit"))
	       (#\- . (:door door-1)))))


(defun tut-door ()
  (make-instance
   'level-blueprint
   :palettes '((:default . ((:background 0.2 0.2 0.3 1.0)
                            (:help 1.0 1.0 0.0 1.0)))
               (door-1 . ((:foreground 0.3 0.7 0.3 1.0)
                          (:flash/feedback 0.4 1.0 0.4 1.0)))
               (door-2 . ((:foreground 1.0 0.5 0.5 1.0)
                          (:flash/feedback 1.0 0.6 0.6 1.0))))
   :name "Door"
   :dimensions 11
   :grid #("##### V ### "
           "##      ### "
           "##      ### "
           "##-######## "
           "##           "
           "##         "
           "##           "
           "#####=### ###"
           "####   ## ###"
           "#### E ## ###"
           "#########@###"
)
   :on-start (lambda (level) (pick-palette level :default))
   :on-winning (next 'tut-doors)
   :bindings '((#\X . (:trigger :lose))
               (#\@ . (:trigger :win))
               (#\V . :start)
               (#\M . (:help! "If you don't stop, you don't fall"))
               (#\E . (:help "Press Esc to restart"))
	       (#\- . (:door door-1))
               (#\= . (:door door-2)))))

(defun tut-doors ()
  (make-instance
   'level-blueprint
   :palettes '((:default . ((:background 0.2 0.2 0.3 1.0)
                            (:help 1.0 1.0 0.0 1.0))))
   :name "Doors"
   :dimensions 13
   :grid #("##V##########"
           "       #"
           "      V#V    "
           "      ###-#  "
           "  #       ##   "
           " ##       ####  "
           "### # #   ####   "
           "### #-## #####   #"
           "### # ## #####   #"
           "###       ####   "
           "###        ####   "
           "###        ####   "
           "####@##@##@###  "
           )
   :on-start (lambda (level) (pick-palette level :default))
   :on-winning (next 'tut-buttons)
   :bindings '((#\X . (:trigger :lose))
               (#\@ . (:trigger :win))
               (#\V . :start)
               (#\M . (:help! "If you don't stop, you don't fall"))
               (#\E . (:help "Press Esc to restart"))
	       (#\- . (:door door-1))
               (#\= . (:door door-2)))))


(defun tut-buttons ()
  (make-instance
   'level-blueprint
   :palettes '((:default . ((:background 0.2 0.2 0.3 1.0)
                            (:help .5 1.0 0.0 1.0))))
   :name "Buttons and gates"
   :dimensions 13
   :grid #("####V##V##V###"
           "     #   #     "
           "     #   #    "
           "     #   ###S"
           "###^##        "
           "     #"
           "     ###S#####"
           "                 "
           "                 "
           "            "
           "             "
           "######^######"
           "######@######")
   :on-start (lambda (level) (pick-palette level :default))
   :on-winning (next 'tut-release)
   :bindings '((#\X . (:trigger :lose))
               (#\@ . (:trigger :win))
               (#\V . :start)
               (#\S . (:button button-group-1 (:trigger gate-1)))
               (#\Z . (:button button-group-2 (:trigger gate-2)))
               (#\^ . (:gate gate-1))
               (#\+ . (:gate gate-2))
               (#\M . (:help! "Press all buttons")))))

(defun tut-release ()
  (make-instance
   'level-blueprint
   :palettes '((:default . ((:background 0.2 0.4 0.3 1.0)
                            (:help .5 1.0 0.0 1.0))))
   :name "Blocked square"
   :dimensions 13
   :grid #("      V      "
           "            "
           "#b       #      "
           "###B######"
           "###           "
           "###           "
           "###           "
           "### #     ###   "
           "###-#####-###          "
           "### #####  ##"
           "                "
           "                "
           "###@#####@###")
   :on-start (lambda (level) (pick-palette level :default))
   :on-winning (next 'tut-freeze)
   :bindings '((#\X . (:trigger :lose))
               (#\@ . (:trigger :win))
               (#\V . :start)
               (#\M . (:help! "Doors of the same group open when all pressed"))
               (#\b . (:trigger :release s0))
               (#\B . (:blocked-square s0))
	       (#\- . (:door door-1))
               (#\= . (:door door-2)))))

(defun tut-freeze ()
  (make-instance
   'level-blueprint
   :palettes '((:default . ((:background 0.1 0.3 0.2 1.0)
                            (:help .5 1.0 0.0 1.0)
                            (:foreground 1/10 1/10 1/10 1)
                            (:win/flash 1.0 0.5 0.5 1)))
               (s0 . ((:square 0.2 0.2 0.2 1)
                      (:blocked-square 0.1 0.1 0.1 1))))
   :name "Debris"
   :dimensions 13
   :grid #("###V##!##V###"
           "    !#      !"
           "b   !# /  #     "
           "####!### ## #"
           "    B##   ! !"
           "     ### /    "
           "     # # #! !"
           "       #s#! !#"
           "     #   #!# ###"
           "    ##     #####"
           "# ####   #####"
           "#S####  ######"
           "######O@OOOOO")
   :on-start (lambda (level) (pick-palette level :default))
   :on-winning (next 'tut-mirror)
   :bindings '((#\@ . (:trigger :win))
               (#\O . (:fake :win))
               (#\! . (:class/loc wall-square))
               (#\/ . (:invisible-blocker))
               (#\V . :start)
               (#\S . (:button bg1 (:trigger g1)))
               (#\s . (:gate g1))
               (#\b . (:trigger :release s0))
               (#\B . (:class/loc visibly-wall-square :name s0 :blockedp t)))))

(defun tut-mirror ()
  (make-instance
   'level-blueprint
   :palettes '((:default . ((:wall 0.1 0 0 1)
                            (:background 0.7 0.2 0.2 1)
                            (:help .5 1.0 0.0 1.0)))
               (door-1 . ((:foreground 0.8 0.4 0.4 1.0)
                          (:flash/feedback 1.0 0.3 0.3 1.0))))
   :name "Mirror"
   :dimensions 13
   :grid #("      V      "
           "#           #"
           "#########-###"
           "###         # "
           "### 8       ##"
           "##########  ##"
           "# B ##### b ##   "
           "#          "
           "# #       # #"
           "# #       # #  "
           "# ####==##### "
           "# ##      ###"
           "######@@#####"

           )
   :on-start (lambda (level) (pick-palette level :default))
   :on-winning (next 'tut-mirror-2)
   :bindings '((#\X . (:trigger :lose))
               (#\@ . (:trigger :win))
               (#\V . :start)
               (#\8 . (:trigger :invert))
               (#\M . (:help! "Doors of the same group open when all pressed"))
               (#\b . (:trigger :release s0))
               (#\B . (:blocked-square s0))
	       (#\- . (:door door-1))
               (#\= . (:door door-2)))))


(defun tut-mirror-2 ()
  (make-instance
   'level-blueprint
   :palettes '((:default . ((:wall 0.1 0 0 1)
                            (:background 0.7 0.2 0.2 1)
                            (:help .5 1.0 0.0 1.0)))
               ((:releaser-for s0) (:foreground 0 0 0 1))
               (s0 . ((:square 0.2 0.2 0.2 1)
                      (:blocked-square 0.1 0.1 0.1 1)))
               (door-1 . ((:foreground 0.8 0.4 0.4 1.0)
                          (:flash/feedback 1.0 0.3 0.3 1.0))))
   :name "Mirror constraints"
   :dimensions 13
   :grid #("######VW#####"
           "#           #"
           "# V         #"
           "# ##8########"
           "###   #######"
           "##     ######"
           "##     ######"
           "## ~~~ ######"
           "###   #######"
           "#### ########"
           "##     ######"
           "##     ######"
           "######@######"

           )
   :on-start (lambda (level) (pick-palette level :default))
   :on-winning (next 'tut-chute)
   :bindings '((#\@ . (:trigger :win))
               (#\V . :start)
               (#\W . (:start :inverted))
               (#\8 . (:trigger :invert))
               (#\~ . (:door d1)))))

(defun tut-chute ()
  (make-instance
   'level-blueprint
   :palettes '((:default . ((:wall 0.1 0 0 1)
                            (:background 0.5 0.2 0.7 1)
                            (:help .5 1.0 0.0 1.0))))
   :name "Time"
   :dimensions 13
   :grid #("      V      "
           "#           #"
           "#########-###"
           "           # "
           "         b # "
           "##B####### ##"
           "#   ########## "
           "#"
           "#           #"
           "#           #"
           "# #         # "
           "# #####=###=# "
           "#######@###@##" )

   :on-start (lambda (level) (pick-palette level :default))
   :on-winning (next 'intro-level)
   :bindings '((#\X . (:trigger :lose))
               (#\@ . (:trigger :win))
               (#\V . :start)
               (#\8 . (:trigger :invert))
               (#\B . (:blocked-square s0))
               (#\b . (:trigger :release s0))
               (#\M . (:help! "Doors of the same group open when all pressed"))
               (#\- . (:door door-1))
	       (#\= . (:door door-2)))))

(defun level/lateral-thinking ()
  (make-instance
   'level-blueprint
   :name "Asymetry"
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
                     (:outside . :background)
                     (:blocked-square 0.3 1 0.3 0.5)
                     (:background 0.3 0.5 0.3 1)
                     (:foreground 0 1 0 1)
                     (:door 0 1 0 0.35)))
               (door-2 . ((:foreground 0.2 0.6 0.2 1.0)
                          (:flash/feedback 0.3 0.9 0.3 1.0))))
   :on-winning #'quit-game
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

