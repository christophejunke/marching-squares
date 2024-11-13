(defsystem :marching-squares
  :depends-on (:trivia
               :alexandria
               :sdl2
               :sdl2-ttf
               :sdl2-event-loop
               :cl-opengl
               :bricabrac
               :bordeaux-threads)
  :components ((:file "packages")
               (:file "utils")
               (:module "framework"
                :depends-on ("packages")
                :serial t
                :components ((:file "generic")
                             (:file "mixins")
                             (:file "blueprint")
                             (:file "locations")
                             (:file "namespace")
                             (:file "triggers")
                             (:file "groups")
                             (:file "active")
                             (:file "levels")
                             (:file "mobiles")
                             (:file "doors")
                             (:file "transform")
                             (:file "palette")
                             (:file "display")
                             (:file "release")
                             (:file "buttons")
                             (:file "keymaps")
                             (:file "games")))

               (:module "entities"
                :components ((:file "squares")))

               (:file "ms")

               (:module "levels"
                :components ((:file "tutorials")))

               (:file "lev-1")
               )
  :author "Christophe Junke <junke.christophe@gmail.com>"
  :license "MIT")
