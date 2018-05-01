(defsystem :marching-squares
  :depends-on (:trivia :alexandria :sdl2 :sdl2-ttf :cl-opengl)
  :components ((:file "packages")
               (:module
                "framework"
                :components ((:file "generic")
                             (:file "mixins")
                             (:file "blueprint")
                             (:file "locations")
                             (:file "levels")
                             (:file "namespace")
                             (:file "doors")
                             (:file "triggers")
                             (:file "groups")
                             (:file "mobiles")
                             (:file "release")
                             (:file "transform")
                             (:file "display")
                             (:file "active")
                             (:file "buttons")
                             (:file "keymaps")
                             (:file "games")))
               (:module
                "entities"
                :components ((:file "squares")))
               (:file "ms"))
  :author "Christophe Junke <junke.christophe@gmail.com>"
  :license "MIT")
