;;;; gap.asd

(asdf:defsystem #:gap
  :description "Describe gap here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:uiop
	       #:fiveam
	       #:cffi
	       #:sdl2
	       #:sdl2-ttf
	       #:alexandria)
  :components ((:file "package") ;; package.lisp
               (:file "gap") ;; gap.lisp
	       (:file "sdl2/sdl2") ;; sdl2/sdl2.lisp
	       ))


;; (ql:quickload "uiop") ;; directory stuff
;; (ql:quickload "fiveam") ;; testing suite

;; ;; realistically if we want to actually use this stuff we need some sort of viewer
;; (ql:quickload "sdl2")
;; (ql:quickload "sdl2-ttf")
;; (ql:quickload "alexandria")


