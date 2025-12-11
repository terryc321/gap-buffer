;;;; package.lisp

(uiop:define-package #:gap
  (:use #:cl)
  (:local-nicknames (#:tt #:fiveam))
  (:local-nicknames (#:alex #:alexandria))
  (:local-nicknames (#:sdl2 #:sdl2))
  (:local-nicknames (#:ttf #:sdl2-ttf)))
  
;; (uiop:define-package :text-atlas
;;   (:use :cl)
;;   (:local-nicknames (:sdl2 :sdl2))
;;   (:local-nicknames (:ttf :sdl2-ttf))
;;   (:export :run))

