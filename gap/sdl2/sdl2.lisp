

;; (ql:quickload :sdl2)
;; (ql:quickload :sdl2-ttf)

;; assuming we have :gap loaded
;; start with initially empty buffer
;;
;; use a simple sdl2 renderer
;; https://github.com/lispgames/cl-sdl2/blob/main/examples/renderer.lisp


;; (require :sdl2)
;; (require :sdl2-ttf)

(in-package :gap)

(defun test-render-clear (renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer))

(defun test-render-hello (renderer)
  (sdl2:set-render-draw-color renderer 255 0 0 255)
  ;; H
  (sdl2:render-draw-line renderer 20 20 20 100)
  (sdl2:render-draw-line renderer 20 60 60 60)
  (sdl2:render-draw-line renderer 60 20 60 100)
  ;; E
  (sdl2:render-draw-line renderer 80 20 80 100)
  (sdl2:render-draw-line renderer 80 20 120 20)
  (sdl2:render-draw-line renderer 80 60 120 60)
  (sdl2:render-draw-line renderer 80 100 120 100)
  ;; L
  (sdl2:render-draw-line renderer 140 20 140 100)
  (sdl2:render-draw-line renderer 140 100 180 100)
  ;; L
  (sdl2:render-draw-line renderer 200 20 200 100)
  (sdl2:render-draw-line renderer 200 100 240 100)
  ;; O
  (sdl2:render-draw-line renderer 260 20 260 100)
  (sdl2:render-draw-line renderer 260 100 300 100)
  (sdl2:render-draw-line renderer 300 20 300 100)
  (sdl2:render-draw-line renderer 260 20 300 20))

(defun test-render-lines (renderer)
  (sdl2:with-points ((a 200 200)
                     (b 300 400)
                     (c 400 200))
    (sdl2:set-render-draw-color renderer 0 0 255 255)
    (multiple-value-bind (points num) (sdl2:points* a b c)
      (sdl2:render-draw-lines renderer points num))))

(defun test-render-points (renderer)
  (sdl2:with-points ((a (random 800) (random 800))
                     (b (random 800) (random 800))
                     (c (random 800) (random 800)))
    (sdl2:set-render-draw-color renderer 0 255 0 255)
    (multiple-value-bind (points num) (sdl2:points* a b c)
      (sdl2:render-draw-points renderer points num))))

(defun test-render-rect (renderer)
  (sdl2:render-draw-rect renderer (sdl2:make-rect 400 400 35 35)))

(defun test-render-rects (renderer)
  (multiple-value-bind (rects num)
      (apply #'sdl2:rects*
             (loop :for x :upto 5
                   :for y :upto 5
                   :collect (sdl2:make-rect (+ 400 (* x 10)) (+ 200 (* y 10)) 8 8)))
    (sdl2:render-draw-rects renderer rects num)))

(defun test-render-fill-rect (renderer)
  (sdl2:render-fill-rect renderer (sdl2:make-rect 445 400 35 35)))

(defun test-render-fill-rects (renderer)
  (multiple-value-bind (rects num)
      (apply #'sdl2:rects*
             (loop :for x :upto 5
                   :collect (sdl2:make-rect (+ 500 (* x 10)) 400 8 8)))
    (sdl2:set-render-draw-color renderer 255 0 255 255)
    (sdl2:render-fill-rects renderer rects num)))

(defun renderer-test ()
  "Test the SDL_render.h API"
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "SDL2 Renderer API Demo" :flags '(:shown))
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
        (sdl2:with-event-loop (:method :poll)
          (:keyup
           (:keysym keysym)
           (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
             (sdl2:push-event :quit)))
          (:idle
           ()
           (test-render-clear renderer)
           (test-render-hello renderer)
           (test-render-lines renderer)
           (test-render-points renderer)
           (test-render-rect renderer)
           (test-render-rects renderer)
           (test-render-fill-rect renderer)
           (test-render-fill-rects renderer)
           (sdl2:render-present renderer)
	   (sdl2:delay 33))
          (:quit () t))))))



               ;; (hello-text (let* ((surface (sdl2-ttf:render-text-solid font
               ;;                                                         "hello world"
               ;;                                                         255
               ;;                                                         255
               ;;                                                         255
               ;;                                                         0))
               ;;                    (texture (sdl2:create-texture-from-surface my-renderer
               ;;                                                               surface)))
               ;;               (sdl2:free-surface surface)
               ;;               texture))
               ;; (destination-rect (sdl2:make-rect (round (- 150 (/ (sdl2:texture-width hello-text) 2.0)))
	       ;; 					 (round (- 150 (/ (sdl2:texture-height hello-text) 2.0)))
	       ;; 					 (sdl2:texture-width hello-text)
	       ;; 					 (sdl2:texture-height hello-text)))


(defmacro cheap-scancode (ch)
  (let ((sym (intern (string-upcase (format nil "SCANCODE-~a" ch)) "KEYWORD")))
    `((sdl2:scancode= (sdl2:scancode-value keysym) ,sym)
      (insert buf ,ch)
      (format t "user pressed letter ~a key!~%" ,ch)
      (update-text))))

;;(cheap-scancode #\a)

(defmacro cheap-scancode-head (ch)
  (let ((sym (intern (string-upcase (format nil "SCANCODE-~a" ch)) "KEYWORD")))
    `(sdl2:scancode= (sdl2:scancode-value keysym) ,sym)))

(defmacro cheap-scancode-body (ch)
  `(progn
     (insert buf ,ch)
     (format t "user pressed letter ~a key!~%" ,ch)
     (update-text)))


		 

;; hello-text is a sdl2 texture
(defun basic-example ()
  (let ((buf (make-buffer))
	(buf-len 6)
	(buf-str nil)
	(my-render nil)
	(dest-rect nil)
	(hello-text nil)	
	(font nil))
    (insert buf #\t)
    (insert buf #\e)
    (insert buf #\r)
    (insert buf #\r)
    (insert buf #\y)
    
    
    (labels ((update-text ()
	       ;;(format t "buffer contents ~a ~%" (buffer-contents buf))
	       (setq buf-str (buffer-contents buf))
	       (setq buf-len (length buf-str))
	       (when hello-text (sdl2:destroy-texture hello-text))
	       (setq hello-text nil)
	       (when (not (zerop (length (buffer-contents buf))))
		 ;; not sure about size of texture rectangle
		 ;; we only render to screen if buffer-contents is non zero length 
		 (setq hello-text (let* ((surface (sdl2-ttf:render-text-solid font
									      buf-str
									      255
									      255
									      255
									      0))
					 (texture (sdl2:create-texture-from-surface my-render
										    surface)))
				    (sdl2:free-surface surface)
				    texture))
		 (setq dest-rect (sdl2:make-rect (round (- 150 (/ (sdl2:texture-width hello-text) 2.0)))
						 (round (- 150 (/ (sdl2:texture-height hello-text) 2.0)))
						 (sdl2:texture-width hello-text)
						 (sdl2:texture-height hello-text))))))
      
      (sdl2:with-init (:everything)
	;;Technically speaking sdl2-ttf can be initialized without sdl2 
	(sdl2-ttf:init)
	(sdl2:with-window (the-window :title "Basic Font Example" :w 1024 :h 768 :flags '(:shown))
	  (sdl2:with-renderer (my-renderer the-window :flags '(:accelerated))
	    (setq my-render my-renderer)
            (setq font (sdl2-ttf:open-font (asdf:system-relative-pathname 'sdl2-ttf-examples "examples/PROBE_10PX_OTF.otf") 20))
	    (update-text)	  
            (flet ((text-renderer (renderer)
                     (sdl2:render-copy renderer
                                       hello-text
                                       :source-rect (cffi:null-pointer)
                                       :dest-rect dest-rect))
                   (clear-renderer (renderer)
                     (sdl2:set-render-draw-color renderer 0 0 0 255)
                     (sdl2:render-clear renderer)))
              (sdl2:with-event-loop (:method :poll)
	            (:keydown (:keysym keysym)
                      (let ((scancode (sdl2:scancode-value keysym))
                            (sym (sdl2:sym-value keysym))
                            (mod-value (sdl2:mod-value keysym)))
                        (cond
                          ((sdl2:scancode= scancode :scancode-w) (format t "~a~%" "WALK"))
                          ((sdl2:scancode= scancode :scancode-s) (sdl2:show-cursor))
                          ((sdl2:scancode= scancode :scancode-h) (sdl2:hide-cursor)))
                        (format t "Key sym: ~a, code: ~a, mod: ~a~%"
                                sym
                                scancode
                                mod-value)
			;; SDL_KMOD_LSHIFT 0x0001u   left shift = #x1
			(format t "Left shift pressed = ~a ~%" (logand #x1 mod-value))
			;; SDL_KMOD_RSHIFT 0x0001u   left shift = #x1			
			(format t "Right shift pressed = ~a ~%" (logand #x1 mod-value))
			

			))
		(:keyup
		 (:keysym keysym)
		 (cond
		   ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
		    (sdl2:push-event :quit))
		   ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-left)
		    (format t "user pressed left arrow key!~%")
		    (backward-char buf))
		   ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-right)
		    (format t "user pressed right arrow key!~%")
		    (forward-char buf))
		   ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-backspace)
		    (backspace-delete buf)
		    (format t "user pressed backspace key~%")
		    (update-text)
		    )
		   ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-delete)
		    (format t "user pressed delete key~%")
		    (delete-delete buf)
		    (update-text))

		   ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-space)
		    (format t "user pressed space key~%")
		    (insert buf #\space)
		    (update-text))
		   
		   ;; ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-a)
		   ;;  (insert buf #\a)
		   ;;  (format t "user pressed letter a key!~%")
		   ;;  (update-text))
		   
		   ;;(cheap-scancode #\a)
		   ((cheap-scancode-head #\a) (cheap-scancode-body #\a))
		   ((cheap-scancode-head #\b) (cheap-scancode-body #\b))
		   ((cheap-scancode-head #\c) (cheap-scancode-body #\c))
		   ((cheap-scancode-head #\d) (cheap-scancode-body #\d))
		   ((cheap-scancode-head #\e) (cheap-scancode-body #\e))
		   ((cheap-scancode-head #\f) (cheap-scancode-body #\f))
		   ((cheap-scancode-head #\g) (cheap-scancode-body #\g))
		   ((cheap-scancode-head #\h) (cheap-scancode-body #\h))
		   ((cheap-scancode-head #\i) (cheap-scancode-body #\i))
		   ((cheap-scancode-head #\j) (cheap-scancode-body #\j))
		   ((cheap-scancode-head #\k) (cheap-scancode-body #\k))
		   ((cheap-scancode-head #\l) (cheap-scancode-body #\l))
		   ((cheap-scancode-head #\m) (cheap-scancode-body #\m))
		   ((cheap-scancode-head #\n) (cheap-scancode-body #\n))
		   ((cheap-scancode-head #\o) (cheap-scancode-body #\o))
		   ((cheap-scancode-head #\p) (cheap-scancode-body #\p))
		   ((cheap-scancode-head #\q) (cheap-scancode-body #\q))
		   ((cheap-scancode-head #\r) (cheap-scancode-body #\r))
		   ((cheap-scancode-head #\s) (cheap-scancode-body #\s))
		   ((cheap-scancode-head #\t) (cheap-scancode-body #\t))
		   ((cheap-scancode-head #\u) (cheap-scancode-body #\u))
		   ((cheap-scancode-head #\v) (cheap-scancode-body #\v))
		   ((cheap-scancode-head #\w) (cheap-scancode-body #\w))
		   ((cheap-scancode-head #\x) (cheap-scancode-body #\x))
		   ((cheap-scancode-head #\y) (cheap-scancode-body #\y))
		   ((cheap-scancode-head #\z) (cheap-scancode-body #\z))
		   
		   (t nil)));;keyup
		(:idle ()
                       (clear-renderer my-render)
		       (when (> buf-len 0)		      
			 (text-renderer my-render))
                       (sdl2:render-present my-render))
		(:quit ()
                       (when (> (sdl2-ttf:was-init) 0)
			 (sdl2-ttf:close-font font)
			 (sdl2:destroy-texture hello-text)
			 (sdl2-ttf:quit))
                       t)))))))))


;; ai slop -- 
;; (:keydown (:keysym keysym)
;; 	  (let ((mods (sdl2:mod-value keysym)))
;; 	    (when (plusp (logand mods (sdl2:+kmod-lshift+)))
;; 	      (format t "Left shift is held~%"))
;; 	    (when (plusp (logand mods (sdl2:+kmod-rctrl+)))
;; 	      (format t "Right ctrl is held~%"))
;; 	    ;; Example: check for Ctrl + A
;; 	    (when (and (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-a)
;; 		       (plusp (logand mods (logior (sdl2:+kmod-lctrl+)
;; 						   (sdl2:+kmod-rctrl+)))))
;; 	      (format t "CTRL+A detected!~%"))))	      
