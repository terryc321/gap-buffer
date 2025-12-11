

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


;;glyph string derived from all characters from 32 to 126
;;(coerce (nreverse (let ((xs nil)) (loop for i from 32 to 126 do (setq xs (cons (code-char i) xs))) xs)) 'string)
;; width = 10 height = 22 advance = 10 
(defparameter *glyph-string* " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

(defun glyph-char (ch)
  "Return 0-based index of a printable ASCII character."
  (- (char-code ch) 32))

;; jetbrains mono
;; Width/height/advance = 10/22/10 (JetBrains Mono 20pt monospace)."
(defparameter *glyph-width* 10)
(defparameter *glyph-height* 22)
(defparameter *glyph-advance* 10)

(defun glyph-rect (ch)
  "Return an SDL_Rect for a character in a fixed-width glyph atlas."
  (let ((x (* *glyph-width* (glyph-char ch)))
	(y 0)
	(wid *glyph-width*)
	(hgt *glyph-height*))
    (sdl2:make-rect x y wid hgt)))



(defun draw-text (renderer text x y texture)
  "Draw a string using the pre-rendered glyph texture.
uses *glyph-advance* to keep track of position across screen x direction*"
  (loop for ch across text do
       (when (and (>= (char-code ch) 32) (<= (char-code ch) 126))
         (let ((src (glyph-rect ch))
               (dst (sdl2:make-rect x y *glyph-width* *glyph-height*)))
           (sdl2:render-copy renderer texture
                             :source-rect src
                             :dest-rect dst)
           (incf x *glyph-advance*)))))



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


;; (defmacro cheap-scancode (ch)
;;   (let ((sym (intern (string-upcase (format nil "SCANCODE-~a" ch)) "KEYWORD")))
;;     `((sdl2:scancode= (sdl2:scancode-value keysym) ,sym)
;;       (insert buf ,ch)
;;       (format t "user pressed letter ~a key!~%" ,ch)
;;       (update-text))))

;;(cheap-scancode #\a)

(defmacro cheap-scancode-head (ch)
  (let ((sym (intern (string-upcase (format nil "SCANCODE-~a" ch)) "KEYWORD")))
    `(sdl2:scancode= (sdl2:scancode-value keysym) ,sym)))

(defmacro cheap-scancode-body (ch)
  `(progn
     (insert buf ,ch)
     ;;(format t "user pressed letter ~a key!~%" ,ch)
     ;;(update-text)
     ))

(defmacro cheap-keytest(ch ch2)
    (let ((sym (intern (string-upcase (format nil "SCANCODE-~a" ch)) "KEYWORD")))
      `(when (sdl2:scancode= scancode ,sym)
	 (cond
	   ((zerop (logand (logior #x1 #x2) mod-value))
	    ;; no shift key pressed
	    (cheap-scancode-body ,ch))
	   (t ;; shift key pressed
	    (cheap-scancode-body ,ch2))))))
			

;; (defun update-text ()
;;   ;; Render a string at position (50,50)
;; (loop for ch across "Hello, SDL2!"
;;       for x = 50 then (+ x 10)
;;       do (sdl2:render-copy renderer glyph-texture
;;                            :source-rect (glyph-rect ch)
;;                            :dest-rect (sdl2:make-rect :x x :y 50 :w 10 :h 22))))



;; hello-text is a sdl2 texture
(defun basic-example ()
  (let ((buf (make-buffer))
	(buf-len 6)
	(buf-str nil)
	;;(my-render nil)
	;;(dest-rect nil)
	(hello-tex nil)
	(glyph-tex nil) ;; characters in one big texture	
	(font nil))
    (insert buf #\t)
    (insert buf #\e)
    (insert buf #\r)
    (insert buf #\r)
    (insert buf #\y)
    
    
      (sdl2:with-init (:everything)
	;;Technically speaking sdl2-ttf can be initialized without sdl2 
	(sdl2-ttf:init)
	(sdl2:with-window (the-window :title "Basic Font Example" :w 1024 :h 768 :flags '(:shown))
	  (sdl2:with-renderer (my-renderer the-window :flags '(:accelerated))
	    ;; (setq my-render my-renderer)

	    ;;(setq font (sdl2-ttf:open-font (asdf:system-relative-pathname 'sdl2-ttf-examples "examples/PROBE_10PX_OTF.otf") 20))
	    ;;(setq font (sdl2-ttf:open-font "/usr/share/fonts/fonts-go/Go-Mono.ttf" 20))
	    (setq font (sdl2-ttf:open-font "/usr/share/fonts/truetype/jetbrains-mono/fonts/ttf/JetBrainsMono-Regular.ttf" 20))
	    ;; create  glyph texture
	    (setq glyph-tex (let* ((surface (sdl2-ttf:render-text-solid font
									      *glyph-string*
									      255  ;; red
									      255  ;; green
									      255  ;; blue
									      0))
					 (texture (sdl2:create-texture-from-surface my-renderer
										    surface)))
				    (sdl2:free-surface surface)
				    texture))
		 
	    ;;(update-text)	  
            (flet ((text-renderer (renderer)

		     ;; show all characters in texture -- ie the whole texture glyph-tex
		     (sdl2:render-copy renderer glyph-tex
                             :source-rect (cffi:null-pointer)
                             :dest-rect (sdl2:make-rect 50 50
							(* (+ 1(length *glyph-string*)) (* 2 *glyph-width*))
							(* 2 *glyph-height*)))
		     
		     (let* ((buf-str (buffer-contents buf))
			    (buf-len (length buf-str))
			    (x 50)
			    (y 100))
		       (loop for i from 0 to (+ -1 buf-len) do
			 (let ((ch (char buf-str i)))
			   (sdl2:render-copy renderer glyph-tex
                             :source-rect (sdl2:make-rect (* 12 (- (char-code ch) 32)) 0 (+ 2 *glyph-width*) (+ 4 *glyph-height*))
                             :dest-rect (sdl2:make-rect x y
							(* 2 *glyph-width*)
							(* 2 *glyph-height*)))
			   (incf x (* 2 *glyph-advance*))))))
		   ;; 
		   (clear-renderer (renderer)
		     ;; background colour in red green blue
                     (sdl2:set-render-draw-color renderer 0 50 50 255)
                     (sdl2:render-clear renderer)))
              (sdl2:with-event-loop (:method :poll)
	            (:keydown (:keysym keysym)
                      (let ((scancode (sdl2:scancode-value keysym))
                            (sym (sdl2:sym-value keysym))
                            (mod-value (sdl2:mod-value keysym)))
                        (cond
                          ((sdl2:scancode= scancode :scancode-w) (format t "~a~%" "WALK"))
                          ((sdl2:scancode= scancode :scancode-s) (sdl2:show-cursor))
                          ((sdl2:scancode= scancode :scancode-h) (sdl2:hide-cursor))
			  ;; fUNCTION kEYSs - woop woop 
			  ((sdl2:scancode= scancode :scancode-f1)
			   (format t "FORMULA ONEE ........ FORMULA ONE  brrrUUUMMMM...~%"))
			  )

			;; ;; TODO TODO 
			;; (when (sdl2:scancode= scancode :scancode-a)
			;;   (cond
			;;     ((zerop (logand (logior #x1 #x2) mod-value))
			;;       ;; no shift key pressed
			;;       (cheap-scancode-body #\a))
			;;     (t ;; shift key pressed
			;;      (cheap-scancode-body #\A))))
			(cheap-keytest #\a #\A)
			(cheap-keytest #\b #\B)
			(cheap-keytest #\c #\C)
			(cheap-keytest #\d #\D)
			(cheap-keytest #\e #\E)
			(cheap-keytest #\f #\F)
			(cheap-keytest #\g #\G)
			(cheap-keytest #\h #\H)
			(cheap-keytest #\i #\I)
			(cheap-keytest #\j #\J)
			(cheap-keytest #\k #\K)
			(cheap-keytest #\l #\L)
			(cheap-keytest #\m #\M)
			(cheap-keytest #\n #\N)
			(cheap-keytest #\o #\O)
			(cheap-keytest #\p #\P)
			(cheap-keytest #\q #\Q)
			(cheap-keytest #\r #\R)
			(cheap-keytest #\s #\S)
			(cheap-keytest #\t #\T)
			(cheap-keytest #\u #\U)
			(cheap-keytest #\v #\V)
			(cheap-keytest #\w #\W)
			(cheap-keytest #\x #\X)
			(cheap-keytest #\y #\Y)
			(cheap-keytest #\z #\Z)

			(cheap-keytest #\0 #\])
			(cheap-keytest #\1 #\!)
			(cheap-keytest #\2 #\@)
			(cheap-keytest #\3 #\#)
			(cheap-keytest #\4 #\$)
			(cheap-keytest #\5 #\%)
			(cheap-keytest #\6 #\^)
			(cheap-keytest #\7 #\&)
			(cheap-keytest #\8 #\*)
			(cheap-keytest #\9 #\[)

			
			(when (sdl2:scancode= scancode :scancode-leftbracket)
			  (cond
			    ((zerop (logand (logior #x1 #x2) mod-value)) ;;no shift key
			     (cheap-scancode-body #\( ))
			    (t ;; shift key pressed
			     (cheap-scancode-body #\{ ))))

			(when (sdl2:scancode= scancode :scancode-rightbracket)
			  (cond
			    ((zerop (logand (logior #x1 #x2) mod-value)) ;;no shift key
			     (cheap-scancode-body #\) ))
			    (t ;; shift key pressed
			     (cheap-scancode-body #\} ))))

			(when (sdl2:scancode= scancode :scancode-apostrophe)
			  (cond
			    ((zerop (logand (logior #x1 #x2) mod-value)) ;;no shift key
			     (cheap-scancode-body #\' ))
			    (t ;; shift key pressed
			     (cheap-scancode-body #\" ))))

			(when (sdl2:scancode= scancode :scancode-semicolon)
			  (cond
			    ((zerop (logand (logior #x1 #x2) mod-value)) ;;no shift key
			     (cheap-scancode-body #\; ))
			    (t ;; shift key pressed
			     (cheap-scancode-body #\: ))))
			
			(when (sdl2:scancode= scancode :scancode-period)
			  (cond
			    ((zerop (logand (logior #x1 #x2) mod-value)) ;;no shift key
			     (cheap-scancode-body #\. ))
			    (t ;; shift key pressed
			     (cheap-scancode-body #\> ))))
			
			(when (sdl2:scancode= scancode :scancode-comma)
			  (cond
			    ((zerop (logand (logior #x1 #x2) mod-value)) ;;no shift key
			     (cheap-scancode-body #\, ))
			    (t ;; shift key pressed
			     (cheap-scancode-body #\< ))))
			
			(when (sdl2:scancode= scancode :scancode-slash)
			  (cond
			    ((zerop (logand (logior #x1 #x2) mod-value)) ;;no shift key
			     (cheap-scancode-body #\/ ))
			    (t ;; shift key pressed
			     (cheap-scancode-body #\? ))))

			(when (sdl2:scancode= scancode :scancode-backslash)
			  (cond
			    ((zerop (logand (logior #x1 #x2) mod-value)) ;;no shift key
			     (cheap-scancode-body #\\ ))
			    (t ;; shift key pressed
			     (cheap-scancode-body #\| ))))

			(when (sdl2:scancode= scancode :scancode-equals)
			  (cond
			    ((zerop (logand (logior #x1 #x2) mod-value)) ;;no shift key
			     (cheap-scancode-body #\= ))
			    (t ;; shift key pressed
			     (cheap-scancode-body #\+ ))))

			(when (sdl2:scancode= scancode :scancode-minus)
			  (cond
			    ((zerop (logand (logior #x1 #x2) mod-value)) ;;no shift key
			     (cheap-scancode-body #\- ))
			    (t ;; shift key pressed
			     (cheap-scancode-body #\_ ))))

			(when (sdl2:scancode= scancode :scancode-grave)
			  (cond
			    ((zerop (logand (logior #x1 #x2) mod-value)) ;;no shift key
			     (cheap-scancode-body #\` ))
			    (t ;; shift key pressed
			     (cheap-scancode-body #\~ ))))

			(when (sdl2:scancode= scancode :scancode-printscreen)
			  (format t "printscreen pressed~%"))


			(when (sdl2:scancode= scancode :scancode-numlockclear)
			  (format t "numlockclear pressed~%")
			  )

			(when (sdl2:scancode= scancode :scancode-kp-divide)
			  (format t "keypad divide pressed~%")
			  )

			(when (sdl2:scancode= scancode :scancode-kp-multiply)
			  (format t "keypad  multiply pressed~%")
			  )

			(when (sdl2:scancode= scancode :scancode-kp-minus)
			  (format t "keypad minus pressed~%")
			  )

			(when (sdl2:scancode= scancode :scancode-kp-plus)
			  (format t "keypad plus pressed~%")
			  )

			(when (sdl2:scancode= scancode :scancode-kp-enter)
			  (format t "keypad enter pressed~%")
			  )


			(when (sdl2:scancode= scancode :scancode-up)
			  (format t "up arrow pressed~%")
			  )

			(when (sdl2:scancode= scancode :scancode-down)
			  (format t "down arrow pressed~%")
			  )

			(when (sdl2:scancode= scancode :scancode-left)
			  (format t "left arrow pressed~%")
			  )

			(when (sdl2:scancode= scancode :scancode-right)
			  (format t "right arrow pressed~%")
			  )

			(when (sdl2:scancode= scancode :scancode-kp-0)
			  (format t "keypad 0 pressed ~%")
			  )

			(when (sdl2:scancode= scancode :scancode-kp-1)
			  (format t "keypad 1 pressed~%")
			  )

			(when (sdl2:scancode= scancode :scancode-kp-2)
			  (format t "keypad 2 pressed~%")
			  )

			(when (sdl2:scancode= scancode :scancode-kp-3)
			  (format t "keypad 3 pressed~%")
			  )

			(when (sdl2:scancode= scancode :scancode-kp-4)
			  (format t "keypad 4 pressed~%")
			  )

			(when (sdl2:scancode= scancode :scancode-kp-5)
			  (format t "keypad 5 pressed ~%")
			  )

			(when (sdl2:scancode= scancode :scancode-kp-6)
			  (format t "keypad 6 pressed~%")
			  )

			(when (sdl2:scancode= scancode :scancode-kp-7)
			  (format t "keypad 7 pressed~%")
			  )

			(when (sdl2:scancode= scancode :scancode-kp-8)
			  (format t "keypad 8 pressed~%")
			  )

			(when (sdl2:scancode= scancode :scancode-kp-9)
			  (format t "keypad 9 pressed~%")
			  )
			
			;; (when (sdl2:scancode= scancode :scancode-numlockclear)
			;;   (format t "numlockclear pressed~%"))
			
			(when (sdl2:scancode= scancode :scancode-f1)
			  (format t "f1 pressed~%"))
			(when (sdl2:scancode= scancode :scancode-f2)
			  (format t "f2 pressed~%"))
			(when (sdl2:scancode= scancode :scancode-f3)
			  (format t "f3 pressed~%"))
			(when (sdl2:scancode= scancode :scancode-f4)
			  (format t "f4 pressed~%"))
			(when (sdl2:scancode= scancode :scancode-f5)
			  (format t "f5 pressed~%"))
			(when (sdl2:scancode= scancode :scancode-f6)
			  (format t "f6 pressed~%"))
			(when (sdl2:scancode= scancode :scancode-f7)
			  (format t "f7 pressed~%"))
			(when (sdl2:scancode= scancode :scancode-f8)
			  (format t "f8 pressed~%"))
			(when (sdl2:scancode= scancode :scancode-f9)
			  (format t "f9 pressed~%"))
			(when (sdl2:scancode= scancode :scancode-f10)
			  (format t "f10 pressed~%"))
			(when (sdl2:scancode= scancode :scancode-f11)
			  (format t "f11 pressed~%"))
			(when (sdl2:scancode= scancode :scancode-f12)
			  (format t "f12 pressed~%"))

			(when (sdl2:scancode= scancode :scancode-kp-period)
			  (format t "keypad period pressed~%"))

			(when (sdl2:scancode= scancode :scancode-insert)
			  (format t "insert pressed~%"))

			(when (sdl2:scancode= scancode :scancode-home)
			  (format t "home pressed~%"))

			(when (sdl2:scancode= scancode :scancode-pageup)
			  (format t "pageup pressed~%"))
			
			(when (sdl2:scancode= scancode :scancode-pagedown)
			  (format t "pagedown pressed~%"))

			(when (sdl2:scancode= scancode :scancode-end)
			  (format t "end pressed~%"))

			(when (sdl2:scancode= scancode :scancode-delete)
			  (format t "delete pressed~%"))
			
			
			(when (sdl2:scancode= scancode :scancode-return)
			  "maybe insert a newline?"
                           (cheap-scancode-body #\return))
			
                        (format t "Key sym: ~a, code: ~a, mod: ~a, keyword: ~a~%"
                                sym
                                scancode
                                mod-value
				(sdl2:scancode-symbol scancode)
				)
			
			;; (format t "left shift key pressed => ~a~%" (logand #x1 mod-value))
			;; (format t "right shift key pressed => ~a~%" (logand #x2 mod-value))
			;; (format t "any shift key (left or right) key pressed => ~a~%" (logand (logior #x1 #x2) mod-value))

			;; (format t "left control key pressed => ~a~%" (logand #x40 mod-value))
			;; (format t "right control key pressed => ~a~%" (logand #x80 mod-value))
			;; (format t "any control key (left or right) pressed => ~a~%" (logand (logior #x40 #x80) mod-value))

		   (cond
		   ((sdl2:scancode= scancode :scancode-left)
		    ;;(format t "user pressed left arrow key!~%")
		    (backward-char buf))
		   ((sdl2:scancode= scancode :scancode-right)
		    ;;(format t "user pressed right arrow key!~%")
		    (forward-char buf))
		   ((sdl2:scancode= scancode :scancode-backspace)
		    (backspace-delete buf)
		    ;;(format t "user pressed backspace key~%")
		    ;;(update-text)
		    )
		   ((sdl2:scancode= scancode :scancode-delete)
		    ;;(format t "user pressed delete key~%")
		    (delete-delete buf)
		    ;;(update-text)
		    )

		   ((sdl2:scancode= scancode :scancode-space)
		    ;;(format t "user pressed space key~%")
		    (insert buf #\space)
		    ;;(update-text)
		    )

		   ((sdl2:scancode= scancode :scancode-escape)
		    (sdl2:push-event :quit))
		   
		   ;; ((sdl2:scancode= scancode :scancode-a)
		   ;;  (insert buf #\a)
		   ;;  (format t "user pressed letter a key!~%")
		   ;;  (update-text))
			)))
			  
			
		    (:keyup
		     (:keysym keysym)
		     ;; useful scancode 
		     (let ((scancode (sdl2:scancode-value keysym))
                           (sym (sdl2:sym-value keysym))
                           (mod-value (sdl2:mod-value keysym)))
                      
		 (cond
		   ((sdl2:scancode= scancode :scancode-escape)
		    (sdl2:push-event :quit))
		   (t nil))))
		(:idle ()
                       (clear-renderer my-renderer)
		       (when (> buf-len 0)		      
			 (text-renderer my-renderer))
                       (sdl2:render-present my-renderer))
		(:quit ()
                       (when (> (sdl2-ttf:was-init) 0)
			 (sdl2-ttf:close-font font)
			 (sdl2:destroy-texture hello-tex)
			 (sdl2-ttf:quit))
                       t))))))))


;; to step around bordeaux-threads pain points
(defun run ()
  (sdl2:make-this-thread-main #'basic-example))


;; ai slop -- 
;; (:keydown (:keysym keysym)
;; 	  (let ((mods (sdl2:mod-value keysym)))
;; 	    (when (plusp (logand mods (sdl2:+kmod-lshift+)))
;; 	      (format t "Left shift is held~%"))
;; 	    (when (plusp (logand mods (sdl2:+kmod-rctrl+)))
;; 	      (format t "Right ctrl is held~%"))
;; 	    ;; Example: check for Ctrl + A
;; 	    (when (and (sdl2:scancode= scancode :scancode-a)
;; 		       (plusp (logand mods (logior (sdl2:+kmod-lctrl+)
;; 						   (sdl2:+kmod-rctrl+)))))
;; 	      (format t "CTRL+A detected!~%"))))	      
