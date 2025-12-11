;;;; ------------------------------------------------------------
;;;; Minimal Glyph Atlas Example using CL-SDL2 + cl-sdl2-ttf
;;;; ------------------------------------------------------------

;; (ql:quickload :sdl2)
;; (ql:quickload :sdl2-ttf)

(in-package :gap)

;;;; Data structure for glyph info
(defstruct glyph
  src-rect     ;; SDL_Rect source rect inside atlas
  advance      ;; horizontal advance (int)
  minx         ;; offset x
  maxy         ;; offset y baseline
  width
  height)


(defparameter *texture-access-target* 2)
(defparameter *blendmode-blend* #x1)  ;; from SDL_blendmode.h

(defparameter *font-path* "/usr/share/fonts/truetype/jetbrains-mono/JetBrainsMono-Regular.ttf")
(defparameter *font-size* 16)
(defparameter *atlas-width* 512)
(defparameter *atlas-height* 512)

(defparameter *glyphs* (make-hash-table))

(defun load-font ()
  (ttf-open-font *font-path* *font-size*))

(defun create-atlas-texture (renderer)
  (let ((tex (sdl2:create-texture
              renderer
              sdl2:+pixelformat-rgba8888+
              *texture-access-target*
              *atlas-width* *atlas-height*)))
    (set-texture-blend-mode tex *blendmode-blend*)
    tex))




(defun build-glyph-atlas (renderer font)
  (let ((atlas (create-atlas-texture renderer)))
    ;; draw glyphs into atlas
    (set-render-target renderer atlas)
    (render-clear renderer)

    (let ((x 0)
          (y 0)
          (row-height 0))
      (loop for ch from 32 to 126 do
           (multiple-value-bind (w h) (SDL2-FFI.FUNCTIONS:TTF-GLYPH-METRICS font ch)
             ;; Render glyph surface
             (let* ((surface (sdl2-ffi.functions:ttf-render-glyph-blended font ch '(255 255 255 255)))
                    (texture (sdl2:create-texture-from-surface renderer surface))
                    (gw (sdl2:surface-width surface))
                    (gh (sdl2:surface-height surface)))

               ;; Move to next row if needed
               (when (> (+ x gw) *atlas-width*)
                 (setf x 0
                       y (+ y row-height)
                       row-height 0))

               ;; Draw glyph into atlas
               (let ((dst (sdl2:make-rect :x x :y y :w gw :h gh)))
                 (sdl2:render-copy renderer texture :dest-rect dst)

                 ;; Save glyph info
                 (setf (gethash ch *glyphs*)
                       (make-glyph
                        :src-rect dst
                        :advance w
                        :width gw
                        :height gh)))

               ;; Update atlas cursor
               (incf x gw)
               (setf row-height (max row-height gh))

               (free-surface surface)
               (destroy-texture texture)))))

    (set-render-target renderer nil)
    atlas))

(defun draw-text (renderer atlas string x y)
  (loop for ch across string
        for glyph = (gethash (char-code ch) *glyphs*) do
          (when glyph
            (let* ((src (glyph-src-rect glyph))
                   (dst (make-rect
                         :x x
                         :y y
                         :w (glyph-width glyph)
                         :h (glyph-height glyph))))
              (render-copy renderer atlas :source-rect src :dest-rect dst)
              (incf x (glyph-advance glyph))))))

(defun run ()
  (sdl2:with-init (:video)
    (ttf-init)
    (let* ((window (sdl2:create-window "CL-SDL2 Text Atlas"
                                       :x :centered :y :centered
                                       :w 800 :h 600
                                       :flags '(:shown :opengl)))
           (renderer (sdl2:create-renderer window -1 '(:accelerated :presentvsync)))
           (font (load-font))
           (atlas (build-glyph-atlas renderer font)))
      (unwind-protect
           (loop with quit = nil
                 until quit do
                   (sdl2:while-event (ev)
                     (when (typep ev 'sdl2:quit-event)
                       (setf quit t)))

                   (render-clear renderer)

                   ;; Draw sample text
                   (draw-text renderer atlas "Hello, SDL2 Atlas!" 50 50)

                   (render-present renderer))
        ;; cleanup
        (ttf-close-font font)
        (destroy-texture atlas)
        (destroy-renderer renderer)
        (destroy-window window)
        (ttf-quit)))))

