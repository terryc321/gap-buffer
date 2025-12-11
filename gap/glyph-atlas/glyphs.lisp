

;; Read the glyph data from a file
(defparameter *glyphs-data*
  (with-open-file (in "glyphs.scm")
    (read in)))

;; Convert to a hash table keyed by character
(defparameter *glyphs-table* (make-hash-table))

(dolist (g *glyphs-data*)
  (let ((char (second g))  ;; #\a, #\b, etc.
        (props (rest (cddr g))))  ;; (:x X :y Y :w W :h H :advance A)
    (setf (gethash char *glyphs-table*) props)))

;; Access example:
(gethash #\A *glyphs-table*)
;; => (:x 330 :y 0 :w 10 :h 22 :advance 10)

