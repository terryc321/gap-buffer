;;;; gap.lisp

(ql:quickload "uiop") ;; directory stuff
(ql:quickload "fiveam") ;; testing suite
;; (load "gap.lisp")
;; to do tests ... (run-tests)
;;



(in-package #:gap)

(uiop:add-package-local-nickname :tt :fiveam )



;; ** common lisp problems 
;; how do we rename symbols to prevent name clash
;; remove 

;; an initial buffer
;; what makes up a buffer ?
;; how track line column ?

(defstruct buf
  vec
  from
  to
  )

(defun make-buffer (&optional (len 10))
  (assert (>= len 10))
  (make-buf :vec (make-array len :initial-element nil)
	    :from 0
	    :to (- len 1)))


(defun insert(buf ch)
  (cond
    ((>= (buf-from buf) (buf-to buf))
     (expand-gap-buffer buf)
     (insert-no-expand buf ch))
    (t  (insert-no-expand buf ch))))


(defun insert-no-expand(buf ch)  
  (setf (aref (buf-vec buf) (buf-from buf)) ch)
  (incf (buf-from buf)))


(defun expand-gap-buffer(buf)  
  (let* ((newsize (* 2 (length (buf-vec buf))))
	 (tmp (make-buffer newsize)))
    ;; copy chars between (zero) to (old-gap-buffer-start - 1) across
    (let ((j 0))
      (loop for i from 0 to (+ -1 (buf-from buf)) do
	(let ((ch (aref (buf-vec buf) i)))
	  (when (not (null ch))
	    (setf (aref (buf-vec tmp) j) ch)
	    (incf j))))
      ;; copy down from right to left
      (let ((k (+ -1 (length (buf-vec tmp)))))
	(loop for n from (+ -1 (length (buf-vec buf))) downto (+ 1 (buf-to buf)) do
	  (let ((ch (aref (buf-vec buf) n)))
	    (when (not (null ch))
	      (setf (aref (buf-vec tmp) k) ch)
	      (decf k))))      
      ;; set gap buffer markers at end of tmp array
      (setf (buf-from tmp) (buf-from buf))
      (setf (buf-to tmp) k)

      ;; nullify gap buffer
      (loop for r from (buf-from tmp) to (buf-to tmp) do
	(setf (aref (buf-vec tmp) r) nil))

      ;; now take over buf
      ;; since buf is a structure we can mutate it inplace
      ;; any future reference to buf will reflect the new mutated version
      (setf (buf-from buf) (buf-from tmp))
      (setf (buf-to buf) (buf-to tmp))
      (setf (buf-vec buf) (buf-vec tmp))
      t))))

    
(defun buffer-contents (buf) 
  (let ((str "")
	(arr (buf-vec buf)))
    (loop for i from 0 to (- (length arr) 1) do
      (let ((ch (aref arr i)))
      (when (not (null ch))
	(setq str (concatenate 'string str (format nil "~a" ch))))))
    str))


(defun backspace-delete (buf)
  (setf (buf-from buf) (max 0 (+ -1 (buf-from buf))))
  (setf (aref (buf-vec buf) (buf-from buf)) nil))

(defun delete-delete (buf)
  (setf (buf-to buf) (min (+ -1 (length (buf-vec buf)))
			  (+ 1 (buf-to buf))))
  (setf (aref (buf-vec buf) (buf-to buf)) nil))



;; (defun remov (buf)
;;   t)

;; (defun forward (buf)
;;   t)

;; (defun backward (buf)
;;    t)

;; (defun up(buf)
;;   t)

;; (defun down (buf)
;;   t)



(tt:def-suite suite-insert)
(tt:in-suite suite-insert)

(tt:test insert-nothing
  (let ((buf (make-buffer)))
    (tt:is (equalp "" (buffer-contents buf)))))

(tt:test insert-a
  (let ((buf (make-buffer)))
    (insert buf #\a)
    (tt:is (equalp "a" (buffer-contents buf)))))

(tt:test insert-ab
  (let ((buf (make-buffer)))
    (insert buf #\a)
    (insert buf #\b)    
    (tt:is (equalp "ab" (buffer-contents buf)))))

(tt:test insert-abc
  (let ((buf (make-buffer)))
    (insert buf #\a)
    (insert buf #\b)
    (insert buf #\c)    
    (tt:is (equalp "abc" (buffer-contents buf)))))

(tt:test insert-abc
  (let ((buf (make-buffer)))
    (insert buf #\a)
    (insert buf #\b)
    (insert buf #\c)    
    (insert buf #\d)
    (insert buf #\e)
    (insert buf #\f)    
    (insert buf #\g)
    (insert buf #\h)
    (insert buf #\i)    
    (tt:is (equalp "abcdefghi" (buffer-contents buf)))))

(tt:test insert-n1
  (let ((buf (make-buffer)))
    (let ((str ""))
      (loop for i from 1 to 100 do 
	(insert buf #\a)
	(setq str (concatenate 'string str "a")))      
    (tt:is (equalp str (buffer-contents buf))))))

(defun run-tests ()
  (tt:run!))

