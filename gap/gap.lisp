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

(defun make-buffer (len)
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
    ;; copy live chars across
    (let ((j 0))
      (loop for i from 0 to (+ -1 (length (buf-vec buf))) do
	(let ((ch (aref (buf-vec buf) i)))
	  (when (not (null ch))
	    (setf (aref (buf-vec tmp) j) ch)
	    (incf j))))
      (setf (buf-from tmp) j)
      (setf (buf-to tmp) (+ -1 (length (buf-vec tmp))))
      ;; original gap buffer location ? now at j
      
      
    t))

    
    ;; (loop for i from 0 to (+ -1 (length (buf-vec buf))) do
    ;;   (let ((ch (aref (buf-vec buf) i)))
    ;; 	(when (not (null ch))
    ;; 	  (format t "copying char ~a from ~a -> ~a ~%" ch i i)
    ;; 	  (setf (aref (buf-vec tmp) j) ch)
    ;; 	  (incf j))))
    ;; ;; assign vectors 
    ;; (setf (buf-vec buf) (buf-vec tmp))
    ;; ;;
    ;; (format t "inserting char ~a at ~a " ch j)
    ;; (setf (aref (buf-vec tmp) j) ch)
    ;; (setf (buf-from buf) (+ j 1))
    ;; (setf (buf-to buf) (+ -1 newsize))
    ;; ;;
    ;; (format t "buffer contents {~A} ~%" (buffer-contents buf))
    ;; ))


    

;; nil means no character stored there
(defun buffer-contents (buf) "")

  ;; (let ((str "")
  ;; 	(arr (buf-vec buf)))
  ;;   (loop for i from 0 to (- (length arr) 1) do
  ;;     (let ((ch (aref arr i)))
  ;;     (when (not (null ch))
  ;; 	(setq str (concatenate 'string str (format nil "~a" ch))))))
  ;;   str))

(defun remov (buf)
  t)

(defun forward (buf)
  t)

(defun backward (buf)
  t)

(defun up(buf)
  t)

(defun down (buf)
  t)



(tt:def-suite suite-insert)
(tt:in-suite suite-insert)
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
    (insert buf 1)
    (insert buf 2)
    (insert buf 3)    
    (insert buf 4)
    (insert buf 5)
    (insert buf 6)    
    (insert buf 7)
    (insert buf 8)
    (insert buf 9)    
    (tt:is (equalp "123456789" (buffer-contents buf)))))


(defun run-tests ()
  (tt:run!))

