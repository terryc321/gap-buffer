;;;; gap.lisp

(ql:quickload "uiop") ;; directory stuff
(ql:quickload "fiveam") ;; testing suite
;; (load "gap.lisp")
;; to do tests ... (run-tests)
;;

#|


gap buffer
initial buffer in a text editor will be empty - the entire file is the gap-buffer 
in essence there is no 'text' in the buffer

how big should the gap buffer be ?

common lisp array 0 to length-1
the perenial problem of the off by errors abound !

unicode characters - multibyte characters - now not just a single ascii character

lets forget unicode for now.

lets forget binary code for now - just visible ascii characters ?

suppose entire memory of text editor consists of this 
0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 ... (LEN-1)

smallest gap buffer we can have is one byte gap buffer ? - thats ready for text

deleting a character -
enlarge gap buffer by one - simply 

inserting a character -
consume one byte - theirby reducing gap buffer size by one

moving down one line -

moving up one line -

forward one character - 

backward one character - 

caret -
open file in buffer the caret is at the start of 'text'
the gap buffer is before all the 'text'

caret position is BETWEEN characters - so if caret is at byte offset 0 , it is 'BEFORE'
the character in 'text' at byte offset 0 

CARET   {FIRST-BYTE}  VIRTUAL-CARET-POS  {SECOND-BYTE}  ANOTHER-CARET-POS 
^                   ^                                   ^

column numbering -
column = 1

row numbering -
row = 1

what does it mean to be at row 0 ? or column 0 ?
why not row -3 ? column -4 ?
row 2+3i ?
row blue ?

when moving caret or cursor around - keep 'view' caret within visible region

extra large files

position of open/closing braces - aka paredit

keyboard input/output - control keys alt super modifiers -
how many keys can be detected by being down
can we use a window manager that simply does not reference






|#


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

(defun make-buffer ()
  (make-buf :vec (make-array 10 :initial-element nil) :from 3 :to 4))

;; is buffer empty? - no 
(defun insert-no-expand(buf ch)
  (format t "buffer before => ~a ~%" buf)
  (setf (aref (buf-vec buf) (buf-from buf)) ch)
  (incf (buf-from buf))
  (format t "buffer now => ~a ~%" buf))

#|

0 1 2 3 4 5 6 7 8 9
a b c 

|#

;; copies all string - then sets to/from indexes after word - always inserts at end of text !?!
(defun insert-with-expand(buf ch)
  (format t "EXPANDING~%")
  (let* ((newsize (+ 1 (length (buf-vec buf))))
	 (j 0)
	 (tmp (make-buf :vec (make-array newsize :initial-element nil) :from 0 :to 0)))
    (loop for i from 0 to (+ -1 (length (buf-vec buf))) do
      (let ((ch (aref (buf-vec buf) i)))
	(when (not (null ch))
	  (format t "copying char ~a from ~a -> ~a ~%" ch i i)
	  (setf (aref (buf-vec tmp) j) ch)
	  (incf j))))
    ;; assign vectors 
    (setf (buf-vec buf) (buf-vec tmp))
    ;;
    (format t "inserting char ~a at ~a " ch j)
    (setf (aref (buf-vec tmp) j) ch)
    (setf (buf-from buf) (+ j 1))
    (setf (buf-to buf) (+ -1 newsize))
    ;;
    (format t "buffer contents {~A} ~%" (buffer-contents buf))
    ))





(defun insert(buf ch)
  (cond
    ((< (buf-from buf) (buf-to buf))
     (insert-no-expand buf ch))
    (t
     (insert-with-expand buf ch))))
    

;; nil means no character stored there
(defun buffer-contents (buf)
  (let ((str "")
	(arr (buf-vec buf)))
    (loop for i from 0 to (- (length arr) 1) do
      (let ((ch (aref arr i)))
      (when (not (null ch))
	(setq str (concatenate 'string str (format nil "~a" ch))))))
    str))

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

