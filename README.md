
# gap buffer v0.01

## gap buffer structure 

first approximation we consider a simple byte array to represent text with a gap buffer somewhere inside.  Initially the buffer is completely empty , there is no content if the buffer were to be saved to a file.

```lisp
(defstruct buf
  vec
  from
  to
  )
```

## make-buffer

lets be able to create an array of some arbitrary positive size with two markers `to` and `from` that represent where the gap buffer starts and ends in the array.

we can even make the default buffer size relatively small , perhaps even as short as 2 bytes long.

```lisp
(defun make-buffer (len)
  (assert (>= len 10))
  (make-buf :vec (make-array len :initial-element nil)
	    :from 0
	    :to (- len 1)))
```

## buffer-contents

A quick and dirty way to get a string representation of the buffer itself

```lisp
(defun buffer-contents (buf) 
  (let ((str "")
	(arr (buf-vec buf)))
    (loop for i from 0 to (- (length arr) 1) do
      (let ((ch (aref arr i)))
      (when (not (null ch))
	(setq str (concatenate 'string str (format nil "~a" ch))))))
    str))
```


## insert character

lets now look at inserting a character into a the gap buffer.  two cases . 

- gap buffer needs to be enlarged
- gap buffer ready for insert character

```lisp
(defun insert(buf ch)
  (cond
    ((>= (buf-from buf) (buf-to buf))
     (expand-gap-buffer buf)
     (insert-no-expand buf ch))
    (t  (insert-no-expand buf ch))))
```

### insert-no-expand

```lisp
(defun insert-no-expand(buf ch)  
  (setf (aref (buf-vec buf) (buf-from buf)) ch)
  (incf (buf-from buf)))
```

### expand-gap-buffer

here we enlarge the gap buffer.  for simplicity we will just double the size of the array container itself.   
we only expand no insertion of characters here.

- double size array container
- copy all live chars on left of old gap buffer over to new array
- copy all live chars on right of old gap buffer over to new array
- set new limits of tmp gap buffer
- nullify tmp gap buffer contents
- take over old buffer structure itself

```lisp

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
	      (decf k)))))      
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
      t)))
```

# backspace delete

deletes the chracter before the caret.  repeatedly smashing the backspace key when already at start of document will do nothing.

move gap buffer left if possible , if gap buffer `from` is already at 0 then keep it at 0 . 
write `nil` making start of gap buffer a non character item.

```lisp
(defun backspace-delete (buf)
  (setf (buf-from buf) (max 0 (+ -1 (buf-from buf))))
  (setf (aref (buf-vec buf) (buf-from buf)) nil))
```

# delete delete

like a forward delete , deletes that character ahead of caret.  

this character to be deleted will be at far right end of the gap buffer.  we can increase gap buffer upto limit of `LEN-1` of container array. again we can write a `nil` into the slot taken over by end of the gap buffer.

repeatedly smashing the del key when already at end of document will do nothing.

```lisp
(defun delete-delete (buf)
  (setf (buf-to buf) (min (+ -1 (length (buf-vec buf)))
			  (+ 1 (buf-to buf))))
  (setf (aref (buf-vec buf) (buf-to buf)) nil))
```

# forward character

we record current right hand end of gap buffer. try to bump end of gap buffer up . if bump caused end gap buffer to move then we know gap can move to right. 
- try bump end of gap buffer , if succeeds end is bumped already
- copy char from end of gap buffer to beginning of gap buffer
- bump begin of gap buffer

```lisp
(defun forward-char (buf)
  (let ((to (buf-to buf)))
    (setf (buf-to buf) (min (+ -1 (length (buf-vec buf)))
			    (+ 1 (buf-to buf))))
    (when (> (buf-to buf) to)
      (setf (aref (buf-vec buf) (buf-from buf)) (aref (buf-vec buf) (buf-to buf)))
      (setf (aref (buf-vec buf) (buf-to buf)) nil)
      (incf (buf-from buf)))))
```

# backward character

similar to forward character. we try to decrement begin of gap buffer. if succeeds we move character over to end of gap buffer. decrement end of gap buffer

```lisp
(defun backward-char (buf)
  (let ((from (buf-from buf)))
    (setf (buf-from buf) (max 0 
			    (+ -1 (buf-from buf))))
    (when (< (buf-from buf) from)
      ;; gap.from has moved left
      ;; buf-from now sits on a character
      ;; copy char gap.from -> gap.to
      (setf (aref (buf-vec buf) (buf-to buf)) (aref (buf-vec buf) (buf-from buf)))
      ;; nullify gap buffer 
      (setf (aref (buf-vec buf) (buf-from buf)) nil)
      ;; move gap.to left
      (decf (buf-to buf)))))
```



# Tests

```lisp
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

(tt:test insert-backspace-1
  (let ((buf (make-buffer)))
    (let ((str ""))
      (loop for i from 1 to 100 do 
	(insert buf #\a)
	(setq str (concatenate 'string str "a")))
      (setq str (subseq str 0 (+ -1 (length str))))
      (backspace-delete buf)
    (tt:is (equalp str (buffer-contents buf))))))


(defun run-tests ()
  (tt:run!))
```

