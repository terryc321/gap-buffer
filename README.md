
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

# THiS is THE End ... 

# SDL2 

common lisp sdl2 ffi using quicklisp seems to work out of the box

## key repeat

waits for a key up down means does not do a key repeat , so have to tweak that , 
determine

## key combinations

accurate control of keyboard whatever sdl2 or however gui implemented is paramount to a good editor experience



# clipboard 

copy paste . especially large copy paste from firefox perhaps advent of code large web page data.

# the initial buffer
initial buffer in a text editor will be empty - the entire file is the gap-buffer 
in essence there is no 'text' in the buffer

# optimal size gap buffer 

how big should the gap buffer be ?
0 bytes initially ?
1000 bytes ?
4k bytes ? 

common lisp array 0 to length-1
the perenial problem of the off by errors abound !

unicode characters - multibyte characters - now not just a single ascii character


## forget unicode - simplify 
lets forget unicode for now.

## forget binary files - simplify
lets forget binary code for now - just visible ascii characters ?

suppose entire memory of text editor consists of this

## disk

disk refers to anything not in memory where the contents of the buffer may possibly be saved for future retrieval

## file vs buffer

file is permanent on disk or solid state.

buffer is in memory and volatile to loss power.

buffer is a model of the contents of the text editor.

we can use model view controller approach to 'look' at various parts of the buffer.

we could have a view looks from start of buffer.

we could have a view looks from end of buffer.

we can have a view looks from middle of buffer.

all on the same 'buffer' model

## undo redo

very likely we will want to make undo / redo - so operations should be recorded and
it be possible to undo whatever changes were made

within scope as it is not generally possible to control the outside world , namely the disk

### empty buffer
for an arbitrary size buffer LEN in an array - typically c code 0 to LEN-1

we can see a number of things .  the start and end of the 'file' is the same location.
the 'file' is essentially empty . 

```ascii
0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 ... (LEN-1)
^----------------------------^ gap buffer
^-- start of file
^-- end of file
```

### buffer

```ascii
0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 ... (LEN-1)
a s d f ^-----------------------------------------^  p  e  t  e  r
^-- start of file                                                ^--- end of file

in emacs if we save the file containing "asdfpeter" in asdfpeter.txt

let us do a canonical hexdump of the text file

> hexdump -C asdfpeter.txt

-------------- decimal byte values  ----------------------- ascii rep ----
00000000  61 73 64 66 70 65 74 65  72 0a                    |asdfpeter.|
------------------------------------------------------------------------
-bytes --  1  2 3  4  5  6  7  8   9  10  

> wc --words --lines --bytes asdfpeter.txt 
 1  1 10 asdfpeter.txt

the file is 10 bytes long , contains 1 word and 

```

### start and end of file buffer
```ascii

if start of file buffer = end of file buffer then the file is currently empty. 
actually a single byte . 
> touch empty.txt

0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 ... (LEN-1)
^---------^ gap buffer
^-- start of file buffer
^-- end of file buffer
```


### insertion

```ascii
suppose we have a gap buffer of 5 characters initially empty , type 'a b c' ,
we notice the end of file marker moves to point at the last character.
characters a b c have been entered into the array, the gap buffer size has decreased

0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 ... (LEN-1)
^---------^ gap buffer
^-- start of file
^-- end of file

0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 ... (LEN-1)
a b c ^---^ gap buffer
^-- start of file
    ^-- end of file

if we enter 'de' , we again notice the end of file marker moves along.
the gap buffer is now squanshed to byte offset 5 .
both start and end of gap buffer point to the same offset.

0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 ... (LEN-1)
a b c d e ^^ gap buffer
^-- start of file
        ^-- end of file

lets now expand the gap buffer to another five bytes

0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 ... (LEN-1)
a b c d e ^---------^ gap buffer
^-- start of file
        ^-- end of file

expansion of gap buffer does not go into the undo history since it does not affect the text model as such        

```

### in the middle

situation where caret is after e but before f 

```ascii
visually  abcde|fghijk


0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 ... (LEN-1)
a b c d e ^---------^   f  g  h  i  j  k
            gap buffer                 ^
^-- start of file                      |
                                       |-- end of file
```

### constraint on gap buffer start & end 

lets suppose we never let gap buffer start and end be the same point . if it ever were to occur then we should immediately grow the gap buffer . 

### 

```ascii

0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 ... (LEN-1)
a b c d e ^---------^   f  g  h  i  j  k
            gap buffer                 ^
^-- start of file                      |
                                       |-- end of file


```


smallest gap buffer we can have is one byte gap buffer ? - thats ready for text

### deleting a character -
enlarge gap buffer by one - simply 

### inserting a character -
consume one byte - theirby reducing gap buffer size by one

### moving down one line -

### moving up one line -

### forward one character - 

### backward one character - 

## caret 
open file in buffer the caret is at the start of 'text'
the gap buffer is before all the 'text'

caret position is BETWEEN characters - so if caret is at byte offset 0 , it is 'BEFORE'
the character in 'text' at byte offset 0 

CARET   {FIRST-BYTE}  VIRTUAL-CARET-POS  {SECOND-BYTE}  ANOTHER-CARET-POS 
^                   ^                                   ^

## line and column numbering 
column = 1

row numbering -
row = 1

what does it mean to be at row 0 ? or column 0 ?
why not row -3 ? column -4 ?
row 2+3i ?
row blue ?

when moving caret or cursor around - keep 'view' caret within visible region

# extra large files

# paredit 

matching brackets , parenthessis , position of open/closing braces - aka paredit

# keyboard input modes

keyboard input/output - control keys alt super modifiers -
how many keys can be detected by being down
can we use a window manager that simply does not reference

# colorised display

colourisor using seperate threads 








