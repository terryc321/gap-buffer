# gap-buffer

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

```


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







