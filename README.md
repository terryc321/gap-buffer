# gap-buffer

# the initial buffer
initial buffer in a text editor will be empty - the entire file is the gap-buffer 
in essence there is no 'text' in the buffer

# optimal size gap buffer 

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







