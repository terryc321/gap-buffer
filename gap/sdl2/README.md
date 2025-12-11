# SDL2 

common lisp sdl2 is installed where

```lisp
(ql:where-is-system "sdl2")
=> #P"/home/terry/quicklisp/dists/quicklisp/software/cl-sdl2-20231021-git/"
```

little bit easier we can ask `slime` directly where a particular procedure is by pressing `alt-.`

# basic-example

entry point to a simple text editor using our gap buffer and sdl2 libraries through cl-sdl2 foreign function interface

```lisp
(sdl2:with-event-loop (:method :poll)
        (:keydown (:keysym keysym)
        (let ((scancode (sdl2:scancode-value keysym))
                (sym (sdl2:sym-value keysym))
                (mod-value (sdl2:mod-value keysym)))
        (cond
                ((sdl2:scancode= scancode :scancode-w) (format t "~a~%" "WALK"))
                ((sdl2:scancode= scancode :scancode-s) (sdl2:show-cursor))
                ((sdl2:scancode= scancode :scancode-h) (sdl2:hide-cursor)))
        (format t "Key sym: ~a, code: ~a, mod: ~a~%"            sym
                scancode
                mod-value)))

```

## keyboard modifiers


```c
/* https://wiki.libsdl.org/SDL3/SDL_Keymod */
/* 
#define SDL_KMOD_LCTRL  0x0040u /**< the left Ctrl (Control) key is down. */
    this means left control key modifier has hex value 40 
	the u just means unsigned value (0 to infinity ...)
*/
#define SDL_KMOD_NONE   0x0000u /**< no modifier is applicable. */
#define SDL_KMOD_LSHIFT 0x0001u /**< the left Shift key is down. */
#define SDL_KMOD_RSHIFT 0x0002u /**< the right Shift key is down. */
#define SDL_KMOD_LEVEL5 0x0004u /**< the Level 5 Shift key is down. */
#define SDL_KMOD_LCTRL  0x0040u /**< the left Ctrl (Control) key is down. */
#define SDL_KMOD_RCTRL  0x0080u /**< the right Ctrl (Control) key is down. */
#define SDL_KMOD_LALT   0x0100u /**< the left Alt key is down. */
#define SDL_KMOD_RALT   0x0200u /**< the right Alt key is down. */
#define SDL_KMOD_LGUI   0x0400u /**< the left GUI key (often the Windows key) is down. */
#define SDL_KMOD_RGUI   0x0800u /**< the right GUI key (often the Windows key) is down. */
#define SDL_KMOD_NUM    0x1000u /**< the Num Lock key (may be located on an extended keypad) is down. */
#define SDL_KMOD_CAPS   0x2000u /**< the Caps Lock key is down. */
#define SDL_KMOD_MODE   0x4000u /**< the !AltGr key is down. */
#define SDL_KMOD_SCROLL 0x8000u /**< the Scroll Lock key is down. */
#define SDL_KMOD_CTRL   (SDL_KMOD_LCTRL | SDL_KMOD_RCTRL)   /**< Any Ctrl key is down. */
#define SDL_KMOD_SHIFT  (SDL_KMOD_LSHIFT | SDL_KMOD_RSHIFT) /**< Any Shift key is down. */
#define SDL_KMOD_ALT    (SDL_KMOD_LALT | SDL_KMOD_RALT)     /**< Any Alt key is down. */
#define SDL_KMOD_GUI    (SDL_KMOD_LGUI | SDL_KMOD_RGUI)     /**< Any GUI key is down. */
```

we can now check if the `left control key` is pressed or not

```lisp
(format t "left shift key pressed => ~a" (logand #x1 mod-value))
(format t "right shift key pressed => ~a" (logand #x2 mod-value))
(format t "any shift key (left or right) key pressed => ~a" (logand (logior #x1 #x2) mod-value))
```

```lisp
(format t "left control key pressed => ~a" (logand #x40 mod-value))
(format t "right control key pressed => ~a" (logand #x80 mod-value))
(format t "any control key (left or right) pressed => ~a" (logand (logior #x40 #x80) mod-value))
```


## key repeat

change key repeat rate to insane speeds if we just change the two numbers , holy knows what they do

```bash
xset r rate 150 30
```

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








