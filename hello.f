\ Hello forth!
\ As my introduction to forth, I'll be making a connect-four game. Feedback more than welcome.

\ First, let's get the "low level" data representation out of the way.
\ This will be subject to change, but for now our 7 column and 6 row board will
\ be represented by an array of 0s, 1s, & 2s,

\ Define a word for allocating an array
: array ( n -- ) ( i -- addr )
  create cells allot
  does> swap cells + ;
\   create cells allot
\   does> cells + ;

\ Create our board
45 array board

\ Set one of the board's cells to zero
: reset-cell ( n -- ) 0 swap board ! ; 

\ and initialize the board's contents to zeroes.
: reset-board 46 0 do i reset-cell loop ;
reset-board

\ Now we want an interface for getting i, j of the board.
\ Diagram of the board:
\       i=0  i=1  i=2  ...  i=6
\       ___  ___  ___  ...  ___
\ j = 0 |0,0 1,0  2,0  ...  3,0
\ j = 1 |0,1 1,1  2,1  ...  3,1
\ j = 2 |...
\  ... ...
\ j = 5 |
\ However, this 2d board is flattened to a 1d array,
\ so j = 0, i in [0, 6] is items 0 through 6 of the array,
\ while j = 1, i in [0, 6] is items 1 + 6 through 6 + 6 of the array.
\ Therefore, any item i,j of the array is
\ item i + (j * 6) of the array.
: formula-i-j ( i1 j -- i2 ) 6 * + ;

\ Retrieve i, j from the board
: get-i-j ( i j -- n ) formula-i-j board @ ;

\ Now we can add a (raw) way to see the board
: veiw-row ( j -- ) cr
	   7 0 do
	     i swap tuck get-i-j ( j -- j i -- i j -- j i j -- j i1)
	     board @ .
	   loop
	   drop ;

: view-board ( -- ) 6 0 do i veiw-row loop ;

\ Now we need to get a visual representation of the board
\ First, we want a word to convert 0, 1, & 2 to their respective visual representations

\ Convert zeros to ' '
: zero-branch ( n -- c ) 0 = if 32 then ;
\ Convert ones to 'x'
: one-branch ( n -- c ) dup 1 = if drop 120 else zero-branch then ;
\ Convert twos to 'o'
: two-branch ( n -- c ) dup 2 = if drop 111 else one-branch then ;


\ Print some delimeters:
\ for any char c, the char will be wrapped in a delimeter like so:
\ |c|
\ ^^^
\ First we define the '|'s
124 Value side-rig
side-rig Value side-lef

\ Then we define the bottom char '^'
94 Value side-bot

\ And the '\r' character
13 Value ret
\ This character seems to behave strangely, so I'll make a word for translating ret -> cr
: retemit ( c -- ) dup 13 = if drop cr else emit then ;

\ Now we want a grid of these delimeters surrounding the chars.
\ The simplest way to get this grid would be to map the board
\ to a string using a loop.
\ To do so, first we must come up with a string structure
\ that will fit our board. To do so, we must come up
\ with a formula that relates the number of chars in the board (46)
\ with the number of chars in the grid.
\ To calculate the correct number of | delimeters, lets look at a few diagrams:
\
\     inner delims
\  /-------^-------\  
\ |c|c|c|c|c|c|c|c|c| <- outer delimeter
\
\ There are always 2 outer delimeters, but there are not always inner delimeters;
\ see this diagram:
\
\ |c|
\
\ If you look count, you will find that the ratio of inner delimeters to characters
\ is one to one, but inner delimeters only start at the second character.
\ Therefore the formula for inner delimeters is n - 1, where n is the number of characters
\ and the formula for the outer delimeters is 2.
\ So the total number of delimeters will always be 2 + (n - 1) => n + 1, where n is the number of
\ characters.
\ Then we have the other delimeter, '^'. for every delimeter and character, there is an '^':
\
\ |c|c|c|c|
\ ^^^^^^^^^
\
\ It then follows that the number of '^' delimeters is equal to the number of characters
\ plus the number of '|' delimeters, or n + n + 1 => 2n + 1
\ Finally we have the newline at the end of each line.
\ Since there is 1 per row, and we know we have 6 rows, there are 6.
\ It then follows that the sum of the delimeters, characters, and newlines is
\ (n) + (n + 1) + (2n + 1) + 6 => (2n + 1) + (2n + 1) + 6 => 4n + 2 + 6 => 4n + 8
\ Since we have 46 characters, we need 4(46) + 8 => 192 long string,
\ aka a 191 long char array since the array starts at zero.

\ Same thing as the earlier array, simply using 'chars' as the size instead of cells
: char-array ( n -- ) ( i -- addr )
  create chars allot
  does> swap chars + ;

191 char-array board-chars

\ And let's add a setter that can easily be chained
\ The normal way of setting isn't all that chain-able. Lets say we have a word,
\ set-foo, that takes a value, and offset, and sets foo at that offset then
\ returns the next offset:
\
\ : set-foo ( n offset -- offset1 ) 1+ tuck foo ! ;
\
\ Well the problem is that now we have an offset in the stack, and to push a number to
\ it again we would have to swap, which is ugly:
\ 
\ 1 0 set-foo .s <1> 1 ok
\ 2 swap set-foo
\
\ Instead, we can make it inherently chain-able by reversing the order of the arguments.
\ To do so, we must first create a word that can change 1 2 3 => 3 1 2

: back-two ( n n1 n2 -- n2 n n1) swap rot swap ;

: chain-char ( offset char --  offset1 ) swap
	      dup 1+ back-two ( char offset -- offset1 char offset )
	      board-chars ! ;

\ Now back to mapping our board to an array of chars.
\ Lets first define some words for inserting |c|
: wrap-lef ( offset -- offset1 ) side-lef chain-char ;
: wrap-rig ( offset -- offset1 ) side-rig chain-char ;
\ Now we will wrap the sides of char à la |c|
: wrap-sides ( offset char -- offset1 ) swap
	     wrap-lef swap chain-char wrap-rig ;

\ Then we need to add the \r^^^...^^^\r delimeters
\ Going back to our formulas, there's n chars per line,
\ and n + 1 deliminating characters, for a total of 2n + 1 chars in a row
\ Since each row is 7 columns long, we want 7(2) + 1 => 15 ^s.
\ First, some words to reduce noise
: insert-bot ( offset -- offset1 ) side-bot chain-char ;
: insert-ret ( offset -- offset1 ) ret chain-char ;

\ A word that inserts the 15 '^'s
: insert-carots ( offset -- offset1 ) 15 0 do insert-bot loop ;

\ And finally a word that inserts \r^^...^^\r
: wrap-bot ( offset -- offset1 ) insert-ret insert-carots insert-ret ;

\ Now we are ready to write a word that maps one row to the char array
: row-to-char-array ( char-offset board-row -- char-offset1 ) swap wrap-lef swap
		    7 0 do
		      i swap tuck ( c-o b-r i -- c-o b-r i b-r )
		      get-i-j ( c-o b-r i b-r -- c-o b-r n )
		      two-branch ( c-o b-r n -- c-o b-r c )
		      rot swap ( c-o b-r c  -- b-r c-o c )
		      chain-char ( b-r c-o c -- b-r c-o1 )
		      wrap-rig ( b-r c-o1 -- b-r c-o2 )
		      swap loop drop wrap-bot ;

\ And a word that maps all the rows to a char array
: board-to-char-array ( -- ) 0 0 row-to-char-array 6 0 do i row-to-char-array loop ;

\ Reset the board
: reset-board-chars 192 0 do 32 i board-chars c! loop ;
reset-board-chars

\ Print the board
: print-board cr 192 0 do i board-chars c@ retemit loop ;
