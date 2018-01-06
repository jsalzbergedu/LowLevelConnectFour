\ Hello forth!
\ As my introduction to forth, I'll be making a connect-four game. Feedback more than welcome.
\ And thank you reepca from #forth for finding bugs and helping me clean things up :)

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
\ 6 * 7 = 42 long board,
42 array board

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
\
\ However, this 2d board is flattened to a 1d array,
\ so j = 0, i in [0, 6] is items 0 through 6 of the array,
\ while j = 1, i in [0, 6] is items 1 + 6 through 6 + 6 of the array.
\ Therefore, any item i,j of the array is
\ item i + (j * 7) of the array.
: formula-i-j ( i1 j -- i2 ) 7 * + ;

\ Retrieve i, j from the board
: get-i-j ( i j -- n ) formula-i-j board @ ;

\ Now we can add a (raw) way to see the board
: view-row ( j -- ) cr
	   7 0 do
	     i over get-i-j . ( j -- j i -- j i j -- j n -- j )
	   loop
	   drop ;

: view-board ( -- ) 6 0 do i view-row loop ;

\ Now we need to get a visual representation of the board
\ First, we want a word to convert 0, 1, & 2 to their respective visual representations

\ Lookup table for converting 0, 1, and 2 to characters
\ 0 -> ' '
\ 1 -> 'x'
\ 2 -> 'o'
\ Make a 3-char-wide lookup table
create >visual-table 3 chars allot
\ Create 3 entries
\ Convert zeros to ' '
32 >visual-table !
\ Convert ones to 'x'
char x >visual-table 1 chars + !
\ Convert twos to 'o'
char o >visual-table 2 chars + !
\ And make a word to use the lookup table to convert the char
: >visual >visual-table + c@ ;
\ And make a word for consuming that char by emitting it
: >emitted >visual emit ;

\ Print some delimeters:
\ for any char c, the char will be wrapped in a delimeter like so:
\ |c|
\ ^^^
\ First we define the '|'s, which will go on the left & right of the char
124 Value side-lr
\ And then we define a word that prints it to reduce noise
: wrap-lr side-lr emit ;

\ Then we define the bottom char '^'
94 Value side-bot
\ and a word to print it
: emit-carot side-bot emit ;
\ We will also want words to wrap the bottom, which must be done a line at a time.
\ We can find the correct number of carots by just counting.
\ Since every row is made of 7 columns, all the rows will look like this:
\
\ |c|c|c|c|c|c|c|
\ ^^^^^^^^^^^^^^^
\
\ So each ^^^...^^^ sequence is made out of 15 characters
: emit-carots ( -- ) 15 0 do emit-carot loop ;
\ We also cannot forget the cairrage returns (\r):
\
\ |c|c|c|c|c|c|c|\r^^^^^^^^^^^^^^^\r
\
\ So the sequence of ^^^...^^^ carots is more correctly represented as
\ \r^^^...^^^
: wrap-bot ( -- ) cr emit-carots cr ;

\ Now we are ready to print out our board.
\ Lets first define some words for printing |c|
: print-row ( row-offset -- ) wrap-lr 7 0 do
	      i over ( row-offset i -- row-offset i row-offset )
	      get-i-j ( row-offset i row-offset -- row-offset n )
	      >emitted ( row-offset n -- row-offset )
	      wrap-lr
	      loop drop wrap-bot ;

\ And now we can print the whole board
: print-board ( -- ) cr 6 0 do i print-row loop ;

\ Now that we have our board, we can start to interface with the users.
\
\ Before we implement this user interface, let's describe it.
\ Firstly, the user interface will involve refreshing the terminal,
\ printing the board, and printing some instructions.
\ Heres a diagram (where '\f' stands for page break):
\
\ \f
\ | | | | | | | |
\ ^^^^^^^^^^^^^^^
\ | | | | | | | |
\ ^^^^^^^^^^^^^^^
\ | | | | | | | |
\ ^^^^^^^^^^^^^^^
\ | | | | | | | |
\ ^^^^^^^^^^^^^^^
\ | | | | | | | |
\ ^^^^^^^^^^^^^^^
\ | | | | | | | |
\ ^^^^^^^^^^^^^^^
\
\ (Instructions go here.)
\
\ Next, we want to allow the user to navigate the board. To do so, it would be easiest
\ to allow the use of the h and l keys to move the cursor to each column:
\
\ \f
\ | | | | | | | |
\ ^^^^^^^^^^^^^^^
\ | | | | | | | |
\ ^^^^^^^^^^^^^^^
\ | | | | | | | |
\ ^^^^^^^^^^^^^^^
\ | | | | | | | |
\ ^^^^^^^^^^^^^^^
\ | | | | | | | |
\ ^^^^^^^^^^^^^^^
\ | | | | | | | |
\ ^^^^^^^^^^^^^^^
\
\ Navigation keys:     h <-> l
\ 
\ (Instructions go here.)
\
\ Finally, when the user has moved to the column they want to place their
\ connect four game token in, they can hit return to select it:
\
\ \f
\ | | | | | | | |
\ ^^^^^^^^^^^^^^^
\ | | | | | | | |
\ ^^^^^^^^^^^^^^^
\ | | | | | | | |
\ ^^^^^^^^^^^^^^^
\ | | | | | | | |
\ ^^^^^^^^^^^^^^^
\ | | | | | | | |
\ ^^^^^^^^^^^^^^^
\ | | | | | | | |
\ ^^^^^^^^^^^^^^^
\
\ Navigation keys:     h <-> l
\
\ Instructions: Player 1 -- press return to select a cell
\
\ Implementing this user interface will take a lot of intermediate words
\ and values. The first task we will break down is the task
\ of translating the h, l, and return keys to visible actions on the board.
\ From the user's perspective, h and l should simply move the cursor
\ within the cells of the board, but from the program's perspective, something must keep
\ track of which cell the player is in and prevent the user from moving the cursor off the edge.
\ To do this, we need more information relating the board to the cursor.
\ Firstly, we can make it so that the cursor always starts in the
\ leftmost cell (the cursor is represented by a capital C):
\
\ \f
\ |C| | | | | | |
\ ^^^^^^^^^^^^^^^
\ | | | | | | | |
\ ^^^^^^^^^^^^^^^
\ | | | | | | | |
\ ^^^^^^^^^^^^^^^
\ | | | | | | | |
\ ^^^^^^^^^^^^^^^
\ | | | | | | | |
\ ^^^^^^^^^^^^^^^
\ | | | | | | | |
\ ^^^^^^^^^^^^^^^
\ ...
\
\ Secondly, we know that we must move the cursor by two characters to get it
\ to grow across a cell.
\ Thirdly, we know that the cursor cannot go more than
\ 6 steps (12 characters) right.
\ Fourthly, since this will be printed to a terminal, we know that
\ down and right will be the positive directions, as in this diagram from earlier:
\
\       i=0  i=1  i=2  ...  i=6
\       ___  ___  ___  ...  ___
\ j = 0 |0,0 1,0  2,0  ...  3,0
\ j = 1 |0,1 1,1  2,1  ...  3,1
\ j = 2 |...
\  ... ...
\ j = 5 |
\
\ Finally, we know that the cursor cannot go 1 step left from its current position.
\
\ To put those points into code, we will need (among other things) words that pass themselves the number
\ of steps that the cursor moves, and prevents the cursor from going out of bounds.
\ This word does not actually have to move the cursor itself, it just needs to give the
\ number of steps the cursor _would_ move.
\ We will call these words "move-step-left?" and "move-step-right?".
: move-step-left? ( i -- i1 ) dup 0 > if 1- then ;
: move-step-right? ( i -- i1) dup 6 < if 1+ then ;
\ These "cell results" must be doubled, and the characters must be halved,
\ to match the actual number of characters rather than the number of steps
: double ( n -- n1 ) dup + ;
: halve ( n -- n1 ) 2 / ;
\ Now with these words we can write move-cursor-up? move-cursor-down? etc which will
\ give the actual position of the cursor. Since the cursor starts at (x, y) = (1, 1),
\ we also have to subtract 1 before and add 1 after to get an accurate position.
: decrhalve 1- halve ;
: double1+ double 1+ ;
\ Since all the words do this, we will use backtick and execute to aid in code generation
: move-cursor-dir? ( ci word -- ci1 ) swap decrhalve swap execute double1+ ; 
: move-cursor-left? ( ci -- ci1 ) ['] move-step-left? move-cursor-dir? ;
: move-cursor-right? ( ci -- ci1 ) ['] move-step-right? move-cursor-dir? ;

\ And finally we can make words for actually moving the cursor.
\ We could do something like
\
\ : reset-xcursor ( -- x ) 1 1 at-xy 1 1 ;
\
\ But that is needlessly repetitive. Instead, we could make an
\ at-xy that does not consume its x param, and always stays in the first row.
: chain-atx ( x -- x ) dup 1 at-xy ;
: reset-cursor ( -- x ) 1 chain-atx ;
: move-cursor-left ( x -- x1 ) move-cursor-left? chain-atx ;
: move-cursor-right ( x -- x1 ) move-cursor-right? chain-atx ;
\ Now with these words we have for moving the cursor, we can make a
\ key listener that will keep changing the cursor location and recursing to itself until
\ enter is pressed.
\ Since h, l and not h nor l do not map efficiently to 0, 1 & 2, this is not a good candidate for
\ a dispatch table. Instead, we will use a case statement:
: move-cursor-key-case ( x -- x1 flag ) key case
			 [char] h of move-cursor-left true endof
			 [char] l of move-cursor-right true endof
			 13 of reset-cursor false endof
			 \ This is the "nothing matched" section of the case statement
			 \ When no match has been found, x has not yet been consumed
			 true ( x -- x flag )
		       endcase ;
\ Now that we have our key case statement, we can finally
\ implement our key listener, which recurses until the flag is false, then
\ leaves the x value to be used later
: move-cursor-key-listener ( x flag -- x1 ) if move-cursor-key-case recurse else drop then ;
\ With our key listener word, we can make a way to start the key-moving sequence:
: move-cursor-key-listener-init ( -- x ) reset-cursor true move-cursor-key-listener ; 
\ Then, we can make a word that takes the x, y and turns it back into the board's i
: user-select-column ( -- i ) move-cursor-key-listener-init decrhalve ;

\ There are also some smaller peices of the ui mentioned earlier, e.g. the navigation help:
\
\ ...
\ Navigation keys:     h <-> l
\ ...
\
\ Implementing that should be simple, just copy & paste!
: print-nav-keys ( -- ) cr ." Navigation keys:     h <-> l" cr ;

\ Finally, we have instructions. To accurately print the instructions, we would have to
\ know more about the game state than we do currently -- whose turn it is, who is playing etc.
\ Because we cannot currently make any progress in that direction, it is probably a better idea
\ to start implementing those game state peices, e.g. making a set of words to describe
\ who is playing the game
\ We will start with a finite state machine (I don't really know what those are so I hope
\ I'm getting that terminology right) for the player. It only has 2 states:
\ Player = 0 & Player = 1
create player 1 cells allot
0 player !
: get-player-state ( -- n ) player @ ;
: print-player ( -- ) get-player-state 1+ . ;
: get-player-tokennum ( -- n ) get-player-state 1+ ;
\ If player-state is 0, set it to 1. If it is 1, set it to 0.
: update-player-state ( -- ) get-player-state if 0 player ! else 1 player ! then ;
\ There are a few things we can do with the player state. First of all, we can print the instructions.
: instructions cr ." Player " print-player ." press return to select a cell" cr ;
\ Secondly, with the player-state information, we can make a setter for the board.
\ 1 will stand for player 0s token, and 2 will stand for player 1s token:
: set-i-j ( i j -- ) formula-i-j get-player-tokennum ( i j -- i1 playerstate ) swap board ! ;
\ However, as this is a connect four game, we want to have a more specialized setter,
\ which "drops" the token down the column.
\ To do so, we want to find the lowest empty cell, which in this case is
\ the cell with the highest j index that is equal to zero.
\ So we will want a word that will find that lowest empty cell in given a row, and return it.
\ To do that, lets break it up into a few sub-tasks of checking that
\ the cell is empty,
: cell-is-empty? ( i j -- flag ) get-i-j 0 = ;
\ duplicating the top two items of the stack (because we will be consuming both),
: dup2 ( n1 n2 -- n1 n2 n1 n2 ) over over ;
\ and recursing over the cells.
\ There is one wrinkle, however. The lowest empty cell might not actually exist in a column,
\ as the highest row in the column might be empty. For this reason, lowest-empty-cell-rec
\ must return a flag (true if i & j refer to an empty cell, false if not).
\ To do so, we test if the j has gone out of bounds, then return an error flag:
: j-in-board? ( j -- flag ) 0 >= ;
\ Now we want to break this up into multiple branches.
\
\ Branch 1: If the cell is empty, then flag it as true. Otherwise, go up a cell
\ and check that it is not out of bounds.
\ Branch 2: Check that the cell is out of bounds, and if so, then flag it as false. Otherwise, go
\ to Branch 1.
\ 
\ Oh no! This doesn't factor well into pretty little if statements. Indeed, its mutual recursion!
\ Luckily we can always just store the execution token and execute it later
: xt-pointer ( -- ) create 1 cells allot ;
xt-pointer out-of-bounds-branch-ptr
: out-of-bounds-branch-ptr-exec out-of-bounds-branch-ptr @ execute ;
\ Now we are ready to write the first branch
: cell-empty-branch ( i j -- i j flag ) dup2 cell-is-empty? if true else
							      1- out-of-bounds-branch-ptr-exec then ;
\ And now we can write branch 2, which can refer to branch 1 explicitly
: out-of-bounds-branch ( i j -- i j flag ) dup j-in-board? if cell-empty-branch else false then ;
\ Now we just have to set the xt-pointer
' out-of-bounds-branch out-of-bounds-branch-ptr !
\ We can simplify this further by supplying 5 (bottom of the board) as the first j:
: lowest-empty-cell ( i -- i j flag ) 5 out-of-bounds-branch ;
\ Now that we have most of the peices of the game, lets start composing them into higher-level words.
\ We already have enough to print out all of the ui:
: print-ui page print-board print-nav-keys instructions ;
\ We can also chain lowest-empty-cell, user-select-column, & set-i-j
\ as long as we have a word that always gets a valid row
: drop2 drop drop ;
: user-select-valid-row ( -- i j ) user-select-column lowest-empty-cell invert if drop2 recurse then ;
\ And now we can finish the chain.
: user-drop-game-token ( -- ) user-select-valid-row set-i-j ;
\ One more element which we havent added yet -- the player switch

\ Finally, we have enough to make test the whole ui loop (sans winning conditions etc).
: oneturn ( -- ) print-ui user-drop-game-token update-player-state ;
: game ( -- ) oneturn recurse ;

\ Okay, time to add a way to scan for four in a rows.
\ First, we can make a finite state machine with 4 states: 0, 1, 2, & 3, where 3 is the win condition.
\ It will need to have three ways to set it: increase and reset, as if the board has three tokens
\ in a row of the same kind, it must go back from the state of 2 to the state of 0.
\ Finally, if it only gets up to 1 or 2 by the end of the row, we want to reset it.
create scanforwin-fsm 1 cells allot
: reset-scanforwin 0 scanforwin-fsm ! ;
reset-scanforwin
\ Reset if it is not quite a win
: reset-scanforwin-branch ( -- ) scanforwin-fsm @ 3 < if reset-scanforwin then ;
\ We don't want our fsm to go past 3, so incr-scanforwin can only increase it up to 3
: incr-scanforwin ( -- ) scanforwin-fsm @ 3 < if 1 scanforwin-fsm +! then ;
\ And, as per the common usage, we want to either increase it or reset it depending on a
\ boolean flag
: incr-scanforwin-branch ( flag -- ) if incr-scanforwin else reset-scanforwin-branch then ;
\ And finally, we want to be able to see our fsm
: get-scanforwin ( -- n ) scanforwin-fsm @ ;
\ But ofcourse, we only really care if scanforwin is 'win' or 'not win'
: win-scanforwin? ( -- flag ) get-scanforwin 3 = if true else false then ;
\ And later on, unfortunatley, we'll see that we have to jump straight to the win
: win-scanforwin incr-scanforwin incr-scanforwin incr-scanforwin ;
: win-scanforwin-branch ( flag -- ) if win-scanforwin else reset-scanforwin-branch then ;


\ Now that we've done that, its time to use our fsm in a few scanning algorithms.
\ I'm no bit-level hacker, so this will be implemented by making words for scanning
\ horizontally, vertically, diagonally like / and diagonally like \.
\ Because we want to scan for four 1s (0th player token) and four 2s (1st player token),
\ we will have our word take what it is scanning for as a param.
: four-horz-row ( n j -- ) 7 0 do dup2 ( n j -- n j n j)
				    i swap get-i-j = ( n j n j -- n j f )
				    \ If it is equal, then start to increase the fsm. Else, start it over
				    incr-scanforwin-branch ( n j f -- n j )
				 loop drop2 ;
: four-horz ( n -- ) 6 0 do dup i four-horz-row reset-scanforwin-branch loop drop ;

: four-vert-column ( n i -- ) 6 0 do dup2 ( n i -- n i n i )
				       i get-i-j = ( n i n i -- n i f)
				       incr-scanforwin-branch ( n j f -- n j)
				    loop drop2 ;

: four-vert ( n -- ) 7 0 do dup i four-vert-column reset-scanforwin-branch loop drop ;

\ While the vertical and horizontal scanners were easy to write, we now have to figure out
\ how to write a diagonal one. Recall that our board looks like this:
\
\ Diagram of the board:
\       i=0  i=1  i=2  ...  i=6
\       ___  ___  ___  ...  ___
\ j = 0 |0,0 1,0  2,0  ...  3,0
\ j = 1 |0,1 1,1  2,1  ...  3,1
\ j = 2 |...
\  ... ...
\ j = 5 |
\
\ As this diagram impies, While the board is easily indexed vertically and horizontally, we currently
\ have no way to index it diagonally. We could figure out a formula to make a line across the board, 
\ which would be easier to visualize if we rotate the board so that the origin is in the bottom left:
\
\ Diagram of the board:
\ j = 5 |0,5 1,5 2,5 3,5 4,5 5,5 6,5
\ j = 4 |0,4 1,4 2,4 3,4 4,4 5,4 6,4
\ j = 3 |0,3 1,3 2,3 3,3 4,3 5,3 6,3
\ j = 2 |0,2 1,2 2,2 3,2 4,2 5,2 6,2
\ j = 1 |0,1 1,1 2,1 3,1 4,1 5,1 6,1
\ j = 0 |0,0 1,0 2,0 3,0 4,0 5,0 6,0
\       ^^^  ^^^ ^^^ ^^^ ^^^ ^^^ ^^^
\       i=0  i=1 i=2 i=3 i=4 i=5 i=6
\
\ Turning this board into diagonal strips reminds me of slope feilds, so lets graph one of those
\
\  ⟋  ⟋  ⟋  ⟋  ⟋  ⟋  ⟋
\  ⟋  ⟋  ⟋  ⟋  ⟋  ⟋  ⟋
\  ⟋  ⟋  ⟋  ⟋  ⟋  ⟋  ⟋
\  ⟋  ⟋  ⟋  ⟋  ⟋  ⟋  ⟋
\  ⟋  ⟋  ⟋  ⟋  ⟋  ⟋  ⟋
\  ⟋  ⟋  ⟋  ⟋  ⟋  ⟋  ⟋
\
\ This illustrates the problem even better: there is no easy way to map the grid to diagonal
\ strips, and worse, there are many diagonal strips that it can be split up into!
\ Indeed, we should instead start at the peices and count from there. This simplifies the problem a bit
\ when there are only a few tokens:
\
\ j = 5 |
\ j = 4 |
\ j = 3 |                 x
\ j = 2 |             x
\ j = 1 |         x
\ j = 0 |     x
\       ^^^  ^^^ ^^^ ^^^ ^^^ ^^^ ^^^
\       i=0  i=1 i=2 i=3 i=4 i=5 i=6
\
\ Now, when our / direction scanner hits 4,3, it will go up & over and see that there is no
\ token. It will continue this until it has hit 1,0, at which point it will find that there are four
\ in a row. It's terribly inefficient in terms of number of computations it has to make,
\ but it is easily understandable.
\ Another efficiency note -- since throughought the game most of the time no scan will result in a
\ four in a row, its probably not worth it to add in a bunch of branches that check if someone has won
\ before continuing.
\ With that being said, I'd like to repeat that I'm no bit-level hacker, so what I just said may be incorrect.
\ To increase & decrease both i & j, we will want 2 words for the slopes.
\ Recall how positive j is down and positive i is right:
\
\ Diagram of the board:
\       i=0  i=1  i=2  ...  i=6
\       ___  ___  ___  ...  ___
\ j = 0 |0,0 1,0  2,0  ...  3,0
\ j = 1 |0,1 1,1  2,1  ...  3,1
\ j = 2 |...
\  ... ...
\ j = 5 |
\
: a1+b1- ( n n1 -- n2 n3 ) 1- swap 1+ swap ; \ the / direction
: a1+b1+ ( n n1 -- n2 n3 ) 1+ swap 1+ swap ; \ the \ direction
\ So at this point, we could make a word like this :
\ : four-pos-slope-at ( n i j -- ) 4 0 do dup2 get-i-j -rot ( n i j -- n n2 i j )
\ 		    a1+b1- ( n n2 i j ) loop ( n n1 n2 n3 n4 i j ) drop2 ;
\ But it might go off the edge. To prevent that, we can make n1 through n4 be zero 
\ (because none of the tokens will be zero) if
\ they are off the edge:
: i-and-j-within-bottom? ( i j -- f) 5 <= swap 6 <= and ;
: i-and-j-within-top? ( i j -- f ) 0 >= swap 0 >= and ;
: i-and-j-within-bounds? ( i j -- f ) dup2 i-and-j-within-bottom? -rot i-and-j-within-top? and ;
: get-i-j-or-zero-branch ( i j -- n ) dup2 i-and-j-within-bounds? if get-i-j else drop2 0 then ;
: get-i-j-or-zero-branch-noconsume ( i j -- i j n ) dup2 get-i-j-or-zero-branch ;
\ As we will see, we will also need a helper word that can take 5 numbers and check that they are
\ all equal.
: oneconsume= ( n n1 -- n1 f ) dup rot = ;
: five= ( n1 n2 n3 n4 n -- f ) oneconsume= -rot oneconsume= rot and -rot oneconsume= rot and -rot = and ;


\ Now with that, we can write our diagonal scanner words
: four-neg-slope-at ( n i j -- ) 4 0 do get-i-j-or-zero-branch-noconsume -rot ( n i j -- n n2 i j )
					a1+b1+ ( n n2 ... n4 i1 j1 ) loop drop2
		    \ Now we have ( n n1 n2 n3 n4 ) on the stack. If they are all equal to n,
		    \ they will all be equal to each other:
		    five= win-scanforwin-branch  ;
		    \ win-scanforwin-branch ;
: four-neg-slope-column ( n i -- ) 6 0 do dup2 i ( n i -- n i n i j ) four-neg-slope-at ( n i ) loop
			drop2 ;
: four-neg-slope ( n -- ) 7 0 do dup i four-neg-slope-column ( n -- n ) loop drop ;
: four-pos-slope-at ( n i j -- ) 4 0 do get-i-j-or-zero-branch-noconsume -rot ( n i j -- n n2 i j )
					a1+b1- ( n n2 i1 j1 ) loop drop2
		    five=
		    win-scanforwin-branch ;
: four-pos-slope-column ( n i -- ) 6 0 do dup2 i four-pos-slope-at loop drop2 ;
: four-pos-slope ( n -- ) 7 0 do dup i four-pos-slope-column loop drop ;

\ Now we can wrap all those words up into one simple one:
: scan-board-for-win ( n -- ) dup four-horz dup four-vert dup four-pos-slope four-neg-slope ;
\ And we can even get rid of that n parameter:
: scan-board-current-player-win ( -- ) get-player-tokennum scan-board-for-win ;

\ Woohoo! Now we can make our real game loop:
: oneturn ( -- ) print-ui user-drop-game-token scan-board-current-player-win ;
: winui ( -- ) page ." Congratulations, player " print-player ." , you have won the game!" cr ;
: game ( -- ) oneturn win-scanforwin? if winui else update-player-state recurse then ;
