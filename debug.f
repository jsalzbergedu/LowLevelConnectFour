\ Some debugging stuff

: diff-here ( addr -- ) here - 1 cells / ;
: mem-prettyprint ( addr -- ) dup diff-here . [char] | emit 32 emit @ . ;
\ Goes ncells back, ncells forward, & prints out there contents.
\ ncells must be positive
\ Goes ncells forward from addr
: prettyprint-index  ( i -- ) . [char] | emit 32 emit ;
: mem-table ( +ncells addr -- ) swap 1+ 0 do cr i prettyprint-index dup mem-prettyprint 1 cells + loop ;
\ : mem-table ( +ncells addr -- ) swap dup 0 >= if 1- swap dup
\ 						 mem-prettyprint 1 cells + recurse then ;
\ : mem-table-rec ( y x -- y x ) over over = invert if dup cells here + cr mem-prettyprint 1+ recurse then ;
\ : mem-table-recconsume ( x y -- ) swap mem-table-rec drop drop ;
\ : mem-table-backwards ( +ncells -- ) -1 * 0 mem-table-recconsume ;
\ : mem-table-forwards ( +ncells -- ) 0 swap 1+ mem-table-recconsume ;
\ : mem-table ( +ncells -- ) dup mem-table-backwards mem-table-forwards ;
