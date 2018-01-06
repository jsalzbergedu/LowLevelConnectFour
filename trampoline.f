\ A silly attempt to implement a trampoline 

\ A way to implement tail recursion that might blow up the call stack:
create tailrec-bad-xt 1 cells allot
: tailrec-bad ( n -- n ) dup 10 < if 1+ tailrec-bad-xt @ jump then ;
' tailrec-bad tailrec-bad-xt !
\ To test, do 0 tailrec-bad.
\ It works, but it goes deeper in the call stack than it has to.

\ a trampolinable version
create tailrec-good-xt 1 cells allot
\ The flag is false unless tailrec-good is done
\ At the cost of having to tell the trampoline when the computation is done,
\ we can have the trampoline iterativley keep calling tailrec-good.
\ In this instance its not too different from a while loop, but
\ perhaps it would be useful for mutual recursion and other hijinks
: tailrec-good ( n -- n xt f or n -- n f ) dup 10 < if 1+ tailrec-good-xt @ true else false then ;
' tailrec-good tailrec-good-xt !
: trampoline ( xt f -- ) begin if execute false else true then until ;

\ Well, apparently forth has a jump instruction, so I just wasted my time ;v;
