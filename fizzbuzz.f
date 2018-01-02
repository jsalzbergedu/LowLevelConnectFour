\ Fizzbuzz

\ First we do the number modulo 3
: is-factor? ( n1 n2 -- n3 ) mod 
	    \ test if the result is equal to zero
	    0 =
	    \ if so, then return true, else return false
	    if -1 else 0 then ;

: factor-three? ( n1 -- n2 ) 3 is-factor? ; 

: factor-five? ( n1 -- n2 ) 5 is-factor? ;

: factor-three-and-five? 15 is-factor? ;

\ Fizz-buzz-ify the number
: fizzbranch ( n -- ) dup factor-three? if ." fizz " else . then ;

: buzzbranch ( n -- ) dup factor-five? if ." buzz " else fizzbranch then ;

: fizzbuzzbranch ( n -- ) dup factor-three-and-five? if ." fizzbuzz " else buzzbranch then ;

: fizzbuzz ( end start -- ) cr 1 + do i fizzbuzzbranch loop cr ;

