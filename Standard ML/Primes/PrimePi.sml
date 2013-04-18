(*@Krishna
-->This is a prime counting function
-->int->int list
-->gives the number of primes less than or equal to n.
-->PrimePi (1)=0, PrimePi(2)=1, PrimePi(3)=2,PrimePi(4)=2,PrimePi(5)=4,...
-->Same as PrimePi function in Mathematica(?)
*)

use "primes_upto.sml";

fun PrimePi (n) = (*Assuming the clint enters number >=1*)
    let fun helper(m, acc)=
	    if m < n+1 then
		helper(m+1,acc@[List.length(primes_upto(m))])
	    else acc
    in
	helper(1,[])
    end
