(*author @ Krishna
Not the most elegant sml code out there!
*)

use "Primes.sml";
(*
-- Notice the relationship between the number of primes at the diagonal to the input n.
-- Also, notice the awesome relationship between the number of "possible primes" given by Primes.spiral and the given input n
-- The "possible primes" consists of odd numbers scattered within the "spiral square", but not all the odd numbers within the square. 
-- Relationship between odd numbers and and n is given by 2 * n -1.
-- Again, I repeat, the "possible primes" aren't just the half of odd numbers. They come from the diagonals only.

Sweet!

*)

fun nums_fromto (start, stop) =
    let fun get_tuple (m) =
	    let
		val a = Primes.spiral(m)
		val b = length(a)
		val c = Primes.fromlist(a)
		val d = length(c)
	    in
		(m,b,d, floor(real(d) / real(b)*real(100)))
(* d/b gives the percentage of primes at the diagonal to the total numbers at the diagonal*)
	    end
		      
    in
	let fun print_tuple (num) =
		if num < stop then
		    get_tuple (num) :: print_tuple (num+2)
		else []

	in
	    print_tuple(start)
	end

    end

(* Let's look at few examples here below. The first and second items on the tuple is what we're looking at to see the 2*x -1 thing.*)

val n1to9 = nums_fromto(1,9)
val n9to21 = nums_fromto(9,21)
val n9999to10011 = nums_fromto(9999,10011)
