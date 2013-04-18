use "is_prime.sml";

(*
author@Krishna
-->cuban prime::have the form (n + 1)^3 − n^3. 
-->The ﬁrst few are 7, 19, 37, 61, 127, 271,
*)

val tab = fn n=>List.tabulate(n,fn x=>x)
val check_prime = map (fn x => if is_prime((x+1)*(x+1)*(x+1)-(x*x*x)) 
			       then (x+1)*(x+1)*(x+1)-(x*x*x) 
			       else 0)


fun cuban_primes (n) =  
    let
	val possible = (check_prime o tab) (n)
    in
	List.filter(fn x => x > 1) possible
    end
(*for each n, filter checks whether it gives a cuban prime.*)

(*tests*)
val cuban10= List.filter(fn x => x > 1) (cuban_primes 10)
val cuban15= List.filter(fn x => x > 1) (cuban_primes 15)
val cuban20= List.filter(fn x => x > 1) (cuban_primes 20)



