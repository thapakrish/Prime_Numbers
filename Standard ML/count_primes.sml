use "primes_from_to.sml";

(*
--> int*int->int
--> counts the number of primes between two numbers.
*)
fun count_primes (from, to) =
    if from = 2 then
	List.length(primes_from_to(from,to))+1
    else
	List.length(primes_from_to(from,to))
