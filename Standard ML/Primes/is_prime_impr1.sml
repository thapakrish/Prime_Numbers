use "primes_below.sml";

(*author@Krishna
-->int->bool
-->checks if a given number is a prime number
-->is_prime_impr1 is also recursive, in a sense that primes_below checks for primes and gives primes_list.  It uses higher order functions, instead of utilizing SML's recursive call mechanisms.
*)
fun is_prime_impr1 (n)=
    let fun check_prime (acc) =
	    case acc of
		[] => true
	      | hd::[]=> n mod hd <> 0
	      | x::xs' => n mod x <> 0 andalso check_prime(xs')
    in
	let 
	    val m = floor(Math.sqrt (Real.fromInt n))
	    val primes_list = primes_below(m)
	in
	    if n<2 then false
	    else if n = 2 then true
	    else check_prime(primes_list)
	end
    end

