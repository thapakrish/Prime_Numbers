use "primes_below.sml";

(*author@Krishna
-->int->bool
-->checks if a given number is a prime number
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
