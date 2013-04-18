(*author@Krishna
--> Want to check if a given number is a prime number
--> Let's say the given number n = 101. We want to know if it is a prime number.
--> SQRT(101) would give 10/11, depending on floor or ceil functions.
--> WLOG, get's say 11. Want to know primes below 11.
--> In order to check if 11 is a prime, one needs to find out if it is divisible by primes below sqrt(11). Same with 10,9 and so on...
--> So, mutual recursion is the way to go.
*)

fun is_prime_impr2 (n) =
    let fun check_prime (acc) =
	    case acc of
		[] => true
	      | hd::[]=> n mod hd <> 0
	      | x::xs' => n mod x <> 0 andalso check_prime(xs')
    in
	let 
	    val m = floor(Math.sqrt (Real.fromInt n))
	    val primes_list = primes_upto(m)
	in
	    if n<2 then false
	    else if n = 2 then true
	    else check_prime(primes_list)
	end
    end

and primes_upto (n) =
    let fun primes_list (m, acc)=
	    if m<=n then
		if is_prime_impr2(m) then primes_list(m+2,acc@[m])
		else primes_list(m+2,acc)
	    else acc
    in
	(*print ("primes_upto calls-- \n");primes_list(3,[2])*)
	primes_list(3,[2])
    end
