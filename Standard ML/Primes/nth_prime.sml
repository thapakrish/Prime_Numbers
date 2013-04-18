use "is_prime_impr1.sml";

(* author@Krishna
-->int->int
-->gives nth prime number
-->Not that fast. Using is_prime_impr1.
-->Can make it faster by using primes_list that are already available in the environment
*)
fun nth_prime1 (n) = 
    let fun next_prime (m, primes_list)=
	    if n > List.length(primes_list) then
		if is_prime_impr1(m) then next_prime(m+2,primes_list@[m])
		else next_prime(m+2,primes_list)
	    else
		List.last(primes_list)
    in
	next_prime(3,[2])
    end

fun nth_prime2 (n) = 
    let fun check_prime(num,acc) =
	    case acc of
		[] =>true
	      | hd::[] => num mod hd <> 0
	      | x :: xs' =>  num mod x <> 0 andalso check_prime(num, xs')
    in
	let fun next_prime (m, primes_list)=
		if n > List.length(primes_list) then
		    if check_prime(m,primes_list) then next_prime(m+2,primes_list@[m])
		    else next_prime(m+2,primes_list)
		else
		    List.last(primes_list)
	in
	    next_prime(3,[2])
	end
    end
