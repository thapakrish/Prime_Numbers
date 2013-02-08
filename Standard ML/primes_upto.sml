use "is_prime.sml";

fun primes_upto(n) =
    let fun primes_list(m, acc)=
	    if m<=n then
		if is_prime(m) then primes_list(m+1,acc@[m])
		else primes_list(m+1,acc)
	    else acc
    in
	primes_list(2,[])
    end
