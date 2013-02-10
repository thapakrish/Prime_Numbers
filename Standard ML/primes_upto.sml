use "is_prime.sml";
(*author@Krishna
--> int-> int list
--> gives primes from 2 to num, where num is the largest prime number below n
*)
fun primes_upto(n) =
    let fun primes_list(m, acc)=
	    if m<=n then
		if is_prime(m) then primes_list(m+2,acc@[m])
		else primes_list(m+2,acc)
	    else acc
    in
	primes_list(3,[2])
    end
