(*
Problems from https://sites.google.com/site/prologsite/prolog-problems/2

Not the most efficient is_prime algorithm, but it'll do here.
*)


(*2.01*)

fun is_prime (n) =
    let fun not_divisible(m) =
	    if n mod m = 0 then false
	    else if m*m >= n then true
	    else not_divisible(m+2)
    in
	if n < 2 then raise Domain
	else if 6 mod n = 0 then true
	else if n mod 2 = 0 then false
	else not_divisible(3)
    end


(*2.02*)

fun primes_below (n) =
    let fun check (m, acc) =
	if m > n then acc
	else if is_prime m then check(m+2, acc@[m])
	else check(m+2,acc)
    in
	check (3, [2])
    end

fun prime_factors (n) =
    let fun spit_factors (lst) =
	    case lst of
		[]=> []
	      | xs::ys => if n mod xs = 0 then xs::prime_factors(n div xs)
			  else spit_factors (ys)
    in
	spit_factors(primes_below(n))
    end	


(*2.03*)

fun prime_factors_mult (n) =
    let 
	val primes = prime_factors (n)
    in
	let fun primes_mult (primes_lst, prime, count) =
		case primes_lst of
		    []=> [[prime, count]]
		  | xs::ys => if xs = prime then primes_mult (ys, prime, count+1)
			      else [[prime, count]]@primes_mult (ys, xs, 1)
	in
	    primes_mult (primes, hd primes, 0)
	end
    end

fun diff_list (lista, listb) =
    case (lista,listb) of


(*2.04*)

fun primes_between (below, upper) =
    let 
	val bp = primes_below (below)
	val up = primes_below (upper)
    in 

    end
