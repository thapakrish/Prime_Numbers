(*author@Krishna
--> int-> int list
--> gives list of prime numbers below n.
*)

use "is_prime.sml";

fun primes_below (n)=
    let fun divide_by_primes (m,acc)=
	    case acc of
		[] => true
	      | hd::[] => m mod hd <> 0
	      |x::xs' => m mod x <> 0 andalso divide_by_primes(m,xs')
    in
	let fun primes_list (m,xcc) =
		if m >= n then xcc
		else 
		    if divide_by_primes(m,xcc) then primes_list(m+2,xcc@[m])
		    else primes_list(m+2,xcc)
	in
	    primes_list(3,[2])
	end
    end



(*
cuban prime::have the form (n + 1)^3 − n^3
. The ﬁrst few are 7, 19, 37, 61, 127, 271,
*)

val tab = fn n=>List.tabulate(n,fn x=>x)
val check_prime = map (fn x => if is_prime((x+1)*(x+1)*(x+1)-(x*x*x)) 
			       then [(x+1)*(x+1)*(x+1)-(x*x*x)] 
			       else [])
val cuban_prime = check_prime o tab


(*panaitopol primes
p=(x^4-y^4)/(x^3-y^3)
*)
