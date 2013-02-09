(*
--> int*int->int list
--> gives primes numbers between two numbers (including both numbers)
--> when starting num=2, it gives primes from 3 to end num.
--> If one were to start from 2, it is better to use primes_upto or primes_below functions.
--> Can further improve it by checking only for odd numbers.
*)
use "primes_below.sml";

fun primes_from_to (from,to)=
    let  fun divide_by_primes (m,acc)=
	     case acc of
		 [] => true
	       | hd::[] => m mod hd <> 0
	       |x::xs' => m mod x <> 0 andalso divide_by_primes(m,xs')
	 val lst = primes_below(from)
	 val last_lst = List.last(lst)
    in
	let fun primes_list (num,lst) =
		if num>to then lst
		else 
		    if divide_by_primes(num,lst) then primes_list(num+1,lst@[num])
		    else primes_list(num+1,lst)
	in
	    List.filter(fn x => x > last_lst) (primes_list(from,lst))
	end
    end


