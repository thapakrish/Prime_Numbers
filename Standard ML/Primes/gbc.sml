use "primes_below.sml";

(*odd composites are of the form  2*m+1, where m >=4*)
fun possible_odd_composites (N) = 
    let fun helper ( acc, m) =
	    if m > N then acc
	    else helper (acc@[2*m+1], m+1)
    in
	if N < 3 then []
	else helper([], 4)
    end

(*gives bool:true if item is in the list*)
fun is_in ( item, lst) =
    case lst of
	[]=> false
      | xs::ys => if item = xs then true
		  else is_in(item, ys)

(*list1-list2: items in list1 that are not in list2*)	
fun diff (lst1, lst2) =
    case (lst1, lst2) of
	(sth,[])=>sth
      | ([],sth) => []
      | (xs::ys, zs) => if is_in(xs, lst2) then diff(ys,zs)
			else xs::diff(ys,zs)

(*composite numbers*)
fun odd_composites (N)=
    let
	val possible = possible_odd_composites(N)
	val primes = primes_below(List.last possible +2)
    in
	diff (possible, primes)
    end

(*given a number num, and a list of primes p_list, checks 
if num=prime+2*n**2 is true for any number n.*)
fun two_square (num, p_list) =
    let fun multiples (acc,pn) =
	    if (pn+2*acc*acc <= num) then
		(num = pn+2*acc*acc) orelse multiples(acc+1,pn)
	    else false
    in
	case p_list of
	    []=> true
	  | xs::ys => multiples(1,xs) orelse two_square(num,ys)
    end
	
(*given a number N, checks the conjecture for all odd_composites(N)*)	
fun gbc (N) =
    let
	fun prime_tsquare (num, pb) =
	    case num of
		[]=>[]
	      | xs::ys => if two_square(xs,pb) then xs::prime_tsquare(ys,pb)
			  else prime_tsquare(ys,pb)
    in	
	let
	    val comps = odd_composites (N)
	    val pb = primes_below((List.last comps) + 2)
	in
	    prime_tsquare(comps, pb)
	end
    end

