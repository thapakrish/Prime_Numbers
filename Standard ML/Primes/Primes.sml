(*author@Krishna
*)


signature Primes_sig =
sig
exception BadNum
val is_prime : int -> bool
val upto : int -> int list
val from_to : int*int -> int list
val count : int* int -> int
val factors_of : int -> int list
val PrimePi : int -> int list
val fib : int -> int list
val cuban : int -> int list
end

structure Primes :> Primes_sig=
struct

exception BadNum

fun is_divisible_by (n, lst) = (*int*int list -> bool*)
    case lst of
	[]=> true
      | hd::[] => n mod hd <> 0
      | x::xs' => n mod x <> 0 andalso is_divisible_by(n,xs')



fun is_not_divisible (n)= (*int->bool*)
    let
	val sqrt_n = floor(Math.sqrt (Real.fromInt n))
	val odd_num = List.tabulate(sqrt_n div 2 +1, fn x=>2*x +1)
				(*odd_num has 1 but not 2 in the list*)
	val lst = 2::tl(odd_num)
		                (*removing 1 and adding 2 to the list*)
    in
	is_divisible_by (n, lst)
    end				   
		
		   
fun is_prime (n) = (* int -> bool *)
    if n < 0 then raise BadNum
    else if n = 2 then true (* 2 mod 2 <>0 will give false in is_not_divisible*)
    else is_not_divisible (n)


fun upto (n) = (*int -> int list*)
    let fun helper (m,acc) =
	if m <= n then
	    if is_divisible_by(m,acc) then helper(m+2,acc@[m])
	    else helper(m+2,acc)
	else acc
    in
	helper(3,[2])
    end

fun from_to (from,to)=
    let fun helper (m, acc) =
	    if m > to then acc
	    else
		if is_divisible_by (m, acc) then helper(m+2, acc@[m])
		else helper(m+2,acc)
    in
	let val lst = upto(from)
	    val last_lst = List.last(lst) (*last member of primes below from*)
	in
	    List.filter(fn x => x >= last_lst)(helper(if from mod 2 <> 0 then from else from+1,lst))
	(*only interested in members greater than last_lst*)
	end
    end	

fun count (from, to) =
	List.length(from_to(from,to))


fun factors_of (n) =
    let 
	val lst = upto (n)
    in
	List.filter(fn x => n mod x = 0 ) lst
    end

fun fib_sequence (n) = (*int->int list, Fib(0)=0 and Fib(1)=1*)

    if n = 0 then [0]
    else if n = 1 then [0,1]
    else
	let fun helper (count, prev,current,acc)=
		if count<n then
		    helper (count+1,current, prev+current,acc@[current])
		else acc@[current]
	in
	    helper(2,1,1,[0,1])
	end
fun fib (n) = List.filter(fn x => is_prime(x)) (fib_sequence(n))

fun PrimePi (n) = 
    if n < 1 then raise BadNum
    else
	let fun helper(m, acc)=
		if m < n+1 then
		    helper(m+1,acc@[List.length(upto(m))])
		else acc
	in
	    helper(1,[])
	end

val tab = fn n=>List.tabulate(n,fn x=>x)
val check_prime = map (fn x => if is_prime((x+1)*(x+1)*(x+1)-(x*x*x)) 
			       then (x+1)*(x+1)*(x+1)-(x*x*x) 
			       else 0)


fun cuban (n) =  
    let
	val possible = (check_prime o tab) (n)
    in
	List.filter(fn x => x > 1) possible
    end

    
end
