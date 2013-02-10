use "is_prime_impr1.sml";

(* author@Krishna
--> Fibonacci numbers
--> Assumption::n>=1
*)
fun simple_fib (n) =
    if n = 0 then 0
    else if n = 1 then 1
    else simple_fib(n-1)+simple_fib(n-2)
(*
--> Using memoization for faster calculation.
--> Use earlier results
--> Gives nth Fibonacci number
*)
fun memoize_fib (n) =
    if n = 0 then 0
    else if n = 1 then 1
    else
	let fun fib_sequence (m, acc) =
		if m = n then hd(tl (acc))
		else fib_sequence(m+1,tl(acc)@[hd(acc)+hd(tl(acc))])
	in
	    fib_sequence (2,[1,1])
	end
(*
-->Fib sequence upto nth Fibonacci number
-->Tail recursion
*)

fun fib_sequence (n) =
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
	    
(*
-->Prime numbers among the  Fibonacci numbers.
-->How cool is that?
*)

fun prime_fib (n) = List.filter(fn x => is_prime_impr1(x)) (fib_sequence(n))


(*test*)

val pf20 = prime_fib(20)
val pf40 = prime_fib(40)
val pf44 = prime_fib(44)
val last_pf44 = List.last(fib_sequence(44)) 

(*Sweet!*)
