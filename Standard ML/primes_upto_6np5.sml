(*
author@Krishna
-->checking only 6N+1 and 6N+5
-->It may get slower because of too many function calls (especially calls to List).
*)

(*Firstly, create a list of numbers*)

val tab = fn n=>List.tabulate(n,fn x=>x)

(*need a function to multiply the above numbers by 6n+1, 6n+5*)

val lst6np1 = map (fn x => 6*x + 1)
val lst6np5 = map (fn x => 6*x +5) (*can improve by re-using 6*x calculation for large integer multiplication.*)

(*
-->apply these functions to the numbers generated from tab
*)
val curry_num1 = lst6np1 o tab (*Awesome feature*)
val curry_num2 = lst6np5 o tab

(*need to add [2,3] in possible_primes because 6n+1/5 doesn't give 2 and 3*)

fun possible_primes (x) = List.concat[curry_num1 x, curry_num2 x]


(*
---> Instead of checking n+2 for odd numbers, now we have a list of possible prime numbers, which will be given by possible_primes(x)@[2,3].
--> Just like before, pass a list of confirmed prime numbers in acc
--> Check to see if possible_primes are indeed prime numbers by dividing those numbers by confirmed prime numbers.
--Ta daa!
*)
fun primes_upto_6np5 (n) =
    let fun divide_by_primes (m,acc)=
	    case acc of
		[] => true
	      | hd::[] => m mod hd <> 0
	      |x::xs' => m mod x <> 0 andalso divide_by_primes(m,xs')
							      
    in
	let fun primes_list (pos_lst, xcc) =
		if tl (pos_lst) <> []
		then case pos_lst of
			 []=>[]
		       | hd::[] => if divide_by_primes(hd, xcc) then xcc@[hd]
				   else xcc
		       | hd::tl => if divide_by_primes(hd,xcc) then primes_list(tl,xcc@[hd])
				   else primes_list(tl, xcc)
		else xcc
	in
	    primes_list(tl(possible_primes(n)),[2,3])
	end
    end


(*tests*)
val curr = fn x => primes_upto_6np5 (x)
val curr1 = curr 1
val curr2 = curr 2
val curr3 = curr 3
val curr4 = curr 4
val curr5 = curr 5
val curr6 = curr 6
val curr7 = curr 7
val curr8 = curr 8
val curr9 = curr 9
val curr10 = curr 10

(* Interesting pattern. Look at the lengths of curr1,curr2,... *)

val lencur1= length(curr1)
val lencur2= length(curr2)
val lencur3= length(curr3)
val lencur4= length(curr4)
val lencur5= length(curr5)
val lencur6= length(curr6)

(* Doubling breaks down after 6. By doubling, I mean that for lencur4, it is 8, lencur6, it is 12 and so on...*)

val lencur7= length(curr7)
val lencur8= length(curr8)

(*Let's look at other curr's. Let me make a list *)
val lencurmap = List.map(fn x=>length( curr x)) [8,9,10,11,12,13,14,15,16,17,18,19,20]

(*Well, nth here. Move along*)
