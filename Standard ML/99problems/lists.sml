(*
author@Krishna

Problems from https://sites.google.com/site/prologsite/prolog-problems/1

Instead of built in functions, I'll be using recursion, let expressions, and pattern matching as much as possible.

*)

structure PrologLists :> PROBLEMS99 =
struct

exception EmptyList
exception BadNum of int

(*1.01*)

fun last_item (lst) =
    case lst of
	[]=> raise EmptyList
      | xs::[] => SOME xs
      | xs::ys => last_item(ys)

(*1.02*)

fun penultimate (lst) =
    case lst of
	[]=> raise EmptyList
      | xs::[] => NONE
      | xs::ys::[] => SOME xs
      | xs::ys::zs => penultimate(ys::zs)

(*1.03*)
				       
fun nth_item (lst, n) =
    case (lst, n) of
	([],num) => raise Empty
      | (xs::ys, 1) => SOME xs
      | (xs::ys, num) => nth_item(ys, num-1)

(*1.04*)

fun size (lst) =
    case lst of
	[]=> 0
      | xs::ys => 1 + size (ys)

(*1.05*)

fun reverse (lst) =
    case lst of
	[]=> []
      | xs::ys => reverse(ys) @ [xs]

(*helper function to avoid polyEqual warnings*)
fun equals (a,b) =
    a=b

(*1.06*)

fun is_palindrome (lst) =
    equals(lst, reverse(lst))

(*1.07*)

fun flatten (lst) =
    let fun unpack (lsts) =
		 case lsts of
		     []=> []
		   | x::y => x :: unpack y
    in
	case lst of
	    []=> []
	  | xs::ys => unpack xs @ flatten ys
    end

(*1.08*)

fun compress (lst) =
    case lst of
	[]=> []
      | xs::ys::zs => if equals (xs, ys) then compress (ys::zs)
		      else xs::compress(ys::zs)
				       
      | xs::ys => xs::compress(ys)


fun compress2 (lst) =
    let fun compare (from, to) =
	    case from of
		[]=> []
	      | xs::ys => if equals(xs, to) then compare(ys, to)
			  else xs :: compress2(ys)
    in
	if null lst then [] else
	compare(lst, hd lst)
    end

(*1.09*)

fun pack (lst) =
    let fun compare (from,to,acc) =
	    case from of
		[]=> acc::[]
	      | xs::ys => if equals(xs, to) then compare (ys, xs, to::acc)
			  else acc::compare (ys,xs,[xs])
    in
	if null lst then []
	else compare (lst, hd lst,[])
    end 
		

(*1.10*)
fun encode (lst) =
    let fun compare (from, to, count) =
	    case from of
		[]=> [(to,count)]
	      | xs::ys => if equals (xs, to) then compare (ys, to, count+1)
			  else [(to, count)] @ encode(xs::ys) 
    in
	if null lst then []
	else compare (lst, hd lst, 0)
    end

(*1.11*)

fun encode_modified (lst) =
    let fun traverse (packed_list) = 
	    case packed_list of
		[]=> []
	      | xs::ys => (hd xs, size xs):: traverse ys
    in
	traverse (pack lst)
    end



(*helper function*)
fun repeat (item, count) =
	if count = 0 then []
	else item::repeat(item, count-1)

(*1.12*)

fun decode (lst) =
    case lst of
	[]=> []
      | xs::ys => repeat xs @decode(ys)


(*1.14*)

fun duplicate (lst) =
    case lst of
	[]=> []
      | xs::ys => xs::[xs]@duplicate(ys)

(*1.15*)

fun duplicateN (N,lst) =
    case (N, lst) of
	(_,[])=>[]
      | (num, xs::ys) => repeat(xs, num)@duplicateN(num, ys)

(*1.16*)

fun drop (N, lst) =
    let fun helper ( M, sth) =
	    case sth of
		[]=> []
	      | xs::ys => if M = 0 then xs :: drop (N, ys)
			  else helper (M-1,ys)
    in
	helper(N, lst)
    end


(*1.17*)
(*TODO::edit it for N = 0*)

fun split (lst, N) =
    let fun dissect (sth, acc, M) =
	    case sth of
		[]=>[]
	      | xs::ys => if M > 0 then dissect (ys, acc@[xs], M-1)
			  else [acc] @ [sth]
    in
	if null lst then []
	else dissect (lst, [], N)
    end



(*1.18*)

fun slice (n1,n2,lst) =
    if null lst then []						 
    else if n1 > 1 then slice(n1-1,n2-1,tl lst)
    else let fun chop (n2, sth) =
		 if n2 < 1 then []
		 else hd sth :: chop (n2-1, tl sth)
	 in
	     chop(n2,lst)
	 end

	     
(*1.19*)

fun rotate (lst, N) =
    let fun shift_items (M, sth, acc) =
	    case sth of
		[]=>[]
	      | xs::ys => if M > 0 then shift_items (M-1, ys, acc@[xs])
			  else sth@acc
    in
	if N = 0 then lst
	else shift_items (N, lst, [])
    end


(*1.20*)
	
fun remove_at(lst, N) =
(*
    if N < 1 then raise BadNum (N)
*)
    case lst of
	[]=> []
      | xs::ys => if N > 1 then xs:: remove_at(ys, N-1)		  
		  else ys
			       
	
(*1.21*)

fun insert_at(alpha, lst, N) =
    case lst of
	[]=> if N = 1 then [alpha] else []
      | xs::ys => if N > 1 then xs :: insert_at(alpha, ys, N-1)
		  else alpha :: lst
				    
				 
(*1.22*)
fun range (num1, num2) =
    if num1<num2 then num1::range(num1+1,num2)
    else [num2]


(*1.23*)
fun rnd_select (lst, num) =
    let
	val x = Random.rand(num, size lst)
	val y = Random.randRange (1, size lst) x
	val item = valOf(nth_item(lst, y))
    in
	if num > 0 then
	    item :: rnd_select(List.filter(fn x=> x <> item) lst, num-1)
	else []
    end	

end

(*1.24*)
