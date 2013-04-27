
(*
signature PrologLists =
sig
exception BadNum
val lastitem : 'a list -> 'a
end
    

structure problems99 :> PrologLists =
struct

exception BadNum
*)


(*1.01*)

fun last_item (lst) =
    case lst of
	[]=> NONE
      | xs::[] => SOME xs
      | xs::ys => last_item(ys)

(*1.02*)

fun penultimate (lst) =
    case lst of
	[]=> NONE
      | xs::[] => NONE
      | xs::ys::[] => SOME xs
      | xs::ys::zs => penultimate(ys::zs)

(*1.03*)
				       
fun nth_item (lst, n) =
    case (lst, n) of
	([],num) => NONE
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

(*1.06*)

fun is_palindrome (lst) =
    lst = reverse(lst)

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
      | xs::ys::zs => if xs = ys then compress (ys::zs)
		      else xs::compress(ys::zs)
				       
      | xs::ys => xs::compress(ys)


fun compress2 (lst) =
    let fun compare (from, to) =
	    case from of
		[]=> []
	      | xs::ys => if xs = to then compare(ys, to)
			  else xs :: compress2(ys)
    in
	if null lst then [] else
	compare(lst, hd lst)
    end

(*1.09*)

(*
fun pack (lst) =
    case lst of
	[]=> []
     | xs::ys::zs => if xs = ys then xs :: pack (ys::zs)
		     else [xs] @ pack (ys::zs)
     | xs::ys => if xs = ys then [xs]
		 else [ys]
		
*)




(*1.10*)
fun encode (lst) =
    let fun compare (from, to, count) =
	    case from of
		[]=> [(to,count)]
	      | xs::ys => if xs = to then compare (ys, to, count+1)
			  else [(to, count)] @ encode(xs::ys) 
    in
	if null lst then []
	else compare (lst, hd lst, 0)
    end

(*1.11*)
(*	
fun encode_modified (lst) =
*)


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
