
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


fun last_item (lst) =
    case lst of
	[]=> NONE
      | xs::[] => SOME xs
      | xs::ys => last_item(ys)

fun penultimate (lst) =
    case lst of
	[]=> NONE
      | xs::[] => NONE
      | xs::ys::[] => SOME xs
      | xs::ys::zs => penultimate(ys::zs)
				       
fun nth_item (lst, n) =
    case (lst, n) of
	([],num) => NONE
      | (xs::ys, 1) => SOME xs
      | (xs::ys, num) => nth_item(ys, num-1)

fun size (lst) =
    case lst of
	[]=> 0
      | xs::ys => 1 + size (ys)

fun reverse (lst) =
    case lst of
	[]=> []
      | xs::ys => reverse(ys) @ [xs]


fun is_palindrome (lst) =
    lst = reverse(lst)


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


fun compress (lst) =
    case lst of
	[]=> []
      | xs::ys::zs => if xs = ys then compress (ys::zs)
		      else xs::compress(ys::zs)
				       
      | xs::ys => xs::compress(ys)


(*
fun pack (lst) =
    case lst of
	[]=> []
     | xs::ys::zs => if xs = ys then xs :: pack (ys::zs)
		     else [xs] @ pack (ys::zs)
     | xs::ys => if xs = ys then [xs]
		 else [ys]
		
*)




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

fun encode (lst) =
    let fun compare (from, to, count) =
	    case from of
		[]=> [[to,count]]
	      | xs::ys => if xs = to then compare (ys, to, count+1)
			  else [[to, count]] @ encode(xs::ys) 
    in
	if null lst then []
	else compare (lst, hd lst, 0)
    end
(*	
fun encode_modified (lst) =
*)
