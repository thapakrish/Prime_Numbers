(*author@Krishna
*)

fun reverse_number (num) = (* painful way of reversing a num*)
    let fun make_list (num, acc) =
	    if num > 10 then
		make_list (num div 10, acc @ [num mod 10])
	    else
		acc @ [num]
    in
	if num < 10 then [num]
	else make_list (num, [])
    end

fun num_rev (num) = (* int-> int, much easier*)
    let
	val x = Int.toString num
	val y = explode x
	val z = implode (rev y)
    in
	valOf (Int.fromString z)
    end


fun maybe_palindrome (num) =
    let fun iterate (n, iter) =
	    if iter > 50 then false
	    else
		if n = num_rev(n) then true
		else iterate (num_rev(n) + n, iter+1)
    in
	iterate(num, 0)
    end


fun lychrel (from, to) = (*int*int -> int*)
    if from< to then
	if maybe_palindrome(from) then
	    [from] @ lychrel(from+1,to)
	else lychrel(from+1,to)
    else []
	     


