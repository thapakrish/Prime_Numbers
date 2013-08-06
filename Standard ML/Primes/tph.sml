(*
author@Krishna
need to change code for LargeInts.
*)

fun triangular (n) =
    if n > 0 then
	n * (n + 1) div 2 :: triangular (n-1)
    else []

fun pentagonal (n) =
    if n > 0 then
	n * (3* n - 1) div 2 :: pentagonal (n-1)
    else []

fun hexagonal (n) =
    if n > 0 then
	n * (2* n - 1) :: hexagonal (n-1)
    else []

fun compare (a,b,c) =
    let fun compare_two (one, two) =
	    case one of
		[]=>[]
	      | xs::ys => List.filter(fn x=> x = xs) two :: compare_two(ys,two)
    in
	let
	    val aplusb= List.concat (compare_two (a, b))
	    val bplusc = List.concat (compare_two (b,c))
	in
	    (aplusb, bplusc)
	end
    end
								    
fun tph (n) =
    let val tri = triangular (n)
	val pent = pentagonal (n)
	val hex = hexagonal (n)
    in
	compare(tri, pent, hex)
    end
	
	

