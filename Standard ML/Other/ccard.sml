(* 
    - Does not handle large ints,yet
    - Questions from http://www.cis.upenn.edu/~cis194/hw/01-intro.pdf
*)

fun toDigits num =
    let
	val remainder = num mod 10
	val quotient = num div 10
    in
	if quotient > 9 then
	   toDigits(quotient)@[remainder]
	else
	    [quotient]@[remainder]
    end

fun doubleEveryOther lst =
    let fun leaveMe lst1 =
	    case lst1 of
		[]=>[]
	      | x::y => x::doubleEveryOther y
    in
	case lst of
	    []=>[]
	   |xs::ys  => 	2*xs :: leaveMe ys
    end
	
fun sumDigits lst =
    case lst of
	[]=> 0
      | xs::ys => xs mod 10 +xs div 10+sumDigits ys


fun validate num =
    let
	val toD= toDigits num
	val dEo= doubleEveryOther toD
	val sumD = sumDigits dEo
    in
	sumD mod 10 = 0	
    end


fun hanoi (n, from, to, spare)=
    if n = 1 then [(from,to)]
    else Hanoi(n-1,from,spare,to) @ ((from,to)::Hanoi(n-1,spare,to,from))
