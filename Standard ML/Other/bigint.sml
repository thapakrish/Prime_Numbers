(* author@Krishna
    -Questions from http://www.cs.nmsu.edu/~gupta/Classes/CS171/Lab5.html
*)


fun to_int_list clst =
    case clst of
	[]=>[]
      | xs::ys => ord(xs)-ord(#"0"):: to_int_list ys

fun to_char_list ilst =
    case ilst of
	[]=>[]
      | xs::ys => chr(xs+ord(#"0"))::to_char_list ys

fun reverse lst =
    if null lst 
    then []
    else reverse (tl lst) @ [hd lst]

fun string_to_list numstr =
    reverse (to_int_list (explode numstr))

fun list_to_string ilist =
    implode(to_char_list (reverse ilist))

fun Hd lst =
    if null lst then 0
    else hd lst

fun Tl lst =
    if null lst then nil
    else tl lst

fun add_carry1 (a, b, carry)=
    let
	val xx = (Hd a) + (Hd b) + carry
    in
	if (null a andalso null b andalso carry=0) then nil
	else
	    (xx div 10) :: add_carry1(Tl a, Tl b, xx mod 10)
    end

(* did it differently*)	

fun add_carry (a,b,carry) =
    case (a,b,carry) of
	([],[],0) => []
      | ([],[],num) => [num]
      | ([],xs::ys,carry) => (xs+carry) mod 10 :: add_carry(a,ys,(xs+carry) div 10)
      | (xs::ys,[],carry) => (xs+carry) mod 10 :: add_carry(ys,b,(xs+carry) div 10)
      | (xs1::ys1,xs2::ys2,carry) => (xs1+xs2+carry) mod 10 :: add_carry(ys1,ys2,(xs1+xs2+carry) div 10)


fun add (lst1, lst2) =
    add_carry(lst1,lst2,0)

fun Add (str1,str2) =
    list_to_string(add(string_to_list str1, string_to_list str2))


fun mult_carry (lst, num, carry)=
    case (lst,num,carry) of
	([],num,0) => []
      | ([],num,carry) => [carry]
      | (xs::ys,num,carry) => (xs*num+carry) mod 10 :: mult_carry(ys,num,(xs*num+carry) div 10)

(* helper functions for multiply*)
fun add_them_all (lsts) =
    case lsts of
	[]=>[]
      | xs::ys => add (xs,add_them_all(ys))

fun pad_zeroes (num) =
    if num = 1 then [0]
    else 0::pad_zeroes (num-1)


(*
    -I implemented it differently
*)
fun multiply (lst1, lst2) =
    let fun get_list (m,n,pad) =
	case (m,n,pad) of
	    (m,[],pad)=> []
	  | (m,xs::ys,0) => mult_carry(lst1,xs,0) ::get_list(m,ys,pad+1)
	  | (m,xs::ys,pad) => (pad_zeroes(pad)@mult_carry(lst1,xs,0)) ::get_list(m,ys,pad+1)
    in
	add_them_all(get_list(lst1,lst2,0))
    end

fun Multiply (str1, str2)=
    list_to_string(multiply(string_to_list str1, string_to_list str2))

