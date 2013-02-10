use "is_prime_impr1.sml";

(*Krishna Thapa
--> int->int list
--> gives prime factors of a give number
--> can further improve by lowering the prime_below argument
*)
fun prime_factors (n)=
    let 
	val lst = primes_below (n+1)
    in
	List.filter(fn x => n mod x = 0 ) lst
    end
