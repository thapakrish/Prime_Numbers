(*
--> fn : int->bool
--> Very simple code/ nothing fancy here, just to test my understanding of let expressions.
--> Checks if given integer is prime
--> Gives error to negative numbers
--> Assumption :: user enters number>=3 
*)
fun is_prime(n) =
    let fun is_not_divisible(from, to)=
	    if from < to then
		n mod from <> 0 andalso is_not_divisible (from+1,to)
	    else true
    in
	is_not_divisible(2,floor(Math.sqrt (Real.fromInt 5))+1)
    end 
	
