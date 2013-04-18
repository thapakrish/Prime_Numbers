(*author@Krishna
--> fn : int->bool
--> Very simple code/ nothing fancy here, just to test my understanding of let expressions.
--> Checks if given integer is prime
--> Gives error to negative numbers
--> Assumption :: user enters number>=2
*)
fun is_prime (n) =
    let fun is_not_divisible (from, to)=
	    if from <= to then
		n mod 2 <> 0 andalso n mod from <> 0 andalso is_not_divisible (from+2,to)
	    else true
    in
	is_not_divisible(3,floor(Math.sqrt (Real.fromInt n))+1)
    end 
	
