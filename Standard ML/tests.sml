
(*tests for is_prime*)
(*primes*)
val true3 = is_prime(3)=true;
val true5 = is_prime(5)=true;
val true7 = is_prime(7)=true;
val true11 = is_prime(11)=true;
val true13 = is_prime(13)=true;
val true911 = is_prime(911)=true;

(*non-primes*)
val false4 = is_prime(4)=false;
val false6 = is_prime(6)=false;
val false8 = is_prime(8)=false;
val false10 = is_prime(10)=false;
val false12 = is_prime(12)=false;
val false14 = is_prime(14)=false;
val false1000 = is_prime(1000)=false;
