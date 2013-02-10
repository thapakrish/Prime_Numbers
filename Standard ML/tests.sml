(*Krishna Thapa
tests for is_prime*)

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


(*Tests for primes_upto*)
val true2 = primes_upto(2) = [2];
val true5 = primes_upto(5) = [2,3,5];
val true9 = primes_upto(9) = [2,3,5,7];
val true11 = primes_upto(11) = [2,3,5,7,11];
val true13 = primes_upto(13) = [2,3,5,7,11,13];
val true50 = primes_upto(50) = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47];

