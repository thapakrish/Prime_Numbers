
use  "lists.sml";

val a = last_item [1,2,3] = SOME 3
val b = last_item ["a","b"] = SOME "b"
val c = last_item ["a","b","c"] = SOME "c"

val d = penultimate [1,2,3] = SOME 2
val e = penultimate ["a","b"] = SOME "a"
val f = penultimate ["a","b","c"] = SOME "b"

val g = nth_item ([1,2,3],1) = SOME 1
val h = nth_item (["a","b"],2) = SOME "b"
val i = nth_item (["a","b","c"],3) = SOME "c"

val j = is_palindrome [1,2,3] = false;
val k = is_palindrome [1,2,1]= true;
val l = is_palindrome [1] = true;

val M = compress [1,1,2,3,3,3,3,4,5] = [1,2,3,4,5];
val N = compress [1,2,5]= [1,2,5];
val O = compress [1]= [1];
val P = compress [1,1,2,2,1,1,3]= [1,2,1,3];
