
use  "lists.sml";

val last1 = last_item [1,2,3] = SOME 3
val last2 = last_item ["a","b"] = SOME "b"
val last3 = last_item ["a","b","c"] = SOME "c"

val penultimate1 = penultimate [1,2,3] = SOME 2
val penultimate2 = penultimate ["a","b"] = SOME "a"
val penultimate3 = penultimate ["a","b","c"] = SOME "b"

val nth1 = nth_item ([1,2,3],1) = SOME 1
val nth2 = nth_item (["a","b"],2) = SOME "b"
val nth3 = nth_item (["a","b","c"],3) = SOME "c"

val palin1 = is_palindrome [1,2,3] = false;
val palin2 = is_palindrome [1,2,1]= true;
val palin3 = is_palindrome [1] = true;
val palin4 = is_palindrome [] = true;

val comp1 = compress [1,1,2,3,3,3,3,4,5] = [1,2,3,4,5];
val comp2 = compress [1,2,5]= [1,2,5];
val comp3 = compress [1]= [1];
val comp4 = compress [1,1,2,2,1,1,3]= [1,2,1,3];

val sz1 = size [] = 0;
val sz2 = size [1] = 1;
val sz3 = size ["a","e","i","o","u"] = 5;

val rev1 = reverse [] = [];
val rev2 = reverse [1,2,3] = [3,2,1];
val rev3 = reverse [1] = [1];
val rev4 = reverse ["a","e","i","o","u"] = ["u","o","i","e","a"];

val flat1 = flatten [] = [];
val flat2 = flatten [[1]] = [1];

val encode1 = encode [] = [];
val encode2 = encode [1] = [(1,1)];
val encode3 = encode [1,2,2,3,3,3,4,4,4,4,5,5,6]= [(1,1),(2,2), (3,3),(4,4),(5,2),(6,1)]

val decode1 = [1] = decode (encode [1]);
val decode2 = ["a","b","c"]= decode (encode ["a","b","c"]);
val decode3 = [1]= decode (encode [1]);

val dupli1 = duplicate [1] = [1,1];
val dupli2 = duplicate ["a","b"] = ["a","a","b","b"]

val dupliN1 = duplicateN (1, [2]) = [2];
val dupliN2 = duplicateN (2, [2]) = [2,2];
val dupliN3 = duplicateN (3, [1,2,3]) = [1,1,1,2,2,2,3,3,3];

val drop1 = drop(0,["a","e","i","o","u"]) = ["a","e","i","o","u"];
val drop2 = drop(1, [1,2,3,4]) = [2,4];
val drop3 = drop (2, [1,2,3,4]) = [3];
val drop4 = drop (4, [1,2,3,4]) = [];