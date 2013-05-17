(*
Problems from https://sites.google.com/site/prologsite/prolog-problems/4
*)

datatype 'a bt = Null | Node of 'a * 'a bt * 'a bt

val ex_bt= Node(1,Null,Node(2,Node(3,Null,Null),Null));
