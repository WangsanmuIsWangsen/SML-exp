2.
(* mult : int list -> int 		*)
(* REQUIRES: true		*)
(* ENSURES: mult(L) evaluates to the product of the integers in L. *)
fun mult [ ] = 1
    | mult (x ::L) = x*(mult L); 	
3.
(* mult : int list list -> int        *)
(* REQUIRES: true             *)
(* ENSURES: mult(R) evaluates to the product of all the integers in the lists of R. *)
fun Mult []=1
| Mult(r::R) =(mult r)*(Mult R);
4.
(* mult¡¯ : int list * int -> int 			*)
(* REQUIRES: true				*)
(* ENSURES: mult¡¯(L, a) evaluates to the product of the integers in L and a.*)?
fun mult¡¯ ([ ], a) = a
   | mult¡¯ (x :: L, a) = mult¡¯ (L, x * a);

fun Mult' ([],a) = a
   | Mult' (r::R,a)=Mult'(R,(mult r)*a);
5.
(* double : int -> int *)
(* REQUIRES: n >= 0 *)
(* ENSURES: double n evaluates to 2 * n.*)
fun double (0 : int) : int = 0
   | double n = 2 + double (n - 1);

fun square(n:int):int=
if n=0 then 0
else if n>0 then square(n-1)+double(n-1)+1
else square(~n-1)+double(~n-1)+1;
6.
(* divisibleByThree : int -> bool 	*)
(* REQUIRES: true				*)
(* ENSURES: divisibleByThree n evaluates to true if n is a multiple of 3 and to false otherwise *)

fun divisibleByThree (n : int) : bool=
if n=0 then true
else if n=1 then false
else if n=2 then false
else if n<0 then divisibleByThree (n+3)
else divisibleByThree (n-3);
7.
(* oddP: int -> bool 		*)
(* REQUIRES: n >= 0 		*)
(* ENSURES: evenP n evaluates to true iff n is odd*)
fun oddP(0 : int) : bool = false
   | oddP 1 = true
   | oddP n = oddP(n - 2)