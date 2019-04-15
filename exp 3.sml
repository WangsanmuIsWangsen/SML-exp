1. thenAddone ((int->int)*int)->int
fun Double(x)=2*x;
fun sqrt(x)=x*x;
fun factorial(0)=1
   | factorial(x)=x*factorial(x-1);
fun thenAddone(f,x:int):int=f(x)+1;
测试样例：thenAddone (factorial,5);
2.mapList (('a -> 'b) * 'a list) -> 'b list
fun mapList(f,[])=[]
   | mapList(f,y::L)=f(y)::mapList(f,L);
测试样例：mapList (Double,[1,2,3,4]);
3.mapList' (‘a -> ‘b) -> (‘a list -> ‘b list)
fun mapList' f = fn L => 
		  case L of
          [ ] => [ ]
          | x::R => (f x)::(mapList' f R);
测试样例：mapList' sqrt [1,2,3,4];
4.findOdd int list->int option;
fun findOdd ([]):int option=NONE
   |findOdd (x::L)=
   if (x mod 2)=1(*奇数*) then SOME x
   else findOdd(L);
5.subsetSumOption int list * int -> int list option
fun foldr F z [ ] = z
    | foldr F z (x::L) = F(x, foldr F z L);
fun sum L = foldr (op +) 0 L;
fun map f [ ] = [ ]
    | map f (x::R) = (f x) :: (map f R);
fun sublists [ ] = [ [ ] ]
    | sublists (x::R) =
	let
       val S = sublists R
    in
    S @ map ( fn A => x::A) S
end;
fun Find ([],x)=[]
   | Find(((L:int list)::(R:int list list)),x)=
   if (sum  L)=x then L
   else Find(R,x);
fun subsetSumOption([],x:int):int list option=NONE
   |subsetSumOption(L,x)=
    if (sum(Find(sublists(L),x)))=x then SOME (Find(sublists(L),x))
    else NONE;

6.exists: (‘a -> bool) -> ‘a list -> bool
  forall: (‘a -> bool) -> ‘a list -> bool
fun isEven x:bool=
   if (x mod 2)=0 then true
   else false;
fun exists f []=false
   |exists f (x::L)=
   if f(x) then true
   else (exists f L);
fun forall f []=true
   |forall f (x::L)=
   if f(x) andalso (forall f L) then true
   else false;
7.treeFilter (‘a -> bool) -> ‘a tree -> ‘a option tree
datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree;
fun treeFilter f Empty=Empty
   |treeFilter f (Node(A,x,B))=
   if f(x) then Node(treeFilter f (A),SOME x,treeFilter f (B))
   else Node(treeFilter f (A),NONE,treeFilter f (B));
treeFilter isEven (Node(Node(Empty,2,Empty),1,Node(Empty,3,Empty)));