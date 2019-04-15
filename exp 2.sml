1. list->int
fun reverse []=[]
   | reverse (x ::L)=(reverse L)@[x];
2.list*list->list
fun interleave ([]:int list,[]:int list): int list=[]
   | interleave ( [],x::L) =x::(interleave ([],L))
   | interleave (y::L,[])=y::(interleave (L,[]))
   | interleave (x::L,y::M)=x::y::(interleave(L,M));
3.int list->tree
(*取尾部元素*)	
fun last []=0
	|last [x]=x
	|last (x::L) =last L;
(*删除表尾*)	
fun delast [] = []
	| delast [x]=[]
	| delast (x::L)= x ::delast L;	
(*表的划分*)	
fun split [] = ([],1,[])
	| split [x] = ([],x, [])
	| split [x,y]  = ([x],y,[]) 
    | split (x::L) =
	let val (A, t,B) =split (delast L); 	
	in (x::A, t,B@[last L]) 	
	end;
(*设置新的数据类型tree*)
datatype tree = Empty | Node of tree * int * tree;

fun trans([]):tree = Empty
	|	trans(L) = 
		let val (A, t,B) =split L 
		in Node(trans(A),t,trans(B))
		end;

4.tree->tree  //反转
fun revT (Empty):tree=Empty
   | revT(Node(A,x,B))=Node(revT(B),x,revT(A));
5.tree*int->bood
datatype order = GREATER|EQUAL|LESS;
(*中序遍历*)
fun trav Empty = [ ]
   | trav (Node(t1, x, t2)) = trav t1 @ (x :: trav t2);
(*表的有序判断*)
fun isorder []= true
	| isorder [x] = true
	| isorder [x,y] = (case Int.compare(x,y) of GREATER => true | EQUAL => false| LESS => true)
	| isorder [x,y,z] = (case Int.compare(x,y) of 
	GREATER => (case Int.compare(x,z) of GREATER => true | EQUAL => false| LESS => false) 
	| EQUAL => false
	| LESS => (case Int.compare(y,z) of GREATER => false | EQUAL => false| LESS => true))
	| isorder (x::y::L) = if ((Int.compare(x,y) = Int.compare(y,hd(L))) andalso isorder(y::L)) then true else false;
(*查询元素是否存在且有序*)
fun binarySearch(Empty,t:int) :bool = false
	|	binarySearch(Node(A,x,B),t) = 
	if((t = x orelse binarySearch(A,t) orelse binarySearch(A,t)) andalso isorder(trav(Node(A,x,B)))) then true
	else false;