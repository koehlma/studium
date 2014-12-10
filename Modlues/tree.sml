load "Int";
load "List";

use "./sort.sml";
use "./avltree.sml";

structure Tree = struct
	datatype tree = T of tree list;

	fun createBinTree(0) = T []
	  | createBinTree(n) = let val t = createBinTree(n - 1) in T [t, t] end;

	fun breadth(T nil) = 1
	  | breadth(T tx) = foldl op+ 0 (map breadth tx);
	  
	fun size(T tx) = foldl op+ 1 (map size tx);

	fun depth(T ts) = 1 + foldl Int.max ~1 (map depth ts);

	fun mirror(T ts) = T(rev (map mirror ts));

	fun fold(f)(T ts) = f(map (fold f) ts);	

	fun breadth'(ts) = fold (fn nil => 1 | xs => foldl op+ 0 xs) ts;

	fun size'(ts) = fold (fn xs => foldl op+ 1 xs) ts;

	fun depth'(ts) = fold (fn xs => 1 + (foldl Int.max ~1 xs)) ts;

	fun mirror'(ts) = fold (fn xs => T(rev xs)) ts;

	fun ast(t, nil) = SOME t
	  | ast(T ts, a::ar) = ast(List.nth(ts, a - 1), ar) handle Subscript => NONE;

	fun node(t, a) = isSome(ast(t, a));

	fun root(a) = null(a);

	fun leaf(t, a) = ast(t, a) = SOME (T[]);

	fun inner(t, a) =
		let
			val b = ast(t, a)
		in
			isSome(b) andalso b <> SOME (T[])
		end

	fun pred(nil) = raise Subscript
	  | pred(_::nil) = nil
	  | pred(a::ar) = a::pred(ar);

	fun succ(t, a, k) =
		let
			val a = a@[k]
		in
			if node(t, a) then a else raise Subscript
		end

	fun treeCompare(T tx, T ty) = List.collate treeCompare (tx, ty)

	fun treeCompare'(T nil, T nil) = EQUAL
	  | treeCompare'(T (_::_), T nil) = GREATER
	  | treeCompare'(T nil, T(_::_)) = LESS
	  | treeCompare'(T (x::xr), T (y::yr)) =
		case treeCompare'(x, y) of
			  EQUAL => treeCompare'(T xr, T yr)
			| r => r

	fun direct(T nil) = T[]
	  | direct(T (x::nil)) = T[x]
	  | direct(T ts) =
		let
			val (x1, x2) = Sort.split(ts)
			val (x1, x2) = (map direct x1, map direct x2)
		in
			T(Sort.merge treeCompare (x1, x2))
		end

	fun direct'(T ts) = T(AVLTree.traverse(foldl (AVLTree.insort treeCompare) AVLTree.SENTINEL (map direct ts)))

	fun edges(T nil) = 0
	  | edges(T ts) = foldl op+ (length ts) (map edges ts)

	fun edges'(ts) = fold (fn xs => foldl op+ (length xs) xs) ts
end;
