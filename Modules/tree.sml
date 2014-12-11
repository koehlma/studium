load "Int";
load "List";

use "./sort.sml";

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

	fun foldBreadth(ts) = fold (fn nil => 1 | xs => foldl op+ 0 xs) ts;

	fun foldSize(ts) = fold (fn xs => foldl op+ 1 xs) ts;

	fun foldDepth(ts) = fold (fn xs => 1 + (foldl Int.max ~1 xs)) ts;

	fun foldMirror(ts) = fold (fn xs => T(rev xs)) ts;

	fun ast(t, nil) = SOME t
	  | ast(T ts, a::ar) = ast(List.nth(ts, a - 1), ar) handle Subscript => NONE;

	fun node(t, a) = isSome(ast(t, a));

	fun root(a) = null(a);

	fun leaf(t, a) = ast(t, a) = SOME (T[]);

	fun inner(t, a) =
		let
			val n = ast(t, a)
		in
			isSome(n) andalso n <> SOME (T[])
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

	fun compare(T tx, T ty) = List.collate compare (tx, ty)

	fun compare'(T nil, T nil) = EQUAL
	  | compare'(T (_::_), T nil) = GREATER
	  | compare'(T nil, T(_::_)) = LESS
	  | compare'(T (x::xr), T (y::yr)) =
		case compare'(x, y) of
			  EQUAL => compare'(T xr, T yr)
			| r => r
	
	
	fun direct(T nil) = T[]
	  | direct(T (x::nil)) = T[x]
	  | direct(T ts) =
		let
			val (x1, x2) = Sort.split(ts)
			val (x1, x2) = (map direct x1, map direct x2)
		in
			T(Sort.strictMerge compare (x1, x2))
		end

	fun edges(T nil) = 0
	  | edges(T ts) = foldl op+ (length ts) (map edges ts)

	fun edges'(ts) = fold (fn xs => foldl op+ (length xs) xs) ts
end;
