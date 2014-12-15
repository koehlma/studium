load "Array";
load "Int";

use "std.sml";

structure Heap = struct
    datatype 'a heap = H of ('a * 'a -> order) * 'a Array.array * int
    
    fun root(H (_, a, _)) = Array.sub(a, 0)
    
    fun lastnode(H (_, a, n)) = Array.sub(a, n)
    
    fun key(H (_, a, _), i) = Array.sub(a, i)
    
    fun leftchild(i) = 2 * i
    
    fun rightchild(i) = 2 * i + 1
    
    fun parent(i) = i div 2
    
    fun exists(H (_, _, n), i) = i <= n
    
    fun isleaf(H (_, _, n), i) = i > n div 2
    
    fun swap(a, i, j) = let
            val x = Array.sub(a, i)
            val y = Array.sub(a, j)
        in
            (Array.update(a, i, y); Array.update(a, j, x); a)
        end;
    fun heapify(i, H(c, a, n)) =
        if i <= n div 2 then 
            let
                val rc = rightchild(i)
                val lc = leftchild(i)
                val mc =
                    if rc <= n then
                        if c(Array.sub(a, rc), Array.sub(a, lc)) = GREATER then
                            rc
                        else
                            lc
                    else
                        lc
            in
                if c(Array.sub(a, mc), Array.sub(a, i)) = GREATER then
                    heapify(mc, H(c, swap(a, mc, i), n))
                else
                    H(c, a, n)
            end
        else
            H(c, a, n)
    
    fun makeHeap(H (c, a, n)) = Std.countdn (n div 2) 0 heapify (H(c, a, n)) 
    
    fun fromList c l = makeHeap(H(c, Array.fromList l, length(l) - 1))
end;

structure Sort = struct
    fun split(lx) = foldl (fn (x, (la, lb)) => (x::lb, la)) (nil, nil) lx;
    
    fun insort(_)(a, nil) = [a]
      | insort(cf)(a, x::xs) = if cf(a, x) = GREATER then x::(insort cf (a, xs)) else a::x::xs
    
    fun insertionSort(compare)(lx) = foldl (insort compare) nil lx
    
    fun heapSort(compare)(lx) =
        let
            val Heap.H(_, a, _) = Std.countdn (length lx - 1) 1 (fn (n, Heap.H(_, a, _)) => Heap.heapify(0, Heap.H(compare, Heap.swap(a, 0, n), n - 1))) (Heap.fromList(compare)(lx))
        in
           Std.countdn (Array.length a - 1) 0 (fn (i, lx) => (Array.sub(a, i))::lx) nil
        end                
    
    fun strictMerge(compare)(nil, ly) = ly
      | strictMerge(compare)(lx, nil) = lx
      | strictMerge(compare)(x::lx, y::ly) =
        case compare(x, y) of
              LESS => x::strictMerge compare (lx, y::ly)
            | GREATER => y::strictMerge compare (x::lx, ly)
            | EQUAL => x::strictMerge compare (lx, ly)

    fun strictMergeSort(_)(nil) = nil
      | strictMergeSort(_)(x::nil) = [x]
      | strictMergeSort(c)(lx) =
        let
            val (la, lb) = split(lx)
        in
            strictMerge c (strictMergeSort c la, strictMergeSort c lb)
        end
    
    val strictMergeSortInt = strictMergeSort Int.compare
    
end;


val heap = Heap.fromList Int.compare [9, 16, 14, 7, 5, 3, 18, 19, 12, 27, 24, 20];
Heap.root(heap);
Heap.lastnode(heap);
Heap.isleaf(heap, 5);
Sort.heapSort Int.compare [9, 16, 14, 7, 5, 3, 18, 19, 12, 27, 24, 20];
