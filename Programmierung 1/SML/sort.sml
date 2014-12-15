load "Int";

structure Sort = struct
    fun split(lx) = foldl (fn (x, (la, lb)) => (x::lb, la)) (nil, nil) lx;
    
    fun insort(_)(a, nil) = [a]
      | insort(cf)(a, x::xs) = if cf(a, x) = GREATER then x::(insort cf (a, xs)) else a::x::xs
    
    fun insertionSort(compare)(lx) = foldl (insort compare) nil lx
    
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
