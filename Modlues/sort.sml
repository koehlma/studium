load "Int";

structure Sort = struct
    fun split(lx) = foldl (fn (x, (la, lb)) => (x::lb, la)) (nil, nil) lx;

    fun merge(compare)(nil, ly) = ly
      | merge(compare)(lx, nil) = lx
      | merge(compare)(x::lx, y::ly) =
        case compare(x, y) of
              LESS => x::merge compare (lx, y::ly)
            | GREATER => y::merge compare (x::lx, ly)
            | EQULAL => x::merge compare (lx, ly)

    fun msort(compare)(nil) = nil
      | msort(compare)(x::nil) = [x]
      | msort(compare)(lx) =
        let
            val (la, lb) = split(lx)
        in
            merge compare (msort compare la, msort compare lb)
        end
    
    val msortInt = msort Int.compare
    
    val strictMergeSort = msort
    val strictMerge = merge
end;
