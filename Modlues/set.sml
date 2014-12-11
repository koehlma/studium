load "Int";
load "List";

use "./sort.sml";

structure Set = struct
    datatype 'a set = S of ('a * 'a -> order) * 'a list
        
    fun fromList(c)(l) = S(c, (Sort.strictMergeSort c l))    
    
    fun toList(S (_, l)) = l
    
    fun member(x, S(c, l)) = List.exists (fn v => c(x, v) = EQUAL) l
    
    fun empty(S(_, l)) = null(l)
    
    fun listUnion(c, lx, ly) = Sort.strictMerge c (lx, ly)
    
    fun union(S(c, lx), S(_, ly)) = S(c, listUnion(c, lx, ly))
    
    fun listIntersection(_, _, nil) = nil
      | listIntersection(_, nil, _) = nil
      | listIntersection(c, x::lx, y::ly) =
        case c(x, y) of
              EQUAL => x::listIntersection(c, lx, ly)
            | LESS => listIntersection(c, lx, y::ly) 
            | GREATER => listIntersection(c, x::lx, ly)
            
    fun intersection(S(c, lx), S(_, ly)) = S(c, listIntersection(c, lx, ly))
    
    fun listDifference(_, l, nil) = l
      | listDifference(_, nil, _) = nil
      | listDifference(c, x::lx, y::ly) =
        case c(x, y) of
              EQUAL => listDifference(c, lx, ly)
            | LESS => x::listDifference(c, lx, y::ly)
            | GREATER => listDifference(c, x::lx, ly)
    
    fun difference(S(c, lx), S(_, ly)) = S(c, listDifference(c, lx, ly))
    
    fun listEq(c, nil, nil) = true
      | listEq(c, x::lx, y::ly) = c(x, y) = EQUAL andalso listEq(c, lx, ly)
      | listEq(_, _, _) = false
    
    fun eq(S(c, lx), S(_, ly)) = listEq(c, lx, ly)
    
    fun listSubset(c, nil, _) = true
      | listSubset(c, _, nil) = false
      | listSubset(c, x::lx, y::ly) = 
        case c(x, y) of
              EQUAL => listSubset(c, lx, ly)
            | LESS => false
            | GREATER => listSubset(c, x::lx, ly)
    
    fun subset(S(c, lx), S(_, ly)) = listSubset(c, lx, ly)
    
    fun listAdd(c, x, nil) = [x]
      | listAdd(c, x, y::ly) =
        case c(x, y) of
              LESS => x::y::ly
            | GREATER => y::listAdd(c, x, ly)
            | EQUAL => y::ly
    
    fun add(S(c, l), x) = S(c, listAdd(c, x, l))
    
    fun listDelete(c, x, nil) = nil
      | listDelete(c, x, y::ly) =
        case c(x, y) of
              GREATER => y::listDelete(c, x, ly)
            | _ => ly
    
    fun delete(S(c, l), x) = S(c, listDelete(c, x, l))
    
    val fromIntList = fromList Int.compare
end;
