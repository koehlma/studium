load "String";

use "./avltree.sml";

structure Dict = struct
    datatype 'a entry = E of string * 'a option
    
    fun entryCompare(E (s1, _), E (s2, _)) = String.compare(s1, s2)
    
    fun create() = AVLTree.create entryCompare
    
    fun set(t, k, v) = AVLTree.insort(t, E(k, SOME v))
    
    fun get(t, k) =
        case AVLTree.search(t, E(k, NONE)) of
              SOME (AVLTree.N ((E (_, v)), _, _, _)) => v
            | _ => NONE
    
    fun del(t, k) = AVLTree.delete(t, E(k, NONE))
end;
