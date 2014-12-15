load "Int";
load "Real";
load "String";

use "./avltree.sml";
use "./tree.sml";

structure Dict = struct
    datatype ('a, 'b) entry = E of 'a * 'b option
    
    fun create(c) = AVLTree.create (fn (E (k1, _), E (k2, _)) => c(k1, k2))
    
    fun set(t, k, v) = AVLTree.insort(t, E(k, SOME v))
    
    fun get(t, k) =
        case AVLTree.search(t, E(k, NONE)) of
              SOME (AVLTree.N ((E (_, v)), _, _, _)) => v
            | _ => NONE
    
    fun del(t, k) = AVLTree.delete(t, E(k, NONE))
    
    fun createString() = create String.compare
    fun createInt() = create Int.compare
    fun createReal() = create Real.compare
    fun createTree() = create Tree.compare
end;
