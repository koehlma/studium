load "Int";
load "Real";

use "./tree.sml";

structure AVLTree = struct
    datatype 'a avlnode = N of 'a * 'a avlnode * 'a avlnode * int | SENTINEL
    datatype 'a avltree = T of ('a * 'a -> order) * 'a avlnode
    
    fun create(c) = T(c, SENTINEL)
    
    fun depth(SENTINEL) = 0
      | depth(N(_, _, _, d)) = d

    fun leftmost(SENTINEL) = SENTINEL
      | leftmost(N (v, SENTINEL, rt, d)) = N(v, SENTINEL, rt, d)
      | leftmost(N (_, lt, _, _)) = leftmost(lt)

    fun rightmost(SENTINEL) = SENTINEL
      | rightmost(N (v, lt, SENTINEL, d)) = N(v, lt, SENTINEL, d)
      | rightmost(N (_, _, rt, _)) = rightmost(rt)

    fun rotateLeft(SENTINEL) = SENTINEL
      | rotateLeft(N (v, lt, SENTINEL, d)) = N(v, lt, SENTINEL, d)
      | rotateLeft(N (v, lt, N(v', lt', rt', _), _)) =
        let 
            val lt = N(v, lt, lt', Int.max(depth lt , depth lt') + 1)
        in
            N(v', lt, rt', Int.max(depth lt, depth rt') + 1)
        end

    fun rotateRight(SENTINEL) = SENTINEL
      | rotateRight(N(v, SENTINEL, rt, n)) = N(v, SENTINEL, rt, n)
      | rotateRight(N(v, N(v', lt', rt', _), rt, _)) =
        let
            val rt = N(v, rt', rt, Int.max(depth rt', depth rt) + 1)
        in
            N(v', lt', rt, Int.max(depth lt', depth rt) + 1)
        end

    fun balance(N(v, lt, rt, d)) =
        case depth(rt) - depth(lt) of
              2 =>
                (case rt of
                      N(_, _, _, ~1) => rotateLeft(N(v, lt, rotateRight(rt), d))
                    | N(_, _, _, _) => rotateLeft(N(v, lt, rt, d)))
            | ~2 =>
                (case lt of
                      N(_, _, _, 1) => rotateRight(N(v, rotateLeft(lt), rt, d))
                    | N(_, _, _, _) => rotateRight(N(v, lt, rt, d)))
            | _ => N(v, lt, rt, d)

    fun insortNode(_, x, SENTINEL) = N(x, SENTINEL, SENTINEL, 0)
      | insortNode(c, x, N(v, lt, rt, d)) = 
        case c(x, v) of
              LESS =>
                let
                    val t = insortNode(c, x, lt)
                    val d = Int.max(depth rt, depth t) + 1
                in
                    balance(N(v, t, rt, d))
                end
            | GREATER =>
                let
                    val t = insortNode(c, x, rt)
                    val d = Int.max(depth lt, depth t) + 1
                in
                    balance(N(v, lt, t, d))
                end
            | EQUAL => N(v, lt, rt, d)
    
    fun insort(T (c, r), x) = T(c, insortNode(c, x, r))
    
    fun deleteNode(_, _, SENTINEL) = SENTINEL
      | deleteNode(c, x, N(v, lt, rt, _))= 
        case c(x, v) of
              LESS => 
                let
                    val t = deleteNode(c, x, lt)
                    val d = Int.max(depth rt, depth t) + 1
                in
                    balance(N(v, t, rt, d))
                end
            | GREATER =>
                let
                    val t = deleteNode(c, x, rt)
                    val d = Int.max(depth lt, depth t) + 1
                in
                    balance(N(v, lt, t, d))
                end
            | EQUAL =>
                case (lt, rt) of
                      (SENTINEL, SENTINEL) => SENTINEL
                    | (lt, SENTINEL) => lt
                    | (SENTINEL, rt) => rt
                    | (lt, rt) =>
                        let
                            val (N (v, _, rx, _)) = leftmost(rt)
                            val t = deleteNode(c, v, rt)
                            val d = Int.max(depth lt, depth t) + 1
                        in
                            balance(N(v, lt, t, d))
                        end
    
    fun delete(T (c, r), x) = T(c, deleteNode(c, x, r))
    
    fun foldNode(f)(s)(SENTINEL) = s
      | foldNode(f)(s)(N (v, lt, rt, _)) = foldNode f (f (foldNode f s lt, v)) rt
    
    fun fold(f)(s)(T (_, r)) = foldNode f s r
    
    fun traverseNode(SENTINEL) = nil
      | traverseNode(N (v, lt, rt, _)) = (traverseNode lt)@[v]@(traverseNode rt)
    
    fun traverse(T (_, r)) = traverseNode(r)
      
    fun searchNode(c, x, SENTINEL) = NONE
      | searchNode(c, x, N(v, lt, rt, h)) =
        case c(x, v) of 
              LESS => searchNode(c, x, lt)
            | GREATER => searchNode(c, x, rt)
            | EQUAL => SOME (N(v, lt, rt, h))
    
    fun search(T (c, r), x) = searchNode(c, x, r)
    
    fun createInt() = create(Int.compare)
    fun createReal() = create(Real.compare)
    fun createTree() = create(Tree.compare)
    
    fun sum(t) = fold op+ 0 t
    fun prod(t) = fold op* 1 t
end;
