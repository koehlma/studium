load "Int";

structure AVLTree = struct
    datatype 'a avltree = AVL of 'a * 'a avltree * 'a avltree * int | SENTINEL
    
    fun depth(SENTINEL) = 0
      | depth(AVL(_, _, _, d)) = d

    fun leftmost(SENTINEL) = SENTINEL
      | leftmost(AVL (v, SENTINEL, rt, d)) = AVL(v, SENTINEL, rt, d)
      | leftmost(AVL (_, lt, _, _)) = leftmost(lt)

    fun rightmost(SENTINEL) = SENTINEL
      | rightmost(AVL (v, lt, SENTINEL, d)) = AVL(v, lt, SENTINEL, d)
      | rightmost(AVL (_, _, rt, _)) = rightmost(rt)

    fun rotateLeft(SENTINEL) = SENTINEL
      | rotateLeft(AVL(v, lt, SENTINEL, d)) = AVL(v, lt, SENTINEL, d)
      | rotateLeft(AVL(v, lt, AVL(v', lt', rt', _), _)) =
        let 
            val lt = AVL(v, lt, lt', Int.max(depth lt , depth lt') + 1)
        in
            AVL(v', lt, rt', Int.max(depth lt, depth rt') + 1)
        end

    fun rotateRight(SENTINEL) = SENTINEL
      | rotateRight(AVL(v, SENTINEL, rt, n)) = AVL(v, SENTINEL, rt, n)
      | rotateRight(AVL(v, AVL(v', lt', rt', _), rt, _)) =
        let
            val rt = AVL(v, rt', rt, Int.max(depth rt', depth rt) + 1)
        in
            AVL(v', lt', rt, Int.max(depth lt', depth rt) + 1)
        end

    fun balance(AVL(v, lt, rt, d)) =
        case depth(rt) - depth(lt) of
              2 =>
                (case rt of
                      AVL(_, _, _, ~1) => rotateLeft(AVL(v, lt, rotateRight(rt), d))
                    | AVL(_, _, _, _) => rotateLeft(AVL(v, lt, rt, d)))
            | ~2 =>
                (case lt of
                      AVL(_, _, _, 1) => rotateRight(AVL(v, rotateLeft(lt), rt, d))
                    | AVL(_, _, _, _) => rotateRight(AVL(v, lt, rt, d)))
            | _ => AVL(v, lt, rt, d)

    fun insort(compare)(x, SENTINEL) = AVL(x, SENTINEL, SENTINEL, 0)
      | insort(compare)(x, AVL(v, lt, rt, d)) =
        case compare(x, v) of
              LESS =>
                let
                    val t = insort(compare)(x, lt)
                    val d = Int.max(depth rt, depth t) + 1
                in
                    balance(AVL(v, t, rt, d))
                end
            | GREATER =>
                let
                    val t = insort(compare)(x, rt)
                    val d = Int.max(depth lt, depth t) + 1
                in
                    balance(AVL(v, lt, t, d))
                end
            | EQUAL => AVL(v, lt, rt, d)
            
    fun delete(compare)(x, SENTINEL) = SENTINEL
      | delete(compare)(x, AVL(v, lt, rt, _)) =
        case compare(x, v) of
              LESS => 
                let
                    val t = delete(compare)(x, lt)
                    val d = Int.max(depth rt, depth t) + 1
                in
                    balance(AVL(v, t, rt, d))
                end
            | GREATER =>
                let
                    val t = delete(compare)(x, rt)
                    val d = Int.max(depth lt, depth t) + 1
                in
                    balance(AVL(v, lt, t, d))
                end
            | EQUAL =>
                case (lt, rt) of
                      (SENTINEL, SENTINEL) => SENTINEL
                    | (lt, SENTINEL) => lt
                    | (SENTINEL, rt) => rt
                    | (lt, rt) =>
                        let
                            val (AVL (v, _, rx, _)) = leftmost(rt)
                            val t = delete(compare)(v, rt)
                            val d = Int.max(depth lt, depth t) + 1
                        in
                            balance(AVL(v, lt, t, d))
                        end

    fun traverse(SENTINEL) = nil
      | traverse(AVL (v, lt, rt, _)) = (traverse lt)@[v]@(traverse rt)
      
    fun search(compare)(x, SENTINEL) = NONE
      | search(compare)(x, AVL(v, lt, rt, h)) =
        case compare(x, v) of 
              LESS => search compare (x, lt)
            | GREATER => search compare (x, rt)
            | EQUAL => SOME (AVL(v, lt, rt, h))
  
    val intInsort = insort(Int.compare)
    val intDelete = delete(Int.compare)
end;
