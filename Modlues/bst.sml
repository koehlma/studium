structure BST = struct
    datatype 'a bst = BST of 'a * 'a bst * 'a bst | SENTINEL;

    fun leftmost(SENTINEL) = SENTINEL
      | leftmost(BST (v, SENTINEL, rt)) = BST(v, SENTINEL, rt)
      | leftmost(BST (_, lt, _)) = leftmost(lt)

    fun rightmost(SENTINEL) = SENTINEL
      | rightmost(BST (v, lt, SENTINEL)) = BST(v, lt, SENTINEL)
      | rightmost(BST (_, _, rt)) = rightmost(rt)

    fun insort(compare)(x, SENTINEL) = BST(x, SENTINEL, SENTINEL)
      | insort(compare)(x, BST(v, lt, rt)) =
        case compare(x, v) of
              LESS => BST(v, insort(compare)(x, lt), rt)
            | GREATER => BST(v, lt, insort(compare)(x, rt))
            | EQUAL => BST(v, lt, rt)

    fun delete(compare)(x, SENTINEL) = SENTINEL
      | delete(compare)(x, BST(v, lt, rt)) =
        case compare(x, v) of
              LESS => BST(v, delete(compare)(x, lt), rt)
            | GREATER => BST(v, lt, delete(compare)(x, rt))
            | EQUAL =>
                case (lt, rt) of
                      (SENTINEL, SENTINEL) => SENTINEL
                    | (lt, SENTINEL) => lt
                    | (SENTINEL, rt) => rt
                    | (lt, rt) =>
                        let
                            val (BST (v, lx, rx)) = leftmost(rt)
                        in
                            BST(v, lt, delete(compare)(v, rt))
                        end
end;
