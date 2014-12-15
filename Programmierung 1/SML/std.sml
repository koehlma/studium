structure Std = struct
    fun iter 0 f s = s | iter n f s = iter (n - 1) f (f s)
      
    fun iterup n m f s = if n > m then s else iterup (n + 1) m f (f s)
    
    fun iterdn n m f s = if n < m then s else iterdn (n - 1) m f (f s)
    
    fun first s f = if f s then s else first (s + 1) f
end;


