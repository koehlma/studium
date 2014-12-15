structure Std = struct
    fun iter 0 f s = s | iter n f s = iter (n - 1) f (f s)
      
    fun iterup n m f s = if n > m then s else iterup (n + 1) m f (f s)
    
    fun countup n m f s = if n > m then s else countup (n + 1) m f (f (n, s))
    
    fun iterdn n m f s = if n < m then s else iterdn (n - 1) m f (f s)
    
    fun countdn n m f s = if n < m then s else countdn (n - 1) m f (f (n, s))
    
    fun first s f = if f s then s else first (s + 1) f
end;
