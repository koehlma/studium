fun whileloop g f = if g() then (f(); whileloop g f) else ()
fun forloop s g f = if g() then (f(); s(); forloop s g f) else ()

val i = ref 0;
val _ = forloop (fn () => i := !i + 1) (fn () => !i < 10) (fn () => ());
val _ = whileloop (fn () => !i < 30) (fn () => i := !i * 2);

val t = !i;

