structure Level : LEVEL =
struct

type level = int

val level = ref 0

fun topLevel () = !level
fun incr () = level := !level + 1
fun reset () = level := 0
val compare = Int.compare

structure Map:>ORD_MAP where type Key.ord_key=level = IntBinaryMap
end