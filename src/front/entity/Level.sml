structure Level : LEVEL =
struct

type level = int

val level = ref 0

fun topLevel () = !level
val rootLevel = 0
fun incr () = level := !level + 1
fun reset () = level := 0
val compare = Int.compare
val toString = Int.toString

structure Map:>ORD_MAP where type Key.ord_key=level = IntBinaryMap
end