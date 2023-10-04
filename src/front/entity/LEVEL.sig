signature LEVEL =
sig

eqtype level

val topLevel : unit -> level
val incr : unit -> unit
val reset : unit -> unit
val compare : level * level -> order

structure Map:ORD_MAP where type Key.ord_key=level

end