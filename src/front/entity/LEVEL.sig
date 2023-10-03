signature LEVEL =
sig

eqtype level

val topLevel : unit -> level
val incr : unit -> unit
val reset : unit -> unit
val compare : level * level -> order

end