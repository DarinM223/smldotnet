signature HELLO = sig
  include HELLO
  val bar : string
end

structure Hello : HELLO = struct
  open Hello
  val bar = "bar"

  fun main () = print ("Foo: " ^ Int.toString foo ^ " Bar: " ^ bar ^ "\n")
end
