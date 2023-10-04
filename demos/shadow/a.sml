signature HELLO = sig
  val foo : int
end

structure Hello : HELLO = struct
  val foo = 2
end
