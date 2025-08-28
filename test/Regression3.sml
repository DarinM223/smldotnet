structure Regression3 = struct
  fun main () =
    let
      val f = ref ()
      val g = ref (print "hello\n")
      val () = f := !g
      val g = ref ((), 1, ())
      val () = g := ((), 2, ())
      val h = ref { a = (), b = 1, c = () }
      val () = h := { a = (), b = 2, c = (print "world\n"; ()) }
    in
      ()
    end
end