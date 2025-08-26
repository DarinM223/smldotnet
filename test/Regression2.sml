
structure Regression2 = struct
  fun main () =
    let
      val f = ref (print "hello\n")
      val () = f := print "world\n"
    in
      ()
    end
end