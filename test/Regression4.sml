structure Regression4 = struct
  fun I i = (i - 1) before print ("Running I\n")
  fun J j = ~j before print ("Running J\n")
  fun I' (i: Int64.int) = (i - 1) before print ("Running I\n")
  fun J' (j: Int64.int) = ~j before print ("Running J\n")
  fun testInt () =
    let
      val i = I 0
      val j = J 2
      val k = 2 * ~2
      val l = Int.div (2, ~2)
    in
      print ("I: " ^ Int.toString i ^ "\n");
      print ("J: " ^ Int.toString j ^ "\n");
      print ("K: " ^ Int.toString k ^ "\n");
      print ("L: " ^ Int.toString l ^ "\n")
    end
  fun testLong () =
    let
      val i: Int64.int = I' 0
      val j: Int64.int = J' 2
      val k: Int64.int = 2 * ~2
      val l: Int64.int = Int64.div (2, ~2)
    in
      print ("I: " ^ Int64.toString i ^ "\n");
      print ("J: " ^ Int64.toString j ^ "\n");
      print ("K: " ^ Int64.toString k ^ "\n");
      print ("L: " ^ Int64.toString l ^ "\n")
    end
  fun main () =
    ( print "Testing ints\n"
    ; testInt ()
    ; print "Testing longs\n"
    ; testLong ()
    )
end