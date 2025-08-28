structure Regression5 = struct
  fun main () =
    let
      (* Doesn't check for overflow for words *)
      val w: Word64.word = 0w5000
      val w' = w * w * w + w * w * w
      val () = print ("Word: " ^ Word64.toString w' ^ "\n");
      (* Raises overflow for ints *)
      val i: int = 5000
      val i' = i * i * i + i * i * i
    in
      print ("Int: " ^ Int.toString i' ^ "\n")
    end
end