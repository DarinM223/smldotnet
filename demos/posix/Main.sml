structure Main = struct
  fun main () =
    let
      open System.IO Mono.Unix
      val path = "testseek"
      val fs = StdioFileStream ("log.txt", "ab")
      val w = StreamWriter fs
      val () = w.#WriteLine ("Log entry: {0}", "Test1")
      val () = w.#Close ()
    in
      print "Hello world\n"
    end
end