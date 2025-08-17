structure Main = struct
  fun main () =
    let
      val path = "."
      val dirstream = Posix.FileSys.opendir path
      fun go s =
        case Posix.FileSys.readdir s of
          SOME file => (print (file ^ "\n"); go s)
        | NONE => (print "Done\n"; Posix.FileSys.closedir s)
      val blah = Posix.FileSys.access ("hello", [Posix.FileSys.A_READ])
      val () = print ("Blah: " ^ Bool.toString blah ^ "\n")
      (* open System.IO Mono.Unix
      val path = "testseek"
      val fs = StdioFileStream ("log.txt", "ab")
      val w = StreamWriter fs
      val () = w.#WriteLine ("Log entry: {0}", "Test1")
      val () = w.#Close () *)
    in
      go dirstream
    end
end