structure Main = struct
  fun main () =
    let
      val s = Posix.FileSys.stat "log.txt"
              handle _ => (
                Posix.FileSys.creat ("log.txt", Posix.FileSys.S.irwxu);
                Posix.FileSys.stat "log.txt"
              )
      val () = print ("Is Directory: " ^ Bool.toString (Posix.FileSys.ST.isDir s) ^ "\n")
      val () = print ("Is Chr: " ^ Bool.toString (Posix.FileSys.ST.isChr s) ^ "\n")
      val () = print ("Is Blk: " ^ Bool.toString (Posix.FileSys.ST.isBlk s) ^ "\n")
      val () = print ("Is Reg: " ^ Bool.toString (Posix.FileSys.ST.isReg s) ^ "\n")
      val () = print ("Is FIFO: " ^ Bool.toString (Posix.FileSys.ST.isFIFO s) ^ "\n")
      val () = print ("Is Link: " ^ Bool.toString (Posix.FileSys.ST.isLink s) ^ "\n")
      val () = print ("Is Sock: " ^ Bool.toString (Posix.FileSys.ST.isSock s) ^ "\n")
      val () = print ("# Links: " ^ Int.toString (Posix.FileSys.ST.nlink s) ^ "\n")
      val () = print ("Uid: " ^ Int.toString (Posix.FileSys.ST.uid s) ^ "\n")
      val () = print ("Gid: " ^ Int.toString (Posix.FileSys.ST.gid s) ^ "\n")
      val () = print ("Size: " ^ Position.toString (Posix.FileSys.ST.size s) ^ "\n")
      val () = print ("Access time: " ^ Time.toString (Posix.FileSys.ST.atime s) ^ "\n")
    in
      ()
    end
end