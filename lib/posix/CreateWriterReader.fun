functor CreateWriterReader (S : sig
                                  type reader
                                  type writer
                                  type vector
                                  type array
                                  type array_slice
                                  type vector_slice
                                  type file_desc = int
                                  val close : file_desc -> unit
                                  structure Error : POSIX_ERROR
                                  datatype whence
                                    = SEEK_SET
                                    | SEEK_CUR
                                    | SEEK_END
                                  structure O :
                                            sig
                                              include BIT_FLAGS
                                              val append : flags
                                              val nonblock : flags
                                              val sync : flags
                                            end
                                  val setfl : file_desc * O.flags -> unit

                                  type pos = Position.int
                                  val lseek : file_desc * Position.int * whence
                                              -> Position.int
                                  val fdToIOD : file_desc -> OS.IO.iodesc

                                  val RD : {
	                            name : string,
	                            chunkSize : int,
	                            readVec : (int -> vector) option,
	                            readArr : (array_slice -> int) option,
	                            readVecNB : (int -> vector option) option,
	                            readArrNB : (array_slice -> int option) option,
	                            block : (unit -> unit) option,
	                            canInput : (unit -> bool) option,
	                            avail : unit -> int option,
	                            getPos : (unit -> pos) option,
	                            setPos : (pos -> unit) option,
	                            endPos : (unit -> pos) option,
	                            verifyPos : (unit -> pos) option,
	                            close : unit -> unit,
	                            ioDesc : OS.IO.iodesc option
                                  } -> reader
                                  val WR : {
	                            name : string,
	                            chunkSize : int,
	                            writeVec : (vector_slice -> int) option,
	                            writeArr : (array_slice -> int) option,
	                            writeVecNB : (vector_slice -> int option) option,
	                            writeArrNB : (array_slice -> int option) option,
	                            block : (unit -> unit) option,
	                            canOutput : (unit -> bool) option,
	                            getPos : (unit -> pos) option,
	                            setPos : (pos -> unit) option,
	                            endPos : (unit -> pos) option,
	                            verifyPos : (unit -> pos) option,
	                            close : unit -> unit,
	                            ioDesc : OS.IO.iodesc option
                                  } -> writer

                                  val readArr : int * array_slice -> int
                                  val readVec : int * int -> vector
                                  val writeArr : int * array_slice -> int
                                  val writeVec : int * vector_slice -> int
                                  val vectorLength : vector -> int
                                end) =
  struct
    open S
    val raiseSys = fn msg => fn opt => raise OS.SysErr (msg, opt)
    fun isreg_ (s : file_desc) : bool = Posix_FileSys.ST.isReg (Posix_FileSys.fstat s)
    fun isReg fd = (isreg_ fd) handle Fail s => raiseSys ("isReg " ^ (Int.toString fd)) NONE

    fun filesize_ (s : file_desc) : Position.int = Posix_FileSys.ST.size (Posix_FileSys.fstat s)
    fun fileSize fd = (filesize_ fd) handle Fail s => raiseSys "filesize" NONE
    exception ClosedStream = IO.ClosedStream

    val bufsize = 4000

    fun posFns (closed, fd) =
        let val pos0 = Position.fromInt 0
        in if (isReg fd)
           then let
             val pos = ref pos0
             fun getPos () = !pos
             fun setPos p = (if !closed
                             then raise ClosedStream
                             else ();
                             pos := lseek(fd,p,SEEK_SET))
             fun endPos () = (if !closed
                              then raise ClosedStream
                              else ();
                              fileSize fd)
             fun verifyPos () = let
               val curPos = lseek(fd, pos0, SEEK_CUR)
             in
               pos := curPos; curPos
             end
             val _ = verifyPos ()
           in
             {pos = pos,
              getPos = SOME getPos,
              setPos = SOME setPos,
              endPos = SOME endPos,
              verifyPos = SOME verifyPos}
           end
           else {pos = ref pos0,
                 getPos = NONE,
                 setPos = NONE,
                 endPos = NONE,
                 verifyPos = NONE}
        end

    fun mkReader {fd, name, initBlkMode} =
        let
          val closed = ref false
          val {pos, getPos, setPos, endPos, verifyPos} =
              posFns (closed, fd)
          val blocking = ref initBlkMode
          fun blockingOn () =
              (setfl(fd, O.flags[]); blocking := true)
          fun blockingOff () =
              (setfl(fd, O.nonblock); blocking := false)
          fun ensureOpen () =
              if !closed then raise ClosedStream else ()
          fun incPos k = pos := Position.+ (!pos, Position.fromInt k)
          val readVec = fn n =>
                           let val v = readVec (fd, n)
                           in incPos (vectorLength v); v
                           end
          val readArr = fn x =>
                           let val k = readArr (fd, x)
                           in incPos k; k
                           end
          fun blockWrap f x =
              (ensureOpen ();
               if !blocking then () else blockingOn ();
               f x)
          fun noBlockWrap f x =
              (ensureOpen ();
               if !blocking then blockingOff () else ();
               (SOME (f x)
                handle (e as OS.SysErr (_, SOME cause)) =>
                       if cause = Error.again then NONE else raise e))
          val close =
           fn () => if !closed then () else (closed := true; close fd)
          val avail =
              if isReg fd
              then fn () => if !closed
                            then SOME 0
                            else SOME (Position.toInt
                                           (Position.-
                                            (fileSize fd,
                                             !pos)))
              else fn () => if !closed then SOME 0 else NONE
        in
          RD {avail = avail,
              block = NONE,
              canInput = NONE,
              chunkSize = bufsize,
              close = close,
              endPos = endPos,
              getPos = getPos,
              ioDesc = SOME (fdToIOD fd),
              name = name,
              readArr = SOME (blockWrap readArr),
              readArrNB = SOME (noBlockWrap readArr),
              readVec = SOME (blockWrap readVec),
              readVecNB = SOME (noBlockWrap readVec),
              setPos = setPos,
              verifyPos = verifyPos}
        end
    fun mkWriter {fd, name, initBlkMode, appendMode, chunkSize} =
        let
          val closed = ref false
          val {pos, getPos, setPos, endPos, verifyPos} =
              posFns (closed, fd)
          fun incPos k = (pos := Position.+ (!pos, Position.fromInt k); k)
          val blocking = ref initBlkMode
          val appendFlgs = O.flags(if appendMode then [O.append] else [])
          fun updateStatus () =
              let
                val flgs = if !blocking
                           then appendFlgs
                           else O.flags [O.nonblock, appendFlgs]
              in
                setfl(fd, flgs)
              end
          fun ensureOpen () =
              if !closed then raise ClosedStream else ()
          fun ensureBlock x =
              if !blocking then () else (blocking := x; updateStatus ())
          fun putV x = incPos (writeVec x)
          fun putA x = incPos (writeArr x)
          fun write (put, block) arg =
              (ensureOpen (); ensureBlock block; put (fd, arg))
          fun handleBlock writer arg =
              SOME(writer arg)
              handle (e as OS.SysErr (_, SOME cause)) =>
                     if cause = Error.again then NONE else raise e
          val close =
           fn () => if !closed then () else (closed := true; close fd)
        in
          WR {block = NONE,
              canOutput = NONE,
              chunkSize = chunkSize,
              close = close,
              endPos = endPos,
              getPos = getPos,
              ioDesc = SOME (fdToIOD fd),
              name = name,
              setPos = setPos,
              verifyPos = verifyPos,
              writeArr = SOME (write (putA, true)),
              writeArrNB = SOME (handleBlock (write (putA, false))),
              writeVec = SOME (write (putV, true)),
              writeVecNB = SOME (handleBlock (write (putV, false)))}
        end
  end