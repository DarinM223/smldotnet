structure Posix_IO: POSIX_IO = struct
  type file_desc = Posix_FileSys.file_desc
  type pid = int

  datatype whence = SEEK_SET | SEEK_CUR | SEEK_END
  datatype open_mode = datatype Posix_FileSys.open_mode
  datatype lock_type = F_RDLCK | F_WRLCK | F_UNLCK

  fun pipe (): {infd: file_desc, outfd: file_desc} =
    let
      val reading = ref 0
      val writing = ref 0
    in
      if Mono.Unix.Native.Syscall.pipe (&reading, &writing) = ~1
        then raise OS.SysErr ("Error in pipe", NONE)
        else { infd = !reading, outfd = !writing }
    end

  fun dup (desc: file_desc): file_desc =
    let val result = Mono.Unix.Native.Syscall.dup desc
    in if result = ~1 then raise OS.SysErr ("Error in dup", NONE) else result
    end
  fun dup2 {old: file_desc, new: file_desc}: unit =
    if Mono.Unix.Native.Syscall.dup2 (old, new) = ~1
      then raise OS.SysErr ("Error in dup2", NONE)
      else ()
  fun close (desc: file_desc): unit =
    if Mono.Unix.Native.Syscall.close desc = ~1
      then raise OS.SysErr ("Error in close", NONE)
      else ()

  fun readArrWord8 (desc: file_desc, slice: Word8ArraySlice.slice): int =
    let
      open Mono.Unix
      val (arr, off, len) = Word8ArraySlice.base slice
      val stream: UnixStream = UnixStream (desc, false)
      val n' = stream.#Read (SOME arr, off, len)
    in
      (* Errors already handled by UnixStream by throwing exceptions *)
      n'
    end
  fun readArrChar (desc: file_desc, slice: CharArraySlice.slice): int =
    let
      open Mono.Unix
      val (arr, off, len) = CharArraySlice.base slice
      val stream: UnixStream = UnixStream (desc, false)
      val reader = System.IO.StreamReader (stream)
      val n' = reader.#Read (SOME arr, off, len)
    in
      n'
    end

  fun readVecWord8 (desc: file_desc, n: int): Word8Vector.vector =
    let
      val arr: Word8Array.array = Word8Array.array (n, 0w0)
      val slice = Word8ArraySlice.full arr
      val _ = readArrWord8 (desc, slice)
    in
      Word8ArraySlice.vector slice
    end
  fun readVecChar (desc: file_desc, n: int): string =
    let
      val arr: CharArray.array = CharArray.array (n, Char.chr 0)
      val slice: CharArraySlice.slice = CharArraySlice.full arr
      val _ = readArrChar (desc, slice)
    in
      System.String (SOME (Prim.fromVector (CharArraySlice.vector slice)))
    end

  fun writeArrWord8 (desc: file_desc, buf: Word8ArraySlice.slice): int =
    let
      open Mono.Unix
      val stream: UnixStream = UnixStream (desc, false)
      val (arr, off, len) = Word8ArraySlice.base buf
      (* FIXME: Uses UnixStream's method so it doesn't return the bytes written *)
      val () = stream.#Write (SOME arr, off, len)
    in
      len
    end
  fun writeArrChar (desc: file_desc, buf: CharArraySlice.slice): int =
    let
      open Mono.Unix
      val stream: UnixStream = UnixStream (desc, false)
      val writer = System.IO.StreamWriter stream
      val (arr, off, len) = CharArraySlice.base buf
      (* FIXME: Uses StreamWriter's method so it doesn't return the bytes written *)
      val () = writer.#Write (SOME arr, off, len)
    in
      len
    end

  fun writeVecWord8 (desc: file_desc, buf: Word8VectorSlice.slice): int =
    let
      open Mono.Unix
      val stream: UnixStream = UnixStream (desc, false)
      val (vec, off, len) = Word8VectorSlice.base buf
      (* FIXME: Uses UnixStream's method so it doesn't return the bytes written *)
      val () = stream.#Write (SOME (Prim.fromVector vec), off, len)
    in
      len
    end
  fun writeVecChar (desc: file_desc, buf: CharVectorSlice.slice): int =
    let
      open Mono.Unix
      val stream: UnixStream = UnixStream (desc, false)
      val writer = System.IO.StreamWriter (stream)
      val (vec, off, len) = CharVectorSlice.base buf
      (* FIXME: Uses StreamWriter's method so it doesn't return the bytes written *)
      val () = writer.#Write (SOME (Prim.fromVector vec), off, len)
    in
      len
    end

  structure O = Posix_FileSys.O

  structure FD = struct
    type flags = word
    val cloexec = 0wx1
    val all = 0wx1
    val toWord = Word.toLargeWord
    val fromWord = Word.fromLargeWord
    structure BF = BitFlags(type flags = flags val all = all val toWord = toWord val fromWord = fromWord)
    open BF
  end

  fun dupfd {old: file_desc, base: file_desc}: file_desc =
    let
      open Mono.Unix.Native
      val result = Syscall.fcntl (old, FcntlCommand.F_DUPFD, base)
    in
      if result = ~1
        then raise OS.SysErr ("Error in dupfd", NONE)
        else result
    end
  fun getfd (desc: file_desc): FD.flags =
    let
      open Mono.Unix.Native
      val result = Syscall.fcntl (desc, FcntlCommand.F_GETFD)
    in
      if result = ~1
        then raise OS.SysErr ("Error in getfd", NONE)
        else Word.fromInt result
    end
  fun setfd (desc: file_desc, flags: FD.flags): unit =
    let
      open Mono.Unix.Native
      val result = Syscall.fcntl (desc, FcntlCommand.F_SETFD, Word.toInt flags)
    in
      if result = ~1
        then raise OS.SysErr ("Error in setfd", NONE)
        else ()
    end
  fun getfl (desc: file_desc): O.flags * open_mode =
    let
      open Mono.Unix.Native
      val result = Syscall.fcntl (desc, FcntlCommand.F_GETFL)
    in
      if result = ~1 then
        raise OS.SysErr ("Error in getfl", NONE)
      else
        let
          val rdwr = O.toWord OpenFlags.O_RDWR
          val rdonly = O.toWord OpenFlags.O_RDONLY
          val wronly = O.toWord OpenFlags.O_WRONLY
          val result = OpenFlags result
          val result' = O.toWord result
          val openMode =
            if SysWord.andb (result', rdwr) = rdwr then
              O_RDWR
            else if SysWord.andb (result', wronly) = wronly then
              O_WRONLY
            else
              O_RDONLY
        in (result, openMode)
        end
    end

  fun setfl (desc: file_desc, Mono.Unix.Native.OpenFlags flags: O.flags): unit =
    if Mono.Unix.Native.Syscall.fcntl (desc, Mono.Unix.Native.FcntlCommand.F_SETFL, flags) = ~1
      then raise OS.SysErr ("Error in setfl", NONE)
      else ()

  local
    open Mono.Unix.Native
    fun convertWhence SEEK_SET = SeekFlags.SEEK_SET
      | convertWhence SEEK_CUR = SeekFlags.SEEK_CUR
      | convertWhence SEEK_END = SeekFlags.SEEK_END
  in
    fun lseek (desc: file_desc, pos: Position.int, flags: whence): Position.int =
      let val result = Syscall.lseek (desc, pos, convertWhence flags)
      in if result = ~1 then raise OS.SysErr ("Error in lseek", NONE) else result
      end
  end

  fun fsync (desc: file_desc): unit =
    if Mono.Unix.Native.Syscall.fsync desc = ~1
      then raise OS.SysErr ("Error in fsync", NONE)
      else ()

  structure FLock = struct
    open Mono.Unix.Native
    type flock = {ltype: lock_type, whence: whence, start: Position.int, len: Position.int, pid: pid option}

    fun flockToFlock {ltype: lock_type, whence: whence, start: Position.int, len: Position.int, pid: pid option}: Flock =
      let
        fun convertLock F_RDLCK = LockType.F_RDLCK
          | convertLock F_WRLCK = LockType.F_WRLCK
          | convertLock F_UNLCK = LockType.F_UNLCK
        fun convertWhence SEEK_SET = SeekFlags.SEEK_SET
          | convertWhence SEEK_CUR = SeekFlags.SEEK_CUR
          | convertWhence SEEK_END = SeekFlags.SEEK_END
        val f = ref Flock.null
      in
        f.#l_type := convertLock ltype;
        f.#l_whence := convertWhence whence;
        f.#l_start := start;
        f.#l_len := len;
        f.#l_pid := (case pid of NONE => 0 | SOME p => p);
        !f
      end
    fun flockToFlock' (flock: Flock): flock =
      let
        fun convertLock LockType.F_RDLCK = F_RDLCK
          | convertLock LockType.F_WRLCK = F_WRLCK
          | convertLock LockType.F_UNLCK = F_UNLCK
        fun convertWhence SeekFlags.SEEK_SET = SEEK_SET
          | convertWhence SeekFlags.SEEK_CUR = SEEK_CUR
          | convertWhence SeekFlags.SEEK_END = SEEK_END
      in
        { ltype = convertLock (!(flock.#l_type))
        , whence = convertWhence (!(flock.#l_whence))
        , start = !(flock.#l_start)
        , len = !(flock.#l_len)
        , pid = case !(flock.#l_pid) of 0 => NONE | n => SOME n
        }
      end

    fun flock (r: flock) = r
    val ltype: flock -> lock_type = #ltype
    val whence: flock -> whence = #whence
    val start: flock -> Position.int = #start
    val len: flock -> Position.int = #len
    val pid: flock -> pid option = #pid
  end

  fun getlk (desc: file_desc, flock: FLock.flock): FLock.flock =
    let
      open Mono.Unix.Native
      val flock = ref (FLock.flockToFlock flock)
      val result = Syscall.fcntl (desc, FcntlCommand.F_GETLK, &flock)
    in
      if result = ~1
        then raise OS.SysErr ("Error in getlk", NONE)
        else FLock.flockToFlock' (!flock)
    end
  fun setlk (desc: file_desc, flock: FLock.flock): FLock.flock =
    let
      open Mono.Unix.Native
      val flock = ref (FLock.flockToFlock flock)
      val result = Syscall.fcntl (desc, FcntlCommand.F_SETLK, &flock)
    in
      if result = ~1
        then raise OS.SysErr ("Error in setlk", NONE)
        else FLock.flockToFlock' (!flock)
    end
  fun setlkw (desc: file_desc, flock: FLock.flock): FLock.flock =
    let
      open Mono.Unix.Native
      val flock = ref (FLock.flockToFlock flock)
      val result = Syscall.fcntl (desc, FcntlCommand.F_SETLKW, &flock)
    in
      if result = ~1
        then raise OS.SysErr ("Error in setlkw", NONE)
        else FLock.flockToFlock' (!flock)
    end

  structure BinWriter = CreateWriterReader(
    struct
      type reader = BinPrimIO.reader
      type writer = BinPrimIO.writer
      type vector = Word8Vector.vector
      type array = Word8Array.array
      type array_slice = Word8ArraySlice.slice
      type vector_slice = Word8VectorSlice.slice
      type file_desc = file_desc
      datatype whence = datatype whence
      structure O = O
      structure Error = Posix_Error
      type pos = Position.int
      val RD = BinPrimIO.RD
      val WR = BinPrimIO.WR
      val close = close
      val fdToIOD = Posix_FileSys.fdToIOD
      val lseek = lseek
      val readArr = readArrWord8
      val writeArr = writeArrWord8
      val writeVec = writeVecWord8
      val readVec = readVecWord8
      val setfl = setfl
      val vectorLength = Word8Vector.length
    end)

  structure TextWriter = CreateWriterReader(
    struct
      type reader = TextPrimIO.reader
      type writer = TextPrimIO.writer
      type vector = CharVector.vector
      type array = CharArray.array
      type array_slice = CharArraySlice.slice
      type vector_slice = CharVectorSlice.slice
      type file_desc = file_desc
      datatype whence = datatype whence
      structure O = O
      structure Error = Posix_Error
      type pos = Position.int
      val RD = TextPrimIO.RD
      val WR = TextPrimIO.WR
      val close = close
      val fdToIOD = Posix_FileSys.fdToIOD
      val lseek = lseek
      val readArr = readArrChar
      val writeArr = writeArrChar
      val writeVec = writeVecChar
      val readVec = readVecChar
      val setfl = setfl
      val vectorLength = CharVector.length
    end)

  val mkBinReader = BinWriter.mkReader
  val mkBinWriter = BinWriter.mkWriter

  val mkTextReader = TextWriter.mkReader
  val mkTextWriter = TextWriter.mkWriter
  val readVec = readVecWord8
  val writeVec = writeVecWord8
  val readArr = readArrWord8
  val writeArr = writeArrWord8
end