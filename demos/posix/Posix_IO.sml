structure Posix_IO = struct
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

  (* val readVec : file_desc * int -> Word8Vector.vector
  val readArr : file_desc * Word8ArraySlice.slice -> int
  val writeVec : file_desc * Word8VectorSlice.slice -> int
  val writeArr : file_desc * Word8ArraySlice.slice -> int *)

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
        fun convertLType (ltype: LockType): lock_type =
          let
            fun toWord (LockType l) = Word.fromInt (Int16.toInt l)
            val ltype = toWord ltype
            val rdlck = toWord LockType.F_RDLCK
            val wrlck = toWord LockType.F_WRLCK
            val unlck = toWord LockType.F_UNLCK
          in
            if Word.andb (ltype, rdlck) = rdlck then
              F_RDLCK
            else if Word.andb (ltype, wrlck) = wrlck then
              F_WRLCK
            else
              F_UNLCK
          end
        fun convertWhence (whence: SeekFlags): whence =
          let
            fun toWord (SeekFlags f) = Word.fromInt (Int16.toInt f)
            val whence = toWord whence
            val set = toWord SeekFlags.SEEK_SET
            val cur = toWord SeekFlags.SEEK_CUR
            val endd = toWord SeekFlags.SEEK_END
          in
            if Word.andb (whence, set) = set then
              SEEK_SET
            else if Word.andb (whence, cur) = cur then
              SEEK_CUR
            else
              SEEK_END
          end
      in
        { ltype = convertLType (!(flock.#l_type))
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
end