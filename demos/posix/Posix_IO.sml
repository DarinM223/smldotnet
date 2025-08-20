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


  (*
  	public enum FcntlCommand : int {
		// Form /usr/include/bits/fcntl.h
		F_DUPFD      =    0, // Duplicate file descriptor.
		F_GETFD      =    1, // Get file descriptor flags.
		F_SETFD      =    2, // Set file descriptor flags.
		F_GETFL      =    3, // Get file status flags.
		F_SETFL      =    4, // Set file status flags.
		F_GETLK      =   12, // Get record locking info. [64]
		F_SETLK      =   13, // Set record locking info (non-blocking). [64]
		F_SETLKW     =   14, // Set record locking info (blocking). [64]
		F_OFD_GETLK  =   36, // Get open file description locking info.
		F_OFD_SETLK  =   37, // Set open file description locking info (non-blocking).
		F_OFD_SETLKW =   38, // Set open file description locking info (blocking).
		F_SETOWN     =    8, // Set owner of socket (receiver of SIGIO).
		F_GETOWN     =    9, // Get owner of socket (receiver of SIGIO).
		F_SETSIG     =   10, // Set number of signal to be sent.
		F_GETSIG     =   11, // Get number of signal to be sent.
		F_NOCACHE    =   48, // OSX: turn data caching off/on for this fd.
		F_SETLEASE   = 1024, // Set a lease.
		F_GETLEASE   = 1025, // Enquire what lease is active.
		F_NOTIFY     = 1026, // Required notifications on a directory
		F_ADD_SEALS  = 1033, // Add seals.
		F_GET_SEALS  = 1034, // Get seals.
	}
   *)

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
end