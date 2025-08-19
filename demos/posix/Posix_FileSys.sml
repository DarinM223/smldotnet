structure Posix_FileSys = struct
  open System.IO Mono.Unix
  type dirstream = OS.FileSys.dirstream
  val opendir = OS.FileSys.openDir
  val readdir = OS.FileSys.readDir
  val rewinddir = OS.FileSys.rewindDir
  val closedir = OS.FileSys.closeDir
  val chdir = OS.FileSys.chDir
  val getcwd = OS.FileSys.getDir

  datatype access_mode = datatype OS.FileSys.access_mode
  val access = OS.FileSys.access

  type uid = int
  type gid = int
  type file_desc = int

  (* Hacks because Mono.Posix doesn't have fileno implemented *)
  val stdin = 0
  val stdout = 1
  val stderr = 2

  fun iodToFD (x: OS.IO.iodesc) : file_desc option = SOME x
  fun fdtoIOD (x: file_desc) : OS.IO.iodesc = x
  fun wordToFD (x: SysWord.word) : file_desc = SysWord.toInt x
  fun fdToWord (x: file_desc) : SysWord.word = SysWord.fromInt x

  structure O =
    struct
      open Mono.Unix.Native
      type mode = OpenFlags
      val append   = OpenFlags.O_APPEND
      val excl     = OpenFlags.O_EXCL
      val noctty   = OpenFlags.O_NOCTTY
      val nonblock = OpenFlags.O_NONBLOCK
      val sync     = OpenFlags.O_SYNC
      val trunc    = OpenFlags.O_TRUNC

      fun toWord (OpenFlags i): SysWord.word = Word.toLargeWord (Word.fromInt i)
      fun fromWord (w: SysWord.word) = OpenFlags (Word.toInt (Word.fromLargeWord w))
      val all = fromWord (List.foldl SysWord.orb 0wx0 (List.map toWord [append, excl, noctty, sync, trunc]))
      structure BF = BitFlags(type flags = mode val all = all val toWord = toWord val fromWord = fromWord)
      open BF
    end

  structure S =
    struct
      open Mono.Unix.Native
      type mode = FilePermissions
      val irwxu = FilePermissions.S_IRWXU
      val irusr = FilePermissions.S_IRUSR
      val iwusr = FilePermissions.S_IWUSR
      val ixusr = FilePermissions.S_IXUSR
      val irwxg = FilePermissions.S_IRWXG
      val irgrp = FilePermissions.S_IRGRP
      val iwgrp = FilePermissions.S_IWGRP
      val ixgrp = FilePermissions.S_IXGRP
      val irwxo = FilePermissions.S_IRWXO
      val iroth = FilePermissions.S_IROTH
      val iwoth = FilePermissions.S_IWOTH
      val ixoth = FilePermissions.S_IXOTH
      val isuid = FilePermissions.S_ISUID
      val isgid = FilePermissions.S_ISGID

      val all   = FilePermissions.ALLPERMS

      fun toWord (FilePermissions w): SysWord.word = Word.toLargeWord w
      fun fromWord (w: SysWord.word) = FilePermissions (Word.fromLargeWord w)
      structure BF = BitFlags(type flags = mode val all = all val toWord = toWord val fromWord = fromWord)
      open BF
    end

    datatype open_mode = O_RDONLY | O_WRONLY | O_RDWR

    local
      open Mono.Unix.Native
      fun convertMode O_RDONLY = OpenFlags.O_RDONLY
        | convertMode O_WRONLY = OpenFlags.O_WRONLY
        | convertMode O_RDWR = OpenFlags.O_RDWR
    in
      fun openf (path: string, openMode: open_mode, flags: O.flags): file_desc =
        let val flags = O.flags [convertMode openMode, flags]
        in Syscall.open (path, flags)
        end

      fun createf (path: string, openMode: open_mode, flags: O.flags, perms: S.mode): file_desc =
        let val flags = O.flags [convertMode openMode, flags, OpenFlags.O_CREAT]
        in Syscall.open (path, flags, perms)
        end

      val creat = Syscall.creat
      val umask = Syscall.umask
    end
    fun link {old : string, new : string}: unit =
      if Mono.Unix.Native.Syscall.link (old, new) = ~1
        then raise OS.SysErr ("Error in link", NONE)
        else ()
    fun mkdir (t : string * S.mode): unit =
      if Mono.Unix.Native.Syscall.mkdir t = ~1
        then raise OS.SysErr ("Error in mkdir", NONE)
        else ()
    fun mkfifo (t: string * S.mode): unit =
      if Mono.Unix.Native.Syscall.mkfifo t = ~1
        then raise OS.SysErr ("Error in mkfifo", NONE)
        else ()
    fun unlink (path: string): unit =
      if Mono.Unix.Native.Syscall.unlink path = ~1
        then raise OS.SysErr ("Error in unlink", NONE)
        else ()
    fun rmdir (path: string): unit =
      if Mono.Unix.Native.Syscall.rmdir path = ~1
        then raise OS.SysErr ("Error in rmdir", NONE)
        else ()
    fun rename {old : string, new : string}: unit =
      if Mono.Unix.Native.Syscall.rename (old, new) = ~1
        then raise OS.SysErr ("Error in rename", NONE)
        else ()
    fun symlink {old : string, new : string}: unit =
      if Mono.Unix.Native.Syscall.symlink (old, new) = ~1
        then raise OS.SysErr ("Error in symlink", NONE)
        else ()

    (* Max path length for Windows *)
    val MAXPATHLEN = Word.toLargeWord 0w260

    fun readlink (path: string): string =
      let
        val builder = System.Text.StringBuilder ()
      in
        if Mono.Unix.Native.Syscall.readlink (SOME path, SOME builder, MAXPATHLEN) = ~1
          then raise OS.SysErr ("Error in readlink", NONE)
          else Prim.unsafeValOf (builder.#ToString ())
      end
end