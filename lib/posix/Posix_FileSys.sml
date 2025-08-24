structure Posix_FileSys : POSIX_FILE_SYS = struct
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
  fun fdToIOD (x: file_desc) : OS.IO.iodesc = x
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

    fun chmod (path, perms: S.mode): unit =
      if Mono.Unix.Native.Syscall.chmod (path, perms) = ~1
        then raise OS.SysErr ("Error in chmod", NONE)
        else ()
    fun fchmod (desc: file_desc, perms: S.mode): unit =
      if Mono.Unix.Native.Syscall.fchmod (desc, perms) = ~1
        then raise OS.SysErr ("Error in fchmod", NONE)
        else ()
    fun chown (path: string, uid: uid, gid: gid): unit =
      if Mono.Unix.Native.Syscall.chown (path, uid, gid) = ~1
        then raise OS.SysErr ("Error in chown", NONE)
        else ()
    fun fchown (desc: file_desc, uid: uid, gid: gid): unit =
      if Mono.Unix.Native.Syscall.fchown (desc, uid, gid) = ~1
        then raise OS.SysErr ("Error in fchown", NONE)
        else ()
    fun ftruncate (desc: file_desc, pos: Position.int): unit =
      if Mono.Unix.Native.Syscall.ftruncate (desc, pos) = ~1
        then raise OS.SysErr ("Error in ftruncate", NONE)
        else ()

    local
      open Mono.Unix.Native
      fun convertName "CHOWN_RESTRICTED" = PathconfName._PC_CHOWN_RESTRICTED
        | convertName "LINK_MAX" = PathconfName._PC_LINK_MAX
        | convertName "MAX_CANON" = PathconfName._PC_MAX_CANON
        | convertName "MAX_INPUT" = PathconfName._PC_MAX_INPUT
        | convertName "NAME_MAX" = PathconfName._PC_NAME_MAX
        | convertName "NO_TRUNC" = PathconfName._PC_NO_TRUNC
        | convertName "PATH_MAX" = PathconfName._PC_PATH_MAX
        | convertName "PIPE_BUF" = PathconfName._PC_PIPE_BUF
        | convertName "VDISABLE" = PathconfName._PC_VDISABLE
        | convertName "ASYNC_IO" = PathconfName._PC_ASYNC_IO
        | convertName "SYNC_IO" = PathconfName._PC_SYNC_IO
        | convertName "PRIO_IO" = PathconfName._PC_PRIO_IO
        | convertName s = raise OS.SysErr ("Invalid pathconf name: " ^ s, NONE)
    in
      fun pathconf (path: string, property: string): SysWord.word option =
        let
          val result = Syscall.pathconf (SOME path, convertName property)
          val Errno lastError = Stdlib.GetLastError ()
        in
          if result = ~1 then
            (if lastError = 0 then NONE
             else raise OS.SysErr ("Error with pathconf", NONE)) (* TODO convert errno into syserr code? *)
          else SOME (SysWord.fromLargeInt result)
        end
      fun fpathconf (desc: file_desc, property: string): SysWord.word option =
        let
          val result = Syscall.fpathconf (desc, convertName property)
          val Errno lastError = Stdlib.GetLastError ()
        in
          if result = ~1 then
            (if lastError = 0 then NONE
             else raise OS.SysErr ("Error with fpathconf", NONE)) (* TODO convert errno into syserr code? *)
          else SOME (SysWord.fromLargeInt result)
        end
    end

    type ino = SysWord.word
    fun wordToIno x = x
    fun inoToWord x = x

    type dev = SysWord.word
    fun wordToDev x = x
    fun devToWord x = x

    fun utime (path: string, NONE) =
          if Mono.Unix.Native.Syscall.utime path = ~1
            then raise OS.SysErr ("Error in utime", NONE)
            else ()
      | utime (path: string, SOME {actime, modtime}) =
          let
            open Mono.Unix.Native
            val buf = ref Utimbuf.null
            val () = (buf.#actime: Utimbuf.actime) := Time.toSeconds actime
            val () = (buf.#modtime: Utimbuf.modtime) := Time.toSeconds modtime
          in
            (* Pass buf by reference address *)
            if Mono.Unix.Native.Syscall.utime (path, &buf) = ~1
              then raise OS.SysErr ("Error in utime", NONE)
              else ()
          end

    structure ST = struct
      open Mono.Unix.Native
      type stat = Stat

      fun isFileType (mode: S.mode, typ: S.mode) =
        SysWord.andb (S.toWord mode, S.toWord FilePermissions.S_IFMT) = S.toWord typ
      fun mode (stat: stat): S.mode = !(stat.#st_mode)
      fun isDir (stat: stat): bool = isFileType (mode stat, FilePermissions.S_IFDIR)
      fun isChr (stat: stat): bool = isFileType (mode stat, FilePermissions.S_IFCHR)
      fun isBlk (stat: stat): bool = isFileType (mode stat, FilePermissions.S_IFBLK)
      fun isReg (stat: stat): bool = isFileType (mode stat, FilePermissions.S_IFREG)
      fun isFIFO (stat: stat): bool = isFileType (mode stat, FilePermissions.S_IFIFO)
      fun isLink (stat: stat): bool = isFileType (mode stat, FilePermissions.S_IFLNK)
      fun isSock (stat: stat): bool = isFileType (mode stat, FilePermissions.S_IFSOCK)
      fun ino (stat: stat): ino = !(stat.#st_ino)
      fun dev (stat: stat): dev = !(stat.#st_dev)
      fun nlink (stat: stat): int = SysWord.toInt (!(stat.#st_nlink))
      fun uid (stat: stat): uid = Word.toInt (!(stat.#st_uid))
      fun gid (stat: stat): gid = Word.toInt (!(stat.#st_gid))
      fun size (stat: stat): Position.int = !(stat.#st_size)
      fun atime (stat: stat): Time.time = Time.fromSeconds (!(stat.#st_atime))
      fun mtime (stat: stat): Time.time = Time.fromSeconds (!(stat.#st_mtime))
      fun ctime (stat: stat): Time.time = Time.fromSeconds (!(stat.#st_ctime))
    end

    fun stat (path: string): ST.stat =
      let
        open Mono.Unix.Native
        val statResult = ref Stat.null
        val result = Syscall.stat (path, &statResult)
      in
        if result = ~1
          then raise OS.SysErr ("Error in stat", NONE)
          else !statResult
      end
    fun lstat (path: string): ST.stat =
      let
        open Mono.Unix.Native
        val statResult = ref Stat.null
        val result = Syscall.lstat (path, &statResult)
      in
        if result = ~1
          then raise OS.SysErr ("Error in lstat", NONE)
          else !statResult
      end
    fun fstat (desc: file_desc): ST.stat =
      let
        open Mono.Unix.Native
        val statResult = ref Stat.null
        val result = Syscall.fstat (desc, &statResult)
      in
        if result = ~1
          then raise OS.SysErr ("Error in fstat", NONE)
          else !statResult
      end
end