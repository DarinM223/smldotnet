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

      type flags = mode
      fun toWord (OpenFlags i): SysWord.word = Word.toLargeWord (Word.fromInt i)
      fun fromWord (w: SysWord.word) = OpenFlags (Word.toInt (Word.fromLargeWord w))
      val all = fromWord (List.foldl SysWord.orb 0wx0 (List.map toWord [append, excl, noctty, sync, trunc]))
      structure BF = BitFlags(type flags = flags val all = all val toWord = toWord val fromWord = fromWord)
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

      type flags = mode
      fun toWord (FilePermissions w): SysWord.word = Word.toLargeWord w
      fun fromWord (w: SysWord.word) = FilePermissions (Word.fromLargeWord w)
      structure BF = BitFlags(type flags = flags val all = all val toWord = toWord val fromWord = fromWord)
      open BF
    end
end