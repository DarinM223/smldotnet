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
end