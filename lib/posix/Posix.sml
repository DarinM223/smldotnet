structure Posix = struct
  structure Error = Posix_Error
  structure FileSys = Posix_FileSys
  structure ProcEnv = Posix_ProcEnv
  structure IO = Posix_IO
end