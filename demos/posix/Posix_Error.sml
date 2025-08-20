structure Posix_Error: POSIX_ERROR = struct
  type syserror = OS.syserror
  val errorMsg = OS.errorMsg
  val errorName = OS.errorName
  val syserror = OS.syserror

  open Mono.Unix.Native

  val fromWord = fn (w: SysWord.word) =>
    case Errno (SysWord.toInt w) of
      Errno.ENOENT => OS_SysError.noent
    | Errno.EACCES => OS_SysError.acces
    | Errno.EEXIST => OS_SysError.exist
    | Errno.ENOTDIR => OS_SysError.notdir
    | _ => OS_SysError.noent (* TODO: fix this to handle other cases *)

  local fun errnoToWord (Errno e) = SysWord.fromInt e
  in
    fun toWord OS_SysError.noent = errnoToWord Errno.ENOENT
      | toWord OS_SysError.acces = errnoToWord Errno.EACCES
      | toWord OS_SysError.exist = errnoToWord Errno.EEXIST
      | toWord OS_SysError.notdir = errnoToWord Errno.ENOTDIR
  end

  val acces       : syserror = OS_SysError.acces
  val again       : syserror = OS_SysError.again
  val badf        : syserror = OS_SysError.badf
  val badmsg      : syserror = OS_SysError.badmsg
  val busy        : syserror = OS_SysError.busy
  val canceled    : syserror = OS_SysError.canceled
  val child       : syserror = OS_SysError.child
  val deadlk      : syserror = OS_SysError.deadlk
  val dom         : syserror = OS_SysError.dom
  val exist       : syserror = OS_SysError.exist
  val fault       : syserror = OS_SysError.fault
  val fbig        : syserror = OS_SysError.fbig
  val inprogress  : syserror = OS_SysError.inprogress
  val intr        : syserror = OS_SysError.intr
  val inval       : syserror = OS_SysError.inval
  val io          : syserror = OS_SysError.io
  val isdir       : syserror = OS_SysError.isdir
  val loop        : syserror = OS_SysError.loop
  val mfile       : syserror = OS_SysError.mfile
  val mlink       : syserror = OS_SysError.mlink
  val msgsize     : syserror = OS_SysError.msgsize
  val nametoolong : syserror = OS_SysError.nametoolong
  val nfile       : syserror = OS_SysError.nfile
  val nodev       : syserror = OS_SysError.nodev
  val noent       : syserror = OS_SysError.noent
  val noexec      : syserror = OS_SysError.noexec
  val nolck       : syserror = OS_SysError.nolck
  val nomem       : syserror = OS_SysError.nomem
  val nospc       : syserror = OS_SysError.nospc
  val nosys       : syserror = OS_SysError.nosys
  val notdir      : syserror = OS_SysError.notdir
  val notempty    : syserror = OS_SysError.notempty
  val notsup      : syserror = OS_SysError.notsup
  val notty       : syserror = OS_SysError.notty
  val nxio        : syserror = OS_SysError.nxio
  val perm        : syserror = OS_SysError.perm
  val pipe        : syserror = OS_SysError.syserr_pipe
  val range       : syserror = OS_SysError.range
  val rofs        : syserror = OS_SysError.rofs
  val spipe       : syserror = OS_SysError.spipe
  val srch        : syserror = OS_SysError.srch
  val toobig      : syserror = OS_SysError.toobig
  val xdev        : syserror = OS_SysError.xdev
end