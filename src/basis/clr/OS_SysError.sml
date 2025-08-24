structure OS_SysError = struct
  (* Use POSIX names here just for convention's sake *)
  datatype syserror
    = acces
    | again
    | badf
    | badmsg
    | busy
    | canceled
    | child
    | deadlk
    | dom
    | exist
    | fault
    | fbig
    | inprogress
    | intr
    | inval
    | io
    | isdir
    | loop
    | mfile
    | mlink
    | msgsize
    | nametoolong
    | nfile
    | nodev
    | noent
    | noexec
    | nolck
    | nomem
    | nospc
    | nosys
    | notdir
    | notempty
    | notsup
    | notty
    | nxio
    | perm
    | syserr_pipe
    | range
    | rofs
    | spipe
    | srch
    | toobig
    | xdev
  exception SysErr of (string * syserror option)

  fun errorMsg noent = "No such file or directory"
    | errorMsg acces = "Permission denied"
    | errorMsg exist = "File exists"
    | errorMsg notdir = "Not a directory"
    | errorMsg _ = "Other type of error"

  fun syserror "acces"        = SOME acces
    | syserror "again"        = SOME again
    | syserror "badf"         = SOME badf
    | syserror "badmsg"       = SOME badmsg
    | syserror "busy"         = SOME busy
    | syserror "canceled"     = SOME canceled
    | syserror "child"        = SOME child
    | syserror "deadlk"       = SOME deadlk
    | syserror "dom"          = SOME dom
    | syserror "exist"        = SOME exist
    | syserror "fault"        = SOME fault
    | syserror "fbig"         = SOME fbig
    | syserror "inprogress"   = SOME inprogress
    | syserror "intr"         = SOME intr
    | syserror "inval"        = SOME inval
    | syserror "io"           = SOME io
    | syserror "isdir"        = SOME isdir
    | syserror "loop"         = SOME loop
    | syserror "mfile"        = SOME mfile
    | syserror "mlink"        = SOME mlink
    | syserror "msgsize"      = SOME msgsize
    | syserror "nametoolong"  = SOME nametoolong
    | syserror "nfile"        = SOME nfile
    | syserror "nodev"        = SOME nodev
    | syserror "noent"        = SOME noent
    | syserror "noexec"       = SOME noexec
    | syserror "nolck"        = SOME nolck
    | syserror "nomem"        = SOME nomem
    | syserror "nospc"        = SOME nospc
    | syserror "nosys"        = SOME nosys
    | syserror "notdir"       = SOME notdir
    | syserror "notempty"     = SOME notempty
    | syserror "notsup"       = SOME notsup
    | syserror "notty"        = SOME notty
    | syserror "nxio"         = SOME nxio
    | syserror "perm"         = SOME perm
    | syserror "pipe"         = SOME syserr_pipe
    | syserror "range"        = SOME range
    | syserror "rofs"         = SOME rofs
    | syserror "spipe"        = SOME spipe
    | syserror "srch"         = SOME srch
    | syserror "toobig"       = SOME toobig
    | syserror "xdev"         = SOME xdev
    | syserror _ = NONE

  fun errorName acces       = "acces"
    | errorName again       = "again"
    | errorName badf        = "badf"
    | errorName badmsg      = "badmsg"
    | errorName busy        = "busy"
    | errorName canceled    = "canceled"
    | errorName child       = "child"
    | errorName deadlk      = "deadlk"
    | errorName dom         = "dom"
    | errorName exist       = "exist"
    | errorName fault       = "fault"
    | errorName fbig        = "fbig"
    | errorName inprogress  = "inprogress"
    | errorName intr        = "intr"
    | errorName inval       = "inval"
    | errorName io          = "io"
    | errorName isdir       = "isdir"
    | errorName loop        = "loop"
    | errorName mfile       = "mfile"
    | errorName mlink       = "mlink"
    | errorName msgsize     = "msgsize"
    | errorName nametoolong = "nametoolong"
    | errorName nfile       = "nfile"
    | errorName nodev       = "nodev"
    | errorName noent       = "noent"
    | errorName noexec      = "noexec"
    | errorName nolck       = "nolck"
    | errorName nomem       = "nomem"
    | errorName nospc       = "nospc"
    | errorName nosys       = "nosys"
    | errorName notdir      = "notdir"
    | errorName notempty    = "notempty"
    | errorName notsup      = "notsup"
    | errorName notty       = "notty"
    | errorName nxio        = "nxio"
    | errorName perm        = "perm"
    | errorName syserr_pipe = "pipe"
    | errorName range       = "range"
    | errorName rofs        = "rofs"
    | errorName spipe       = "spipe"
    | errorName srch        = "srch"
    | errorName toobig      = "toobig"
    | errorName xdev        = "xdev"

  fun syserr code = raise SysErr (errorMsg code, SOME code)
  fun unknownsyserr message = raise SysErr (message, NONE)
end