structure Posix_ProcEnv: POSIX_PROC_ENV = struct
  type uid = word
  type gid = word
  (* type pid = Process.pid *)
  type pid = int
  type file_desc = int

  fun wordToUid w = Word.fromLargeWord w
  fun uidToWord g = Word.toLargeWord g
  fun wordToGid w = Word.fromLargeWord w
  fun gidToWord g = Word.toLargeWord g

  val getpid = Mono.Unix.Native.Syscall.getpid
  val getppid = Mono.Unix.Native.Syscall.getppid
  val getuid = Mono.Unix.Native.Syscall.getuid
  val geteuid = Mono.Unix.Native.Syscall.geteuid
  val getgid = Mono.Unix.Native.Syscall.getgid
  val getegid = Mono.Unix.Native.Syscall.getegid
  fun setuid (uid: uid): unit =
    if Mono.Unix.Native.Syscall.setuid uid = ~1
      then raise OS.SysErr ("Error in setuid", NONE)
      else ()
  fun setgid (gid: gid): unit =
    if Mono.Unix.Native.Syscall.setgid gid = ~1
      then raise OS.SysErr ("Error in setgid", NONE)
      else ()
  fun getgroups (): gid list =
    let
      open Mono.Unix.Native
      val size = Syscall.getgroups NONE
      val arr = Array.array (size, 0w0)
      val result = Syscall.getgroups (SOME arr)
    in
      if result = ~1
        then raise OS.SysErr ("Error in getgroups", NONE)
        else Array.foldr (op::) [] arr
    end
  val getlogin: unit -> string = valOf o Mono.Unix.Native.Syscall.getlogin
  val getpgrp: unit -> pid = Mono.Unix.Native.Syscall.getpgrp
  val setsid: unit -> pid = Mono.Unix.Native.Syscall.setsid
  fun setpgid {pid : pid option, pgid : pid option}: unit =
    let
      val pid = case pid of NONE => 0 | SOME p => p
      val pgid = case pgid of NONE => 0 | SOME p => p
    in
      if Mono.Unix.Native.Syscall.setpgid (pid, pgid) = ~1
        then raise OS.SysErr ("Error in setpgid", NONE)
        else ()
    end

  val uname : unit -> (string * string) list = fn () =>
    let
      open Mono.Unix.Native
      val name = ref (SOME (Utsname ()))
      val result = Syscall.uname (&name)
      val () = if result = ~1 then raise OS.SysErr ("Error in uname", NONE) else ()
      val sysname : string option = !((Prim.unsafeValOf (!name)).#sysname)
      val nodename : string option = !((Prim.unsafeValOf (!name)).#nodename)
      val release : string option = !((Prim.unsafeValOf (!name)).#release)
      val version : string option = !((Prim.unsafeValOf (!name)).#version)
      val machine : string option = !((Prim.unsafeValOf (!name)).#machine)
      val domainname : string option = !((Prim.unsafeValOf (!name)).#domainname)
    in
      List.mapPartial (fn (a, b) => Option.map (fn b => (a, b)) b)
        [ ("sysname", sysname)
        , ("nodename", nodename)
        , ("release", release)
        , ("version", version)
        , ("machine", machine)
        , ("domainname", domainname)
        ]
    end

  fun time (): Time.time = Time.fromSeconds (Mono.Unix.Native.Syscall.time (&(ref (Int64.fromInt 0))))

  (* TODO: implement this *)
  val times : unit
                -> {
                  elapsed : Time.time,
                  utime : Time.time,
                  stime : Time.time,
                  cutime : Time.time,
                  cstime : Time.time
                } = fn () => raise OS.SysErr ("times: not implemented yet", NONE)

  val getenv = OS.Process.getEnv
  val environ : unit -> string list = fn () =>
    let
      val vars = valOf (System.Environment.GetEnvironmentVariables ())
      val vals = valOf (vars.#get_Keys ())
      val iter = valOf (vals.#GetEnumerator ())
      fun go acc (iter : System.Collections.IEnumerator) =
        if iter.#MoveNext ()
          then go ((valOf (iter.#get_Current ()) :> string) :: acc) iter
          else List.rev acc
    in go [] iter
    end

  val ctermid = fn () => "/dev/tty"
  val ttyname: file_desc -> string = valOf o Mono.Unix.Native.Syscall.ttyname
  val isatty: file_desc -> bool = Mono.Unix.Native.Syscall.isatty

  local
    open Mono.Unix.Native
    fun convertName "ARG_MAX" = SysconfName._SC_ARG_MAX
      | convertName "CHILD_MAX" = SysconfName._SC_CHILD_MAX
      | convertName "CLK_TCK" = SysconfName._SC_CLK_TCK
      | convertName "NGROUPS_MAX" = SysconfName._SC_NGROUPS_MAX
      | convertName "OPEN_MAX" = SysconfName._SC_OPEN_MAX
      | convertName "STREAM_MAX" = SysconfName._SC_STREAM_MAX
      | convertName "TZNAME_MAX" = SysconfName._SC_TZNAME_MAX
      | convertName "JOB_CONTROL" = SysconfName._SC_JOB_CONTROL
      | convertName "SAVED_IDS" = SysconfName._SC_SAVED_IDS
      | convertName "VERSION" = SysconfName._SC_VERSION
      | convertName s = raise OS.SysErr ("Unknown sysconf name: " ^ s, NONE)
  in
    fun sysconf (s: string): SysWord.word =
      let val result = Syscall.sysconf (convertName s)
      in
        if result < 0 then raise OS.SysErr ("Error in sysconf", NONE)
        else SysWord.fromLargeInt result
      end
  end
end