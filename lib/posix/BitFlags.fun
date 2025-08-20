functor BitFlags (S : sig
                   eqtype flags
                   val all : flags
                   val toWord : flags -> SysWord.word
                   val fromWord : SysWord.word -> flags
                 end): BIT_FLAGS = struct
  open S
  fun flags (x: flags list): flags = fromWord (List.foldl SysWord.orb 0wx0 (List.map toWord x))
  fun intersect (x: flags list): flags = fromWord (List.foldl SysWord.andb (toWord all) (List.map toWord x))
  fun clear (f1: flags, f2: flags): flags = fromWord (SysWord.andb (SysWord.notb (toWord f1), toWord f2))
  fun allSet (f1: flags, f2: flags): bool =
    let val f1Word = toWord f1
    in SysWord.andb (f1Word, toWord f2) = f1Word
    end
  fun anySet (f1: flags, f2: flags): bool = SysWord.andb (toWord f1, toWord f2) <> 0wx0
end