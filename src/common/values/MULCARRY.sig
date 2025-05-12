(* MulCarry:MULCARRY implements multiplication by 10 and by 16 of Word32.words
   interpreted as an unsigned quantity with carry.
   *)
signature MULCARRY=
sig
   type word
   (* In each case, the carry is given first *)
   val mul10:word->word*word
   val mul16:word->word*word
end
