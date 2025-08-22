(* MulCarryLong:MULCARRY implements multiplication by 10 and by 16 of Word64.words
   interpreted as an unsigned quantity with carry.
   *)
structure MulCarryLong:>MULCARRY where type word = Word64.word=
struct
   (* In each case, the carry is given first *)
   type word = Word64.word

   fun mul10 (w:Word64.word)=
   let
      val mul=Word64.*(w,0w10)
      (* mul is the remainder.  We need to compute the carry.  We know
         that (for real integers) mul=10*w-2^32*carry.  Hence
         *)
      val carry=Word64.mod(Word64.<<(
         Word64.+(Word64.mod(mul,0w11),Word64.mod(w,0w11)),
         0w3)
         ,0w11)
      (* (3 remainders & an extra addition seem excessive but I can't be
         bothered to think up a better way right now) *)
   in
      (carry,mul)
   end

   (* NOTE: 0w28 was carry for 32 bit words, so 0x60 is carry for 64 bit words? *)
   fun mul16 w=(Word64.>>(w,0w60),Word64.<<(w,0w4))
end
