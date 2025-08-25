structure RTLong:>RTLONG =
struct
   type t=Int64.int
   type pair = Word32.word * Word32.word (* high one first *)

   fun fromWordPair (w1, w2) =
      let
         val w1 = Word32.toLarge w1
         val w2 = Word32.toLarge w2
      in
         Int64.fromLarge (Word64.toLargeIntX (Word64.orb (Word64.<<(w1, 0w32), w2)))
      end
   fun toWordPair i =
      let
         val w = Word64.fromLargeInt (Int64.toLarge i)
         fun conv (w:Word64.word) = Word32.fromLargeWord w
      in
         (conv (Word64.>> (w, 0w32)), conv w)
      end

   val W2i=Int32.fromLarge o Word32.toLargeIntX
   val i2w=Word.fromInt

   structure pack:>PACKABLE where type t=t =
   struct
      type t=t
      val pack = Numbers.I8
      fun equal(x,y)=x=y
   end

   fun getlong is = fromWordPair (ReadInts.W4 is,ReadInts.W4 is)

   val log2 = Numbers.Log2'

   val compare = Int64.compare

   structure numops:>NUMOPS where type num=Int64.int
      where type shiftnum=RTInt.t=
   struct
      type num=Int64.int
      type shiftnum=RTInt.t

      val precision=valOf(Int64.precision)
      exception NumOverflow
      fun fromInt value=
      if Numbers.isi8(value) then
         Int64.fromInt(value)
      else raise NumOverflow

      fun toInt i=Int64.toInt i
         handle Overflow => raise NumOverflow

      val toShift = RTInt.fromInt

      fun w2i w=Int64.fromLarge(Word64.toLargeIntX w)
      fun i2w i=Word64.fromLargeInt(Int64.toLarge i)
      fun i2w' i=Word64.fromLargeInt(Int32.toLarge i)
      fun W2w x=Word.fromLargeWord(Word64.toLargeWord x)

      fun deword f (x,y)= w2i(f(i2w x,i2w y))
      val add=deword Word64.+
      val sub=Int64.-
      fun neg x=Int64.-(0,x)

      val mul=deword Word64.*
      fun x div y=
      if y=0 then NONE
      else
         (SOME(Int64.quot(x,y)))
         handle
            Div => NONE (* division by 0 *)
         |  Overflow => SOME x
            (* Overflow can only occur when
               ~2^32 is divided by ~1; for the
               Java VM the result is 2^32 wrapped
               around to get ~2^32 again *)
      fun rem(x,y)=
         (SOME(Int64.rem(x,y)))
         handle
            Div => NONE (* division by 0 *)

      val andb=deword Word64.andb
      val orb=deword Word64.orb
      val xorb=deword Word64.xorb

      fun get5bits x= W2w(Word64.andb(i2w' (RTInt.toInt32 x),0wx1f))
      fun deword5 f (x,y)=w2i(f(i2w x,get5bits y))
      val shl=deword5 Word64.<<
      val shr=deword5 Word64.~>>
      val ushr=deword5 Word64.>>

      val compare=Int64.compare
      val Compare=SOME o compare
      fun lt(a,b)=(compare(a,b)=LESS)
      fun le(a,b)=(compare(a,b)<>GREATER)
   end

   structure IntOps:>INTOPS where type t=t =
   struct
      type t=t
      fun zero _ = 0:Int64.int
      fun mul10 {signed=signed} (n:Int64.int)=
         if signed
         then n*10
         else
            let
               val w=Word64.fromLargeInt(Int64.toLarge n)
               val (carry,mul)=MulCarryLong.mul10 w
            in
               if carry=0w0
               then Int64.fromLarge(Word64.toLargeIntX mul)
               else raise Overflow
            end

      fun mul16 {signed=signed} (n:Int64.int)=
         if signed
         then n*16
         else
            let
               val w=Word64.fromLargeInt(Int64.toLarge n)
               val (carry,mul)=MulCarryLong.mul16 w
            in
               if carry=0w0
               then Int64.fromLarge(Word64.toLargeIntX mul)
               else raise Overflow
            end

      fun do_digit {signed=signed} (n:Int64.int,digit:int)=
         if signed
         then n-Int64.fromLarge(Int.toLarge digit)
         else
            let
               val w=Word64.fromLargeInt(Int64.toLarge n)
               val d=Word64.fromInt digit
               val res=Word64.+(w,d)
            in
               if Word64.>=(res,w)
               then Int64.fromLarge(Word64.toLargeIntX res)
               else raise Overflow
            end

      val neg=Int64.~
   end (* IntOps *)

   structure IC=IntConv(IntOps)

   val fromString=IC.fromString

   val fromInt=numops.fromInt
   fun toInt value=(SOME(numops.toInt value)) handle numops.NumOverflow => NONE


   (* Now to implement toRTInt and fromRTInt.  We use the Rep16 functor
      for this. *)
   structure RepInt=Rep16(RTInt.numops)
   structure RepLong=Rep16(numops)

   fun toRTInt jl=
   let
      val int16s=RepLong.to16 jl
      val chopped=List.drop(int16s,2)
      val res=RepInt.from16 chopped
   in
      res
   end

   fun fromRTInt ji=
   let
      val int16s=RepInt.to16 ji
      val signed=(hd int16s>=0x8000)
      val extended_int16s=
         (if signed
          then
            [0xffff,0xffff]
          else
            [0x0   ,0x0   ]
          ) @ int16s
      val res=RepLong.from16 extended_int16s
   in
      res
   end
end




