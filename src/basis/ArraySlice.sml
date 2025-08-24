structure ArraySlice :> ARRAY_SLICE =
struct

local
  open General Option PrimUtils_.Int
in

type 'a slice = 'a array * int * int

fun length (a,i,len) = len
fun sub ((a,i,len),j) = Array.sub(a,i+j)
fun update((a,i,len),j,x) = Array.update(a,i+j,x)

fun slice (a,i,NONE) = if i < 0 orelse i > Array.length a then raise General.Subscript else (a, i, Array.length a - i)
  | slice (a,i,SOME len) = if i < 0 orelse len < 0 orelse i+len > Array.length a then raise General.Subscript else (a,i,len)

fun full a = (a,0,Array.length a)

fun base (p:'a slice) = p
fun vector sl = Vector.tabulate (length sl, fn i => sub (sl, i))

fun isEmpty ((a,i,0):'a slice) = true
  | isEmpty _ = false

fun subslice ((a,i,n),j,NONE) =
      if j < 0 orelse n < j then raise Subscript else (a, i+j, n-j)
  | subslice ((a,i,n),j,SOME k) =
      if j < 0 orelse k < 0 orelse n < j+k then raise Subscript else (a, i+j, k)

fun foldli f e (slice as (a, i, n)) =
    let fun loop stop =
      let fun lr j res =
    if j < stop then lr (j+1) (f(j, Array.sub(a,j), res))
    else res
      in lr i e end
    in loop (i+n) end

fun foldri f e (slice as (a, i, n)) =
    let fun loop start =
      let fun rl j res =
        if j >= i then rl (j-1) (f(j, Array.sub(a,j), res))
        else res
      in rl start e end;
    in loop (i+n - 1) end

fun appi f (slice as (a, i, n)) =
    let fun loop stop =
      let	fun lr j =
        if j < stop then (f(j, Array.sub(a,j)); lr (j+1))
        else ()
      in lr i end
    in loop (i+n) end

fun mapi (f : 'a -> 'b) (a, i, n) =
    let
      val newvec : 'a array = Prim.newarray(n)
      fun copy j =
        if Int.<(j,n)
        then (Prim.arraystore(newvec, j, f(i+j, Array.sub(a,i+j))); copy (j+1))
        else Prim.toVector newvec
    in
      copy 0
    end



fun app f sl = appi (f o #2) sl
fun map f sl = mapi (f o #2) sl

fun foldl f init sl = foldli (fn (_, a, x) => f(a, x)) init sl
fun foldr f init sl = foldri (fn (_, a, x) => f(a, x)) init sl

fun all (p : 'a -> bool) ((a,i,n) : 'a slice) : bool =
  let
    val stop = i+n
    fun go (a,j) = j >= stop orelse (p (Array.sub(a,j)) andalso go (a,j+1))
  in go (a,i)
  end
fun exists (p : 'a -> bool) ((a,i,n) : 'a slice) : bool =
  let
    val stop = i+n
    fun go (a,j) = j < stop andalso (p (Array.sub(a,j)) orelse go (a,j+1))
  in go (a,i)
  end
fun find (p : 'a -> bool) ((a,i,n) : 'a slice) : 'a option =
  let
    val stop = i+n
    fun go j =
      if j < stop then
    if p (Array.sub(a,j)) then SOME (Array.sub(a,j)) else go (j+1)
      else NONE
  in go i
  end
fun findi (p : int * 'a -> bool) ((a,i,n) : 'a slice) : (int * 'a) option =
  let
    val stop = i+n
    fun go j =
      if j < stop then
        if p (j-i, Array.sub(a,j)) then SOME (j-i, Array.sub(a,j)) else go (j+1)
        else NONE
  in go i
  end
fun collate cmp ((a1,i1,n1), (a2,i2,n2)) =
  let
    val stop = if n1 < n2 then n1 else n2
    fun h (a1,j) =
      if Prim.=(j, stop) then
         if n1 < n2 then LESS
         else if n1 > n2 then GREATER
         else                 EQUAL
      else
       case cmp(Array.sub(a1,i1+j), Array.sub(a2,i2+j)) of
         EQUAL => h (a1,j+1)
       | res   => res
  in h (a1,0)
  end
fun concat _ = raise Fail "ArraySlice.concat not implemented"
fun getItem (a, i, 0) = NONE
  | getItem (a, i, n) = SOME(Array.sub(a, i), (a, i+1, n-1));
fun copy _ = raise Fail "ArraySlice.copy not implemented"
fun copyVec _ = raise Fail "ArraySlice.copyVec not implemented"
fun modifyi f (a, i, n) =
  let
    val stop = i+n
    fun go (a,j) =
      if j < stop then (Array.update(a,j,f(j-i, Array.sub(a,j))); go (a,j+1))
      else ()
  in go (a,i)
  end
fun modify f (a, i, n) =
  let
    val stop = i+n
    fun go (a,j) = if j < stop then (Array.update(a,j,f(Array.sub(a,j))); go (a,j+1))
                   else ()
    in go (a,i)
    end
end

end