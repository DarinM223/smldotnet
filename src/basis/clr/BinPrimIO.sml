structure BinPrimIO : PRIM_IO =
struct

type array = Word8Array.array
type array_slice = Word8ArraySlice.slice
type vector = Word8Vector.vector
type vector_slice = Word8VectorSlice.slice
type elem = Word8.word
type pos = Position.int
datatype reader
  = RD of {
    name : string,
    chunkSize : int,
    readVec : (int -> vector) option,
    readArr : (array_slice -> int) option,
    readVecNB : (int -> vector option) option,
    readArrNB : (array_slice -> int option) option,
    block : (unit -> unit) option,
    canInput : (unit -> bool) option,
    avail : unit -> int option,
    getPos : (unit -> pos) option,
    setPos : (pos -> unit) option,
    endPos : (unit -> pos) option,
    verifyPos : (unit -> pos) option,
    close : unit -> unit,
    ioDesc : OS.IO.iodesc option
    }
datatype writer
  = WR of {
    name : string,
    chunkSize : int,
    writeVec : (vector_slice -> int) option,
    writeArr : (array_slice -> int) option,
    writeVecNB : (vector_slice -> int option) option,
    writeArrNB : (array_slice -> int option) option,
    block : (unit -> unit) option,
    canOutput : (unit -> bool) option,
    getPos : (unit -> pos) option,
    setPos : (pos -> unit) option,
    endPos : (unit -> pos) option,
    verifyPos : (unit -> pos) option,
    close : unit -> unit,
    ioDesc : OS.IO.iodesc option
  }

val compare = Position.compare
fun augmentReader x = x
fun augmentWriter x = x

end