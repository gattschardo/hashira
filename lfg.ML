(* S_n = S_(n - j) + S_(n - k) [mod N] *)

signature LFG =
sig
  datatype lfg = LFG of int * int * int * (int array)

  val mkRand : int -> int -> lfg
  val next : rand -> (rand, int)
end

structure Lfg : LFG =
struct
  datatype lfg = LFG of int * int * int * (int array)

  fun mkRand j k =
    (LFG (j, k, 0, Array.tabulate (k, fn i => i)))

  fun next (LFG (j, k, off, S)) =
    let
      fun idx i = (off + i) mod k
      val r = Array.sub (S, idx j) + Array.sub (S, idx k)
    in
      (LFG (j, k, idx 1, Array.update (S, off, r)),
       r)
    end
end
