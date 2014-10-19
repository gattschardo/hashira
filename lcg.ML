(*
Linear congruency generator:

X_n+1 = a * X_n + c (mod m)

Numerical Recipes:
m = 2^32
a = 1 664 525
c = 1 013 904 223
*)

signature LCG =
sig
  datatype lcg = St of int * int * int * int

  val make : int -> int -> int -> lcg
  val nr : lcg

  val seed : int -> lcg -> lcg

  val next : lcg -> lcg * int
  val next_range : int * int -> lcg -> lcg * int
  val next_vector : 'a vector -> lcg -> lcg * 'a
end

structure Lcg : LCG =
struct
  datatype lcg = St of int * int * int * int

  fun make a c m = St (a, c, m, 0)

  val nr = St (1664525, 1013904223, Real.toInt IEEEReal.TO_ZERO (Math.pow (2.0, 32.0)), 0)

  fun seed s (St (a, c, m, _)) = St (a, c, m, s)

  fun next (St (a, c, m, x)) = 
    let
      val y = (a * x + c) mod m
    in
      (St (a, c, m, y), y)
    end

  fun next_range (i, j) (St (a, c, m, x)) =
    let
      val n = j - i
      val top =  ((((m - n) + 1) div n) * n - 1) + n
      fun iter st =
        let val (st', y) = next st
        in
          if y <= top
          then
            (st', y)
          else
            iter st'
        end
      val (st', x) = iter (St (a, c, m, x))
    in
      (st', x mod n + i)
    end

  fun next_vector v st =
    let val (st', i) = next_range (0, (Vector.length v) - 1) st
    in
      (st', Vector.sub (v, i))
    end
end

(* vim: se ai et: *)
