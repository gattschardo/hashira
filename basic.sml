infix 1 |> |->
infix 1 #> #->

signature BASIC =
sig
  val I : 'a -> 'a
  val K : 'a -> ('a -> 'a)

  val |> : 'a * ('a -> 'b) -> 'b
  val |-> : ('c * 'a) * ('c -> 'a -> 'b) -> 'b
  val #> : ('a -> 'b) * ('b -> 'c) -> 'a -> 'c
  val #-> : ('a -> 'c * 'b) * ('c -> 'b -> 'd) -> 'a -> 'd

  val pair : 'a -> 'b -> 'a * 'b
  val rpair : 'b -> 'a -> 'a * 'b

  val fst : 'a * 'b -> 'a
  val snd : 'a * 'b -> 'b

  val apfst : ('a -> 'c) -> 'a * 'b -> 'c * 'b
  val apsnd : ('b -> 'c) -> 'a * 'b -> 'a * 'c

  val uncurry : ('a -> 'b -> 'c) -> ('a * 'b -> 'c)

  val add : int -> int -> int
  val sub : int -> int -> int

  val min : int -> int -> int
  val max : int -> int -> int

  val inc : int -> int
  val dec : int -> int

  val try : ('a -> bool) -> ('a -> 'a) -> 'a -> 'a

  val maybe_do : ('a -> 'b) -> 'a option -> 'b -> 'b
  val maybe_apply : ('b -> 'a -> 'a) -> 'b option -> 'a -> 'a

  val println : string -> unit

  val split_half : 'a list -> 'a list * 'a list
  val sort : ('a * 'a -> bool) -> 'a list -> 'a list
end

structure Basic : BASIC =
struct
  fun I x = x
  fun K a = (fn _ => a)

  fun a |> f = f a
  fun (a, b) |-> f = f a b
  fun (f #> g) x = x |> f |> g
  fun (f #-> g) x = x |> f |-> g

  fun pair a b = (a, b)
  fun rpair b a = (a, b)

  fun fst (a, _) = a
  fun snd (_, b) = b

  fun apfst f (a, b) = (f a, b)
  fun apsnd f (a, b) = (a, f b)

  fun uncurry f = (fn (a, b) => f a b)

  fun add x y = x + y
  fun sub x y = x - y

  fun min x y = if x < y then x else y
  fun max x y = if x > y then x else y

  val inc = add 1
  val dec = add ~1

  fun try is_ok f a =
    let val a' = f a in
      if is_ok a' then a' else a
    end

  fun maybe_do _ NONE b = b
    | maybe_do f (SOME a) _ = f a

  fun maybe_apply _ NONE b = b
    | maybe_apply f (SOME a) b = f a b

  fun println x = print (x ^ "\n")

  fun split_half l =
    let val x = length l div 2
    in
      (List.take (l, x), List.drop (l, x))
    end

  fun merge _ [] r = r
    | merge _ l [] = l
    | merge lt (x::l) (y::r) =
      if lt (x, y) then
        x::merge lt l (y::r)
      else
        y::merge lt (x::l) r
  and sort _ [] = []
    | sort _ [x] = [x]
    | sort lt l =
      let
        val (left, right) = split_half l
        val left' = sort lt left
        val right' = sort lt right
      in
        merge lt left' right'
      end
end

open Basic

(* vim: se ai et ts=2 sw=2: *)
