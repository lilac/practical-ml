signature ANY =
sig
  type t
  type ot (* original type *)
  val from: ot -> t
  val to: t -> ot option
end

functor Any (S: sig type t end): ANY =
struct
  type t = exn
  type ot = S.t
  exception E of ot

  fun from a = E a
  fun to (e: t): ot option =
    case e of
      E a => SOME a
    | _ => NONE
end

structure AnyInt = Any(struct type t = int end)
structure AnyReal = Any(struct type t = real end)

val anys = [
  AnyInt.from 1,
  AnyReal.from 2.0,
  AnyInt.from 2,
  AnyReal.from 3.0
]

val a = List.nth (anys, 0)
val aInt = AnyInt.to a
val aReal = AnyReal.to a
val b = List.nth (anys, 1)
val bInt = AnyInt.to b
val bReal = AnyReal.to b

