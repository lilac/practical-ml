signature ORD =
sig
  type t
  val compare: t -> t -> bool
end

signature SORT =
sig
  type t
  val f: t list -> t list
end

functor MergeSort (O: ORD): SORT where type t = O.t =
struct
  type t = O.t
  (* A fake sort for now *)
  fun f xs = xs
end

structure IntOrd: ORD =
struct
  type t = int
  fun compare a b = a < b
end

structure RealOrd: ORD =
struct
  type t = real
  fun compare (a: t) b = a < b
end

structure MergeSortInt = MergeSort(IntOrd)
structure MergeSortReal = MergeSort(RealOrd)

val a = MergeSortInt.f([5, 1, 2, 3])

val b = MergeSortReal.f [2.0, 1.0, 3.0]

