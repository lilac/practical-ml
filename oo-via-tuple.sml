type 'a thunk = unit -> 'a

fun bind (f: 'a -> 'b) (a: 'a): 'b thunk =
  fn () => f a

signature SHAPE = sig
  type t
  val default: t
  val area: t -> real
  val draw: t -> unit
end

structure Rectangle = struct
  type t = {x: real, y: real, w: real, h: real}

  val default: t = {x = 0.0, y = 0.0, w = 0.0, h = 0.0}
end

structure RectangleShape : SHAPE = struct
  open Rectangle

  fun area {x, y, w, h} = w * h : real

  fun draw {x, y, w, h} = print ("rect@(" ^ Real.toString x ^ ")\n")
end

structure Circle : SHAPE = struct
  type t = {x: real, y: real, r: real}
  val default: t = {x = 0.0, y = 0.0, r = 0.0}

  fun area {x, y, r} = 2.0 * Math.pi * r * r
  fun draw {x, y, r} = print ("circle@(" ^ Real.toString x ^ ")\n")
end

structure AShape =
struct
  type t = {
    area: real thunk,
    draw: unit thunk
  }

  fun area (s: t): real =
    #area s ()

  fun draw (s: t) =
    #draw s ()
end

functor AsShape (S: SHAPE) =
struct
  fun from (it: S.t): AShape.t = {
    area = bind S.area it,
    draw = bind S.draw it
  }
end

structure RectAsShape = AsShape(RectangleShape)
structure CircleAsShape = AsShape(Circle)

val rectShape = RectAsShape.from
val circleShape = CircleAsShape.from

val shapes = [
  rectShape Rectangle.default,
  RectAsShape.from {x = 0.0, y = 0.0, w = 1.0, h = 2.0} (*{Rectangle.default where w = 1.0, h = 2.0}*),
  CircleAsShape.from Circle.default,
  circleShape {x = 0.0, y = 0.0, r = 2.0} (*{Circle.default where r = 2.0}*)
]

val areas = List.map AShape.area shapes
val _ = List.map AShape.draw shapes

