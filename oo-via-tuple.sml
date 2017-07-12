type 'a thunk = unit -> 'a

type virtual_shape = {
  area: real thunk,
  draw: unit thunk
}

type 'a shape = 'a -> virtual_shape

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

  fun draw {x, y, w, h} = print ("rect@(" ^ Real.toString x ^ ")")
end

structure Circle : SHAPE = struct
  type t = {x: real, y: real, r: real}
  val default: t = {x = 0.0, y = 0.0, r = 0.0}

  fun area {x, y, r} = 2.0 * Math.pi * r * r
  fun draw {x, y, r} = print ("circle@(" ^ Real.toString x ^ ")")
end

functor AsShape (S: SHAPE) =
struct
  val from: S.t shape = fn it => {
    area = bind S.area it,
    draw = bind S.draw it
  }
end

structure RectAsShape = AsShape(RectangleShape)
structure CircleAsShape = AsShape(Circle)

val rectShape: Rectangle.t shape = fn it => {
  area = bind RectangleShape.area it,
  draw = bind RectangleShape.draw it
}

val circleShape: Circle.t shape = fn it => {
  area = fn () => Circle.area it,
  draw = fn () => Circle.draw it
}

val shapes = [
  rectShape Rectangle.default,
  RectAsShape.from {x = 0.0, y = 0.0, w = 1.0, h = 2.0} (*{Rectangle.default where w = 1.0, h = 2.0}*),
  CircleAsShape.from Circle.default,
  circleShape {x = 0.0, y = 0.0, r = 2.0} (*{Circle.default where r = 2.0}*)
]

fun area (s: virtual_shape): real =
  #area s ()


val areas = List.map area shapes

