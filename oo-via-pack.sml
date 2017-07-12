signature SHAPE = sig
  type t
  val default: t
  val area: t -> real
  val draw: t -> unit
end

structure Rectangle : SHAPE = struct
  type t = {x: real, y: real, w: real, h: real}

  val default: t = {x = 0.0, y = 0.0, w = 0.0, h = 0.0}

  fun area {x, y, w, h} = w * h : real

  fun draw {x, y, w, h} = print ("rect@(" ^ Real.toString x ^ ")")
end

structure Circle :> SHAPE = struct
  type t = {x: real, y: real, r: real}
  val default: t = {x = 0.0, y = 0.0, r = 0.0}

  fun area {x, y, r} = 2.0 * Math.pi * r * r
  fun draw {x, y, r} = print ("circle@(" ^ Real.toString x ^ ")")
end

signature A_SHAPE = sig
  (* val meta: pack SHAPE *)
  (* include SHAPE *)
  structure Meta: SHAPE
  val it: Meta.t
end

functor AsShape (S: SHAPE) = struct
  fun from (a: S.t): pack A_SHAPE =
    pack (structure Meta = S; val it = a) : A_SHAPE
end

structure AShape = struct
  fun area shape = let
    structure S = unpack shape: A_SHAPE
  in
    S.Meta.area S.it
  end

  fun draw shape = let
    structure S = unpack shape: A_SHAPE
  in
    S.Meta.draw S.it
  end

  (* Following function causes error "escaping local type name in let expression"
  fun get shape = let
    structure S = unpack shape: A_SHAPE
  in
    S.it
  end
  *)
end

structure RectShape = AsShape(Rectangle)
structure CircleShape = AsShape(Circle)

val shapes = [
  RectShape.from(Rectangle.default),
  CircleShape.from(Circle.default),
  RectShape.from({Rectangle.default where w = 1.0, h = 2.0})
]

do List.app AShape.draw shapes

val sumarea = List.foldl op+ 0.0 (List.map AShape.area shapes)

