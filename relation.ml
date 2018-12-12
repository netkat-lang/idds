module type BinRel = struct
  type ('x, 'y) t

  type 'x set = ('x, unit) t

  val id : ('x, 'x) t
  val zero : ('x, 'y) t

  val test : 'x set -> ('x, 'x) t
  val const : 'x -> ('x, 'x) t

  val compose : ('x, 'y) t -> ('y, 'z) t -> ('x, 'z) t
  val union : ('x, 'y) t -> ('x, 'y) t -> ('x, 'y) t
  val inters : ('x, 'y) t -> ('x, 'y) t -> ('x, 'y) t
end
