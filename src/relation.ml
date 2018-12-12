module type BinRel = sig
  type t
  type d
  type ds

  val id : t
  val zero : t

  val const : d -> t
  val test : ds -> t

  val compose : t -> t -> t
  val union : t -> t -> t
  val inters : t -> t -> t
end
