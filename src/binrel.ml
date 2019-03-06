module type S = sig
  type carrier
  type t

  val id : t
  val empty : t
  val full : t

  val const : carrier -> t
  (* SJS: will want an efficient representation of sets as well... *)
  val test : (carrier -> bool) -> t

  val compose : t -> t -> t
  val union : t -> t -> t
  val inters : t -> t -> t
end
