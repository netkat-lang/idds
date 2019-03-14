(** Boolean variable on which a DD can branch. Morally just an integer, but
    modeled as a record for type safety. *)

type t [@@deriving compare, sexp, hash, eq]


val inp : int -> t
val out : int -> t

val is_inp : t -> bool
val is_out : t -> bool

val index : t -> int

val to_out : t -> t
val is_in_out_pair : t -> t -> bool
