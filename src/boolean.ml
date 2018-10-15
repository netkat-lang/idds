module type Algebra = sig
  type t
  val declare_var : t -> string -> [`Ok | `Duplicate]

  type predicate
  val fls : predicate
  val tru : predicate
  val var : predicate
end

module type Predicate = sig
  type t
  val ( && ) : t -> t -> t
  val ( || ) : t -> t -> t
  val ( ~~ ) : t -> t
end
