module type Predicate = sig
  type t
  val ( && ) : t -> t -> t
  val ( || ) : t -> t -> t
  val ( ~~ ) : t -> t
end


module type Algebra = sig
  val declare_var : string -> [`Ok | `Duplicate]

  module Predicate : Predicate
  val fls : Predicate.t
  val tru : Predicate.t
  val var : string -> Predicate.t
end

