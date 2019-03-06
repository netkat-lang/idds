(** Boolean variable on which a DD can branch. Morally just an integer, but
    modeled as a record for type safety. *)

open Base

module T = struct
  type t = { id : int }
  [@@unboxed]
  [@@deriving compare, sexp, hash]
end
include T
include Comparable.Make(T)
