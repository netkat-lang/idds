(** Boolean variable on which a DD can branch. Morally just an integer, but
    modeled as a record for type safety. *)

open Base

module T = struct
  type t = { id : int }
  [@@unboxed]
  [@@deriving compare, sexp, hash, eq]
end
include T


let inp (id : int) : t = { id = id*2 }
let out (id : int) : t = { id = id*2 + 1 }

let is_inp (var : t) : bool =
  var.id % 2 = 0
  [@@inline]

let is_out (var : t) : bool =
  var.id % 2 = 1
  [@@inline]

let to_out (var : t) : t =
  if var.id % 2 = 1 then var else { id = var.id + 1 }

let is_in_out_pair inp out : bool =
  is_inp inp && is_out out && (inp.id + 1 = out.id)

let index (t : t) : int = t.id / 2
