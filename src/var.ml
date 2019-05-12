(** Boolean variable on which a DD can branch. Morally just an integer, but
    modeled as a record for type safety. *)

open Base

module T = struct
  type t = { id : int }
  [@@unboxed]
  [@@deriving compare, sexp, hash, eq]
end
include T

let leaf_idx = -1

let[@inline] inp (id : int) : t = { id = id*2 }
let[@inline] out (id : int) : t = { id = id*2 + 1 }

let[@inline] is_inp (var : t) : bool =
  var.id % 2 = 0

let[@inline] is_out (var : t) : bool =
  var.id % 2 = 1

let[@inline] to_out (var : t) : t =
  if var.id % 2 = 1 then var else { id = var.id + 1 }

let[@inline] is_in_out_pair inp out : bool =
  is_inp inp && is_out out && (inp.id + 1 = out.id)

let[@inline] index (t : t) : int =
  t.id / 2

let[@inline] closer_to_root (v0 : t) (v1 : t) : [`Left | `Right | `Equal ] =
  match Int.compare v0.id v1.id with
  | 1 -> `Left
  | 0 -> `Equal
  | _ -> `Right

let[@inline] idx_strictly_closer_to_root (idx0 : int) (idx1 : int) : bool =
  idx0 > idx1
