open Base

type t = Dd.t

let eval t env =
  let env Dd.{idx} = env idx in
  Dd.eval t env

let equal = Dd.equal

module Pair = struct
  type t = int * int
    [@@deriving sexp, compare, hash]
end

type manager = {
  dd : Dd.manager;
  conj_cache : (Pair.t, t) Hashtbl.t;
  disj_cache : (Pair.t, t) Hashtbl.t;
  neg_cache : (int, t) Hashtbl.t
}

let manager () : manager = {
  dd = Dd.manager ();
  conj_cache = Hashtbl.create (module Pair);
  disj_cache = Hashtbl.create (module Pair);
  neg_cache = Hashtbl.create (module Int);
}

(* let branch mgr var hi lo =
  if Dd.equal hi lo then
    hi
  else
    Dd.branch mgr var hi lo *)

