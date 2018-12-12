open Base

type t = Dd.t

let eval t env =
  let env (Dd.{idx} : Dd.Var.t) = env idx in
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

let in_var (var : Dd.Var.t) : bool =
  var.idx % 2 = 0
  [@@inline]

let out_var (var : Dd.Var.t) : bool =
  var.idx % 2 = 1
  [@@inline]

let branch mgr (var : Dd.Var.t) (hi : t) (lo : t) : t =
  if out_var var then
    begin match hi, lo with
    | False, False -> hi
    | _ -> Dd.branch mgr var hi lo
    end
  else
    if equal hi lo then hi else
    let i = var.idx + 1 in
    let hi = match hi with
      | Branch { hi; lo=False; var={idx}; _ } when idx = i -> hi
      | _ -> hi
    in
    let lo = match lo with
      | Branch { hi=False; lo; var={idx}; _ } when idx = i -> lo
      | _ -> lo
    in
    begin match hi, lo with
    | Branch { hi=False; lo=l; var={idx}; _ }, _ when idx = i && equal lo l -> lo
    | _, Branch { hi=h; lo=False; var={idx}; _ } when idx = i && equal hi h -> hi
    | _ -> if equal hi lo then hi else Dd.branch mgr var hi lo
    end

(* let apply op cache =
  let rec apply u v =
 *)
