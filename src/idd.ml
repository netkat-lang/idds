open Base

type t = Dd.t

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

let in_var (var : Var.t) : bool =
  var.id % 2 = 0
  [@@inline]

let out_var (var : Var.t) : bool =
  var.id % 2 = 1
  [@@inline]

let branch mgr (var : Var.t) (hi : t) (lo : t) : t =
  if out_var var then
    begin match hi, lo with
    | False, False -> hi
    | _ -> Dd.branch mgr var hi lo
    end
  else
    if equal hi lo then hi else
    let i = var.id + 1 in
    let hi = match hi with
      | Branch { hi; lo=False; var={id}; _ } when id = i -> hi
      | _ -> hi
    in
    let lo = match lo with
      | Branch { hi=False; lo; var={id}; _ } when id = i -> lo
      | _ -> lo
    in
    begin match hi, lo with
    | Branch { hi=False; lo=l; var={id}; _ }, _ when id = i && equal lo l -> hi
    | _, Branch { hi=h; lo=False; var={id}; _ } when id = i && equal hi h -> lo
    | _ -> if equal hi lo then hi else Dd.branch mgr var hi lo
    end

