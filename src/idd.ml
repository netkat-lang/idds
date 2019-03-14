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

let branch (mgr : manager) (var : Var.t) (hi : t) (lo : t) : t =
  if Var.is_out var then
    begin match hi, lo with
    | False, False -> hi
    | _ -> Dd.branch mgr.dd var hi lo
    end
  else
    if equal hi lo then hi else
    let hi = match hi with
      | Branch { hi; lo=False; var=var'; _ }
        when Var.(equal (to_out var) var') ->
        hi
      | _ -> hi
    in
    let lo = match lo with
      | Branch { hi=False; lo; var=var'; _ }
        when Var.(equal (to_out var) var') ->
        lo
      | _ -> lo
    in
    begin match hi, lo with
    | Branch { hi=False; lo=l; var=var'; _ }, _
      when Var.(equal (to_out var) var') && equal lo l ->
      hi
    | _, Branch { hi=h; lo=False; var=var'; _ }
      when Var.(equal (to_out var) var') && equal hi h ->
      lo
    | _ -> if equal hi lo then hi else Dd.branch mgr.dd var hi lo
    end

