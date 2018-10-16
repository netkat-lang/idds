open Base

type t = Dd.t

let eval = Dd.eval

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

let branch mgr var hi lo =
  if Dd.equal hi lo then
    hi
  else
    Dd.branch mgr var hi lo

let conj mgr =
  let rec conj (u : t) (v : t) : t =
    match u, v with
    | False, _ | _, False ->
      Dd.cfalse
    | True, w | w, True ->
      w
    | Branch { var=var_u; hi=hi_u; lo=lo_u; id=id_u },
      Branch { var=var_v; hi=hi_v; lo=lo_v; id=id_v } ->
      if id_u = id_v then u else
        let key = if id_u <= id_v then (id_u, id_v) else (id_v, id_u) in
        Hashtbl.find_or_add mgr.conj_cache key ~default:(fun () ->
          match Int.compare var_u.idx var_v.idx with
          | -1 ->
            let var = var_u in
            let hi = conj hi_u v in
            let lo = conj lo_u v in
            branch mgr.dd var hi lo
          | 1 ->
            let var = var_v in
            let hi = conj u hi_v in
            let lo = conj u lo_v in
            branch mgr.dd var hi lo
          | 0 ->
            let var = var_u in
            let hi = conj hi_u hi_v in
            let lo = conj lo_v lo_u in
            branch mgr.dd var hi lo
          | _ ->
            assert false
        )
  in
  conj

let disj mgr =
  let rec disj (u : t) (v : t) : t =
    match u, v with
    | False, w | w, False ->
      w
    | True, _ | _, True ->
      Dd.ctrue
    | Branch { var=var_u; hi=hi_u; lo=lo_u; id=id_u },
      Branch { var=var_v; hi=hi_v; lo=lo_v; id=id_v } ->
      if id_u = id_v then u else
        let key = if id_u <= id_v then (id_u, id_v) else (id_v, id_u) in
        Hashtbl.find_or_add mgr.disj_cache key ~default:(fun () ->
          match Int.compare var_u.idx var_v.idx with
          | -1 ->
            let var = var_u in
            let hi = disj hi_u v in
            let lo = disj lo_u v in
            branch mgr.dd var hi lo
          | 1 ->
            let var = var_v in
            let hi = disj u hi_v in
            let lo = disj u lo_v in
            branch mgr.dd var hi lo
          | 0 ->
            let var = var_u in
            let hi = disj hi_u hi_v in
            let lo = disj lo_v lo_u in
            branch mgr.dd var hi lo
          | _ ->
            assert false
        )
  in
  disj

let neg mgr =
  let rec neg (u : t) =
    match u with
    | True -> Dd.cfalse
    | False -> Dd.ctrue
    | Branch { var; hi; lo; id } ->
      begin match Hashtbl.find mgr.neg_cache id with
      | Some v ->
        v
      | None ->
        let v = Dd.branch mgr.dd var (neg hi) (neg lo) in
        Hashtbl.add_exn mgr.neg_cache ~key:id ~data:v;
        Hashtbl.add_exn mgr.neg_cache ~key:Dd.(id v) ~data:u;
        v
      end
  in
  neg



module Make () : Boolean.Algebra with type Predicate.t = t = struct
  let vars : (string, int) Hashtbl.t = Hashtbl.create (module String)
  let next_idx = ref 0
  let declare_var s =
    if Hashtbl.mem vars s then `Duplicate else
    let idx = !next_idx in
    Int.incr next_idx;
    Hashtbl.add_exn vars ~key:s ~data:idx;
    `Ok
  let mgr = manager ()

  let tru = Dd.ctrue
  let fls = Dd.cfalse
  let var s =
    let idx = Hashtbl.find_exn vars s in
    let var = Dd.{ idx } in
    Dd.branch mgr.dd var tru fls

  module Predicate = struct
    type nonrec t = t
    let ( && ) = conj mgr
    let ( || ) = disj mgr
    let ( ~~ ) = neg mgr
  end
end
