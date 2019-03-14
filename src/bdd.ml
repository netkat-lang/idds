open Base

type t = Dd.t

let rec eval (t : t) (env : Var.t -> bool) : bool =
  match t with
  | True -> true
  | False -> false
  | Branch { var; hi; lo; _ } ->
    if env var then eval hi env else eval lo env

let equal = Dd.equal
let ctrue = Dd.ctrue
let cfalse = Dd.cfalse

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
          match Var.compare var_u var_v with
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
          match Var.compare var_u var_v with
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

let ite mgr var hi lo =
  disj mgr
    (conj mgr (branch mgr.dd var ctrue cfalse) hi)
    (conj mgr (branch mgr.dd var cfalse ctrue) lo)



module Make () : Boolean.Algebra with type t = t = struct
  let vars : (string, int) Hashtbl.t = Hashtbl.create (module String)
  let next_id = ref 0
  let declare_var s =
    if Hashtbl.mem vars s then `Duplicate else
    let id = !next_id in
    Int.incr next_id;
    Hashtbl.add_exn vars ~key:s ~data:id;
    `Ok
  let mgr = manager ()

  type nonrec t = t
  let tru = Dd.ctrue
  let fls = Dd.cfalse
  let of_bool = function
    | true -> tru
    | false -> fls
  let var s =
    let var = Var.inp (Hashtbl.find_exn vars s) in
    Dd.branch mgr.dd var tru fls
  let ( && ) = conj mgr
  let ( || ) = disj mgr
  let ( ! ) = neg mgr
  let ( == ) = equal
end
