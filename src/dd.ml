open Base

type var = { idx : int }
  [@@unboxed]
  [@@deriving compare, sexp]

type t =
  | True
  | False
  | Branch of { var : var; hi : t; lo: t; id: int }

let id (t : t) : int =
  match t with
  | False -> -2
  | True -> -1
  | Branch { id; _ } -> id

module Triple = struct
  type t = int * int * int
    [@@deriving hash, compare, sexp]
end

type manager = {
  mutable next_id : int;
  branch_cache : (Triple.t, t) Hashtbl.t;
}

let manager () = {
  next_id = 0;
  branch_cache = Hashtbl.create (module Triple);
}

let ctrue = True
let cfalse = False

(* let is_true = function
  | True -> true
  | _ -> false

let is_false = function
  | False -> true
  | _ -> false *)

let branch (mgr : manager) (var : var) (hi : t) (lo : t) : t =
  let triple = (var.idx, id hi, id lo) in
  Hashtbl.find_or_add mgr.branch_cache triple ~default:(fun () ->
    let id = mgr.next_id in
    mgr.next_id <- id + 1;
    Branch { var; hi; lo; id; }
  )

let equal (t1 : t) (t2 : t) : bool =
  match t1, t2 with
  | True, True | False, False ->
    true
  | Branch { id=id1; _ }, Branch { id=id2; _ } ->
    id1 = id2
  | _ ->
    false

let rec eval (t : t) (env : var -> bool) : bool =
  match t with
  | True -> true
  | False -> false
  | Branch { var; hi; lo; _ } ->
    if env var then eval hi env else eval lo env
