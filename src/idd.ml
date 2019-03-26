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

let ident = Dd.ctrue
let empty = Dd.cfalse

let rec eval' expl (tree:t) (env:Var.t -> bool) (n:int) =
  match tree with
  | False -> false
  | True -> List.range 0 n |> List.for_all ~f:(fun i ->
      Set.mem expl i || Bool.equal (env (Var.inp i)) (env (Var.out i)))
  | Branch { var; hi; lo } when Var.is_inp var ->
    eval' expl (if (env var) then hi else lo) env n
  | Branch { var; hi; lo } ->
    eval' (Set.add expl (Var.index var)) (if (env var) then hi else lo) env n

let eval = eval' (Set.empty (module Int))

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


(* relational operations *)
module Rel = struct

  (* booleans *)
  type b = Bdd.t
  let ctrue = Bdd.ctrue
  let cfalse = Bdd.cfalse
  let conj _ _ = failwith "todo"
  let disj _ _ = failwith "todo"
  let neg _ = failwith "todo"

  (* relations *)
  type nonrec t = t
  let zero = empty
  let one = ident
  (* FIXME: check that BDD contains only input variables *)
  let test (bdd : Bdd.t) : t = (bdd :> t)

  let seq _ _ = failwith "todo"
  let union _ _ = failwith "todo"
  let star _ = failwith "todo"

end
