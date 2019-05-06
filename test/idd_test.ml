open Idd_
open Base

module Basic = struct
  let mgr = Idd.manager ()
  let vars = 2
  let x0 = Var.inp 0
  let y0 = Var.out 0
  let x1 = Var.inp 1
  let y1 = Var.out 1
  let x2 = Var.inp 2
  let y2 = Var.out 2
  let x99 = Var.inp 99
  let y99 = Var.out 99

  let rec mk_all_trees n : Idd.t list =
    if n <= 0 then
      [Idd.ident; Idd.empty]
    else
      let ts = mk_all_trees (n-1) in
      let var = if n%2 = 0 then Var.out (n/2) else Var.inp (n/2) in
      List.cartesian_product ts ts
      |> List.map ~f:(fun (hi, lo) ->
          Idd.branch mgr var hi lo
        )
  let trees = mk_all_trees (2*vars)

  let%test "branch reduction: xi ? u : u -> u" =
    List.for_all trees ~f:(fun t ->
      Idd.equal (Idd.branch mgr x99 t t) t
    )

  let%test "branch reduction: yi ? u : u -> u iff u=false" =
    Idd.(equal (branch mgr y99 empty empty) empty) &&
    List.for_all trees ~f:(fun t ->
      Idd.(equal t empty) || not Idd.(equal (branch mgr y99 t t) t)
    )

  let small_trees = mk_all_trees 3

  let%test "iia" =
    List.cartesian_product small_trees small_trees |>
    List.for_all ~f:(fun (u,v) -> Idd.(equal
    (branch mgr (Var.inp 10) (branch mgr (Var.out 10) u empty) v)
    (branch mgr (Var.inp 10) u v)))

  let%test "iib" =
    List.cartesian_product small_trees small_trees |>
    List.for_all ~f:(fun (u,v) -> Idd.(equal
    (branch mgr (Var.inp 10) u (branch mgr (Var.out 10) empty v))
    (branch mgr (Var.inp 10) u v)))

  let%test "iiia" =
    List.for_all trees ~f:(fun u -> Idd.(equal
    (branch mgr (Var.inp 10) (branch mgr (Var.out 10) empty u) u)
    (branch mgr (Var.out 10) empty u)))

  let%test "iiib" =
    List.for_all trees ~f:(fun u -> Idd.(equal
    (branch mgr (Var.inp 10) u (branch mgr (Var.out 10) u empty))
    (branch mgr (Var.out 10) u empty)))

  let%test "eval" =
    Idd.(
      not (eval empty (fun _ -> true) 10) &&
      eval ident (fun _ -> true) 1  &&
      not (eval ident (fun x -> Var.equal x (Var.inp 0)) 1) &&
      let branchtree = Idd.(branch mgr x2 empty ident) in
      not (eval branchtree (fun _ -> true) 3) &&
      eval branchtree (fun x -> not ((Var.index x) = 2)) 3 &&
      not (eval branchtree (fun x -> not (Var.equal x x2)) 3) &&
      let outvartree = Idd.(branch mgr y2 empty ident) in
      eval outvartree (fun x -> not (Var.equal x y2)) 3
    )

  (* Apply tests *)
  let%test "(x = 1) conj (x <- 0 + x <- 1)" = 
    Idd.(equal (apply mgr (&&) (branch mgr (Var.inp 0) ident empty) 
                  (branch mgr (Var.out 0) ident ident))
           (branch mgr (Var.inp 0) ident empty))

  let%test "(x = 1) disj (x <- 0 + x <- 1)" = 
    Idd.(equal (apply mgr (||) (branch mgr (Var.inp 0) ident empty) 
                  (branch mgr (Var.out 0) ident ident))
           (branch mgr (Var.out 0) ident ident))

  (* Helper function to create a random environment where variable indices
    range from 0 to [n-1] *)
  let random_env n : Var.t -> bool =
    List.range 0 n |> List.concat_map ~f:(fun i -> [Var.inp i; Var.out i]) |>
    List.fold ~init:(fun _ -> failwith "unbound in environment")
      ~f:(fun acc var -> let value = Random.bool () in 
           fun v -> if Var.equal v var then value else acc v)
    
  let%test "apply-eval compatibility" = 
    List.cartesian_product small_trees small_trees |>
    List.cartesian_product [(||); (&&)] |>
    List.for_all ~f:Idd.(fun (op, (u,v)) -> 
        let maxu = (index u) + 1 in
        let maxv = (index v) + 1 in
        let env = random_env (max maxu maxv) in
        let ur = (op (eval u env (max maxu maxv)) (eval v env (max maxu maxv))) in
        let app = apply mgr op u v in
        let vr = (eval app env (max maxu maxv)) in
        Bool.equal ur vr)
end
