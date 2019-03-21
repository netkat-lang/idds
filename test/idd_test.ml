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
      let var = if n%2 = 0 then Var.inp (n/2) else Var.out (n/2) in
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
end
