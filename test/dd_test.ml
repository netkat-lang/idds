open Idd
open Base

module Basic = struct
  let%test "ctrue is True" =
    match Dd.ctrue with
    | True -> true
    | _ -> false

  let%test "cfalse is False" =
    match Dd.cfalse with
    | False -> true
    | _ -> false

  let%test "id cfalse <> id ctrue" =
    Dd.(id cfalse <> id ctrue)
end


module WithManager = struct
  let mgr = Dd.manager ()
  let ite idx = Dd.(branch mgr { idx })

  let t1 = Dd.(ite 0 ctrue ctrue)
  let t1' = Dd.(ite 0 ctrue ctrue)
  let t2 = Dd.(ite 0 cfalse cfalse)
  let t2' = Dd.(ite 0 cfalse cfalse)
  let t3 = ite 1 t1 t2
  let t3' = ite 1 t1' t2'
  let ts = [t1; t2; t3]

  let%test "id branch <> id ctrue" =
    List.for_all ts ~f:(fun t -> Dd.(id t <> id ctrue && id t <> id cfalse))

  let%test "branch memoization" =
    phys_equal t1 t1' && 
    phys_equal t2 t2' &&
    phys_equal t3 t3'

  let%test "equal is reflexive" =
    Dd.(equal ctrue ctrue) &&
    Dd.(equal cfalse cfalse) &&
    List.for_all ts ~f:(fun t -> Dd.equal t t)

  let%test "equal discrimates" =
    not Dd.(equal ctrue cfalse) &&
    List.for_all ts ~f:(fun t -> not Dd.(equal t ctrue || equal t cfalse)) &&
    not Dd.(equal t1 t2) &&
    not Dd.(equal t1 t3) &&
    not Dd.(equal t2 t3)

  
end
