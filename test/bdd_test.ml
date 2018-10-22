open Idd

module A : Boolean.Algebra = Bdd.Make()
module Boolean_tests = Boolean_test.Make(A)
