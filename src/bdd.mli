(** Reduced Ordered Binary Decision Diagrams (BDDs). *)

(** {2 Types} *)

(** A BDD is just a DD with two additional structural constraints:
      - {b Ordered}: the variables along any root-leaf path of a BDD increase
        strictly monotonically.
      - {b Reduced}: The [hi] and [lo] subtrees of any branch are distinct. *)
type t = Dd.t

type manager
val manager : unit -> manager

val conj : manager -> t -> t -> t
val disj : manager -> t -> t -> t
val neg : manager -> t -> t


(** BDDs form a boolean algebra. *)
module Make () : Boolean.Algebra
  with type Predicate.t = t
