(** Reduced Ordered Binary Decision Diagrams (BDDs). *)

(** {2 Types} *)

(** A BDD is just a DD with two additional structural constraints:
      - {b Ordered}: the variables along any root-leaf path of a BDD increase
        strictly monotonically.
      - {b Reduced}: The [hi] and [lo] subtrees of any branch are distinct. *)
type t = private Dd.t

val equal : t -> t -> bool

type manager
val manager : unit -> manager

(** {2 Constructors} *)
val ctrue : t
val cfalse : t

(** {2 Boolean operations} *)

val conj : manager -> t -> t -> t (** boolean conjunction (logical and) *)
val disj : manager -> t -> t -> t (** boolean disjunction (logical or) *)
val neg : manager -> t -> t       (** boolean negation (logical not) *)
val test : manager -> int -> t -> t -> t

(** BDDs form a boolean algebra. *)
module Make () : Boolean.Algebra
  with type t = t

(** {2 Semantics} *)

val eval : t -> (int -> bool) -> bool


