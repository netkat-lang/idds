(** Identity Suppressed Decision Diagrams (IDDs). *)

(** {2 Types} *)

(** An IDD is just a DD with two additional structural constraints that ensure
    canonicity: two IDDs encode the same function iff they are the same IDD. *)
type t = private Dd.t


type manager

val manager : unit -> manager


(** {2 Constructors} *)

(** The identity relation. *)
val ident : t

(** The empty relation. *)
val empty : t

(** [branch mgr var hi lo] is the diagram that behaves like [hi] when
    [var = true], and like [lo] when [var = false]. *)
val branch : manager -> Var.t -> t -> t -> t


(** {2 Boolean operations} *)

(** O(1) structural equality.

    {b PRECONDITION}: The result of [equal u v] is only defined when [u] and [v]
    were built using the same manager. Otherwise, the result is arbitrary. *)
val equal : t -> t -> bool

(** {2 Semantics} *)

(** [eval tree env n] evaluates idd [tree] in environment [env] where the 
    variable indices are 0,...,[n]-1 *)
val eval : t -> (Var.t -> bool) -> int -> bool

