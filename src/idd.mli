(** Identity Suppressed Decision Diagrams (IDDs). *)

(** {2 Types} *)

(** An IDD is just a DD with two additional structural constraints that ensure
    canonicity: two IDDs encode the same function iff they are the same IDD. *)
type t = private Dd.t

val equal : t -> t -> bool

type manager
val manager : unit -> manager

(** {2 Constructors} *)

(** {2 Boolean operations} *)

(** {2 Semantics} *)

val eval : t -> (int -> bool) -> bool


