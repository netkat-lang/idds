(** Decision Diagrams (DDs).

    This module serves as the base for the BDD and IDD modules. It provides
    hash-consed decision trees, but does not enforce any structural invariants
    on these trees beyond guranteeing that structural equality coincides with
    physical equality.

    A decision diagram (DD) is a finite binary tree whose leaves are labeled
    with the constants true or false, and whose branches are labeled with
    boolean variables. Each branch has two subtrees [hi] and [lo] for the cases
    that the variable is true or false, respectively.

    Given an assignment of booleans to the variables, one can evaluate such a
    tree in the following way:
      - If the tree is a leaf, return its boolean label.
      - If the tree is a branch labeled with the variable [x], recursively
        evaluate
        {ul
          {- the subtree [hi] if [x = true], or}
          {- the subtree [lo] if [x = false].}
        }

    The module enforces a {b key invariant}: If [u : t] and [v : t] were
    constructed using the same manager, then [id u = id v] if and only if
    [u] and [v] are structurally equal (ignoring IDs). In other words, all
    DDs built using the same manager are
    {{:https://en.wikipedia.org/wiki/Hash_consing} hash consed}.
*)


(** {2 Types} *)

(** Boolean variable on which a DD can branch. Morally just an integer, but
    modeled as a record for type safety. *)
type var = { idx : int }
  [@@unboxed]
  [@@deriving sexp]

(** The type of a decision diagram (DD). *)
type t = private
  | True
  | False
  | Branch of {
    var : var;   (** the variable on which to branch *)
    hi : t;      (** subdiagram for case [var = true] *)
    lo: t;       (** subdiagramm for case [var = false] *)
    id: int;     (** unique identifier for this diagram *)
  }


(** {2 Manager} *)

(** A DD manager [mgr : manager] encapsulates all state required for DD
    construction. In particular, it is required by the [branch] function
    to allocate unique identifiers for each DD. *)
type manager

(** Initialize a fresh manager *)
val manager : unit -> manager


(** {2 Constructors} *)

(** The constant false. *)
val cfalse : t

(** The constant true. *)
val ctrue : t

(** [branch mgr var hi lo] is the diagram that behaves like [hi] when
    [var = true], and like [lo] when [var = false]. *)
val branch : manager -> var -> t -> t -> t


(** {2 Generic operations on DDs} *)

(** O(1) structural equality.

    {b PRECONDITION}: The result of [equal u v] is only defined when [u] and [v]
    were built using the same manager. Otherwise, the result is arbitrary. *)
val equal : t -> t -> bool

(** [eval t env] evaluates the decision diagram [t] in the given environment
    [env : var -> bool].*)
val eval : t -> (var -> bool) -> bool


(** {2 Low-level API} *)

(** [id u] is an integer that is guranteed to be unique to the the diagram [u].
    In particular, the following invariant holds whenever [u] and [v] were
    constructed using the same manager: [id u = id v] iff the trees [u] and [v]
    are structurally equal (ignoring IDs).
    Thus, one can use the IDs to test for equality in O(1). IDs are also useful
    for memoizing functions on DDs. *)
val id : t -> int
