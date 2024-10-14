module type HashedType = sig
  type t
  (* val hash : t -> int *)
end

module type BAG = sig
  type elt
  type t

  val create : int -> t
  (** [create cap] creates empty multiset with capacity = [cap] *)

  val add : elt -> t -> t
  (** [add elem multiset] is a new multiset with [elem] added *)

  val remove : elt -> t -> t
  (** [remove elem multiset] returns a new multiset with one [elem] removed if
      present. Does not affect multiset if [elem] is not present *)

  val size : t -> int
  (** [size multiset] is the count of distinct elements in the [multiset] *)

  val total : t -> int
  (** [total multiset] is the total count of all elements in the [multiset] *)

  val count : elt -> t -> int
  (** [count elem multiset] is the count [elem] elements in the [multiset] *)

  val mem : elt -> t -> bool
  (** [mem elem multiset] is if [elem] is present in the [multiset] *)

  val to_seq : t -> elt Seq.t
  (** [to_seq multiset] is representation of set as sequence with repetitions *)
end

(** [Make Typ] is a functor for building BAG of specified type [Typ]*)
module Make (Typ : HashedType) : BAG with type elt = Typ.t
