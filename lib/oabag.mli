module type HashedType = sig
  type t

  val hash : t -> int
end

module type BAG = sig
  type elt
  type t

  val create : int -> t
  (** [create cap] creates empty multiset with capacity = [cap] *)

  val to_list : t -> (elt * int) list
  (** [to_list multiset] is representation of multiset as list of items paired
      with multiplicities *)

  val to_rep_seq : t -> elt Seq.t
  (** [to_rep_seq multiset] is representation of multiset as sequence with
      repetitions *)

  val equal : t -> t -> bool
  (** [equal ms1 ms2] is true if contents of multistes is equal*)

  val size : t -> int
  (** [size multiset] is the count of distinct elements in the [multiset] *)

  val total : t -> int
  (** [total multiset] is the total count of all elements in the [multiset] *)

  val add : elt -> t -> t
  (** [add elem multiset] is a new multiset with [elem] added *)

  val merge : t -> t -> t
  (** [merge ms1 ms2] is a new multiset as a join of [ms1] and [ms2]*)

  val copy : t -> t
  (** [copy ms] is a new multiset with a data of [ms]*)

  val count : elt -> t -> int
  (** [count elem multiset] is the count [elem] elements in the [multiset] *)

  val mem : elt -> t -> bool
  (** [mem elem multiset] is if [elem] is present in the [multiset] *)

  val remove : elt -> t -> t
  (** [remove elem multiset] returns a new multiset with one [elem] removed if
      present. Does not affect multiset if [elem] is not present *)

  val of_list : elt list -> t
  (** [of_list lst] creates multiset from list *)

  val fold : ('acc -> elt * int -> 'acc) -> 'acc -> t -> 'acc
  (** [fold f init ms] is result of applying function [f] to distinct elements
      in multiset in such that result would be [f](...([f] ([f] [init] x1 c1) x2
      c2) ...), where c1 is multiplicity of element x1. Order is not defined *)

  val filter : (elt * int -> bool) -> t -> t
  (** [filter pred ms] is new multiset which is a copy of [ms], but only with
      element that match [pred]. Iterates over distinct elements and their
      multiplicities, when [pred] is false, whole equal group is removed*)

  val map : (elt -> elt) -> t -> t
  (** [map f ms] is new multiset accuired by applying [f] to each distinct
      element of [ms] and replaces all its copies with return value of [f]. It
      does not distinguish between copies of element and calls predicate once
      for all*)

  val mapc : (elt * int -> elt * int) -> t -> t
  (** [mapc f ms] is new multiset accuired by applying [f] to each distinct
      element of [ms] with its multiplicity, [f] should return new value and its
      new multiplicity. It does not distinguish between copies of element and
      calls predicate once for all*)
end

(** [Make Typ] is a functor for building BAG of specified type [Typ]*)
module Make (Typ : HashedType) : BAG with type elt = Typ.t
