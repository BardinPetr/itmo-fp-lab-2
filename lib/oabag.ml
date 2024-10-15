module type HashedType = sig
  type t

  val hash : t -> int
end

module type BAG = sig
  type elt
  type t

  val create : int -> t
  val to_list : t -> (elt * int) list
  val to_rep_seq : t -> elt Seq.t
  val equal : t -> t -> bool
  val size : t -> int
  val total : t -> int
  val add : elt -> t -> t
  val merge : t -> t -> t
  val copy : t -> t
  val count : elt -> t -> int
  val mem : elt -> t -> bool
  val remove : elt -> t -> t
  val of_list : elt list -> t
  val fold : ('acc -> elt * int -> 'acc) -> 'acc -> t -> 'acc
  val filter : (elt * int -> bool) -> t -> t
  val map : (elt -> elt) -> t -> t
  val mapc : (elt * int -> elt * int) -> t -> t
end

module Make (Typ : HashedType) = struct
  let max_load_factor = 0.5

  type elt = Typ.t

  type 'a cell =
    | Empty
    | Deleted
    | Value of 'a * int (* element x multiplicity *)

  type t = { capacity : int; total : int; size : int; data : elt cell array }

  (* private section *)

  let hash x cap = Typ.hash x mod cap

  (** [probe cur step cap] is index that is probed on [step] iteration from
      [cur] position on array of capacity [cap] *)
  let linear_probe cur step cap = (hash cur cap + step) mod cap

  (** [find_insert_id elem ms] is the cell index and item its multiplicity in
      data array of multiset [ms] associated with [elem] or first empty cell
      after that via probing function. Returned multiplicity is 0 if element not
      matched equal. If table is full and searched element is not present, then
      returns None. *)
  let find_insert_id elem ms =
    let rec aux step =
      if step >= ms.capacity then None
      else
        let id = linear_probe elem step ms.capacity in
        match ms.data.(id) with
        | Empty -> Some (id, 0)
        | Value (value, cnt) when value = elem -> Some (id, cnt)
        | _ -> aux (step + 1)
    in
    aux 0

  (** [find_cell elem ms] is the cell in multiset [ms] data array associated
      with [elem] or None if no such present. Returns: cell id and multiplicity
      or None*)
  let find_cell elem ms =
    let rec aux step =
      if step >= ms.capacity then None
      else
        let id = linear_probe elem step ms.capacity in
        let cell = ms.data.(id) in
        match cell with
        | Empty -> None
        | Value (value, cnt) when value = elem -> Some (id, cnt)
        | _ -> aux (step + 1)
      (* Deleted are skipped *)
    in
    aux 0

  (* public api section *)

  (** [create cap] creates empty multiset with capacity = [cap] *)
  let create cap =
    let cap = max 1 cap in
    { capacity = cap; total = 0; size = 0; data = Array.make cap Empty }

  (** [to_list multiset] is representation of multiset as list of items paired
      with multiplicities *)
  let to_list ms =
    ms.data
    |> Array.to_list
    |> List.filter_map (function
         | Empty | Deleted -> None
         | Value (v, c) -> Some (v, c))

  (** [to_rep_seq multiset] is representation of multiset as sequence with
      repetitions *)
  let to_rep_seq ms =
    ms
    |> to_list
    |> List.to_seq
    |> Seq.map (fun (v, c) -> Seq.repeat v |> Seq.take c)
    |> Seq.concat

  (** [equal ms1 ms2] is true if contents of multistes is equal*)
  let equal a b =
    let open List in
    let sorted =
      [ a; b ] |> map (fun l -> l |> to_list |> sort Stdlib.compare)
    in
    hd sorted = nth sorted 1

  (** [size multiset] is the count of distinct elements in the multiset *)
  let size ms = ms.size

  (** [total multiset] is the total count of all elements in the multiset *)
  let total ms = ms.total

  (** [load_factor multiset] is current load factor of array*)
  let load_factor ms = float_of_int ms.size /. float_of_int ms.capacity

  (** [addm item n multiset] is a new multiset with [elem] added [n] times. *)
  let rec addm item n ms =
    if load_factor ms > max_load_factor then
      (* grow list and copy data *)
      ms
      |> to_list
      |> List.fold_left
           (fun acc (v, c) -> acc |> addm v c)
           (create (ms.capacity * 2))
      |> addm item n
    else
      match find_insert_id item ms with
      | None -> failwith "No space"
      | Some (selected_id, cur_cnt) ->
          {
            data =
              Array.mapi
                (fun i old ->
                  if i = selected_id then Value (item, cur_cnt + n) else old)
                ms.data;
            size = (ms.size + if cur_cnt = 0 then 1 else 0);
            total = ms.total + n;
            capacity = ms.capacity;
          }

  (** [add elem multiset] is a new multiset with [elem] added *)
  let add item ms = addm item 1 ms

  (** [merge ms1 ms2] is a new multiset as a join of [ms1] and [ms2]*)
  let merge ms1 ms2 =
    [ ms1; ms2 ]
    |> List.map to_list
    |> List.concat
    |> List.fold_left
         (fun acc (v, c) -> addm v c acc)
         (create (ms1.capacity + ms2.capacity))

  (** [copy ms] is a new multiset with a data of [ms]*)
  let copy ms =
    ms
    |> to_list
    |> List.fold_left (fun acc (v, c) -> addm v c acc) (create ms.capacity)

  (** [count elem multiset] is the count [elt] elements in the [multiset] *)
  let count item ms =
    match find_cell item ms with Some (_, count) -> count | None -> 0

  (** [mem elem multiset] is if [elt] is present in the [multiset] *)
  let mem item ms = count item ms != 0

  (** [remove elem multiset] returns a new multiset with one [elem] removed if
      present. Does not affect multiset if [elem] is not present *)
  let remove item ms =
    match find_cell item ms with
    | None -> ms
    | Some (selected_id, old_count) ->
        {
          data =
            ms.data
            |> Array.mapi (fun i old ->
                   if i = selected_id then
                     match old with
                     | Value (_, cnt) when cnt = 1 -> Deleted
                     | Value (x, cnt) -> Value (x, cnt - 1)
                     | _ -> failwith "invalid state"
                   else old);
          size = (ms.size + if old_count == 1 then 1 else 0);
          total = ms.total - 1;
          capacity = ms.capacity;
        }

  (** [of_list lst] creates multiset from list *)
  let of_list lst = List.fold_right add lst (create (List.length lst))

  (** [fold f init ms] is result of applying function [f] to distinct elements
      in multiset in such that result would be [f](...([f] ([f] [init] x1 c1) x2
      c2) ...), where c1 is multiplicity of element x1. Application order is not
      defined *)
  let fold func init ms =
    ms.data
    |> Array.to_seq
    |> Seq.filter_map (function
         | Empty | Deleted -> None
         | Value (i_val, i_cnt) -> Some (i_val, i_cnt))
    |> Seq.fold_left func init

  (** [filter pred ms] is new multiset which is a copy of [ms], but only with
      element that match [pred]. Iterates over distinct elements and their
      multiplicities, when [pred] is false, whole equal group is removed*)
  let filter pred ms =
    ms
    |> fold
         (fun acc_ms (v, c) ->
           if pred (v, c) then acc_ms |> addm v c else acc_ms)
         (create ms.capacity)

  (** [map f ms] is new multiset accuired by applying [f] to each distinct
      element of [ms] with its multiplicity, [f] should return new value and its
      new multiplicity. It does not distinguish between copies of element and
      calls predicate once for all*)
  let mapc f ms =
    ms
    |> to_list
    |> List.map f
    |> List.fold_left (fun acc (v, c) -> addm v c acc) (create ms.capacity)

  (** [map f ms] is new multiset accuired by applying [f] to each distinct
      element of [ms] and replaces all its copies with return value of [f]. It
      does not distinguish between copies of element and calls predicate once
      for all*)
  let map f ms = ms |> mapc (fun (v, c) -> (f v, c))
end
