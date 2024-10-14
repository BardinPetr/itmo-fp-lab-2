module type HashedType = sig
  type t
  (* val hash : t -> int *)
end

module type BAG = sig
  type elt
  type t

  val create : int -> t
  val add : elt -> t -> t
  val remove : elt -> t -> t
  val size : t -> int
  val total : t -> int
  val count : elt -> t -> int
  val mem : elt -> t -> bool
  val to_seq : t -> elt Seq.t
end

module Make (Typ : HashedType) = struct
  type elt = Typ.t

  type 'a cell =
    | Empty
    | Deleted
    | Value of 'a * int (* element x multiplicity *)

  type t = { capacity : int; total : int; size : int; data : elt cell array }

  (* private section *)

  (** [hash x cap] is hash value for [x] bound to array capacity [cap]*)
  let hash x cap = Hashtbl.hash x mod cap

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
    { capacity = cap; total = 0; size = 0; data = Array.make cap Empty }

  (** [size multiset] is the count of distinct elements in the multiset *)
  let size ms = ms.size

  (** [total multiset] is the total count of all elements in the multiset *)
  let total ms = ms.total

  (** [to_seq multiset] is representation of set as sequence with repetitions *)
  let to_seq ms =
    ms.data
    |> Array.to_seq
    |> Seq.filter_map (function
         | Empty | Deleted -> None
         | Value (x, cnt) -> Some (Seq.repeat x |> Seq.take cnt))
    |> Seq.concat

  (** [add elem multiset] is a new multiset with [elem] added *)
  let add item ms =
    match find_insert_id item ms with
    | None -> failwith "No space"
    | Some (selected_id, cur_cnt) ->
        {
          data =
            Array.mapi
              (fun i old ->
                if i = selected_id then Value (item, cur_cnt + 1) else old)
              ms.data;
          size = (ms.size + if cur_cnt = 0 then 1 else 0);
          total = ms.total + 1;
          capacity = ms.capacity;
        }

  (** [count elem multiset] is the count [elt] elements in the [multiset] *)
  let count item ms =
    match find_cell item ms with Some (_, count) -> count | None -> 0

  (** [mem elem multiset] is if [elt] is present in the [multiset] *)
  let mem item ms = count item ms != 0

  (** [remove elem multiset] returns a new multiset with one [elem] removed if
      present *)
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
end

module IntBag = Make (Int)

let m =
  let open IntBag in
  create 5 |> add 31 |> add 2342 |> add 123 |> add 31 |> add 123 |> add 123
;;

m
|> IntBag.to_seq
|> Seq.map string_of_int
|> List.of_seq
|> String.concat ", "
|> print_string
