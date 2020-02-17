(** [MP] is the interace for maps*)
module type MP = sig 
  (** The abstract type of values representing a map param. *)
  type map_param = {
    name: string; (*this represents a jpg name for this element*)
    link: string; (*link here represents another map file 
                    that current map element is linked to. If there is NO such
                    linked map, link will be empty string ""*)
  }

  (** Constructor of a map param *)
  val single_map_element_constructor : 
    name: string -> 
    link: string ->
    map_param

  (**[get_name mp] is the name of the map param [mp] *)
  val get_name: map_param -> string

  (**[get_link mp] is the link of the map param [mp] *)
  val get_link: map_param -> string

  (* the name and link of a map param should be IMMUTABLE. 
     There is NO setter method for this module *)
end


module MapParam : MP = struct 
  (** Documentation in MP*)

  type map_param = {
    name: string; (*this represents a jpg name for this element*)
    link: string; (*link here represents another map file 
                    that current map element is linked to. If there is NO such
                    linked map, link will be empty string ""*)
  }

  (** Constructor of a map param *)
  let single_map_element_constructor ~name ~link = {
    link = link;
    name = name;
  }

  (**[get_name mp] is the name of the map param [mp] *)
  let get_link param = param.link

  (**[get_link mp] is the link of the map param [mp] *)
  let get_name param = param.name

end

(** The abstract type of values representing a map. *)
type t = {
  size : int * int; (*total col * total rows *)
  name : string; (*this map name*)
  (**[|((col, row), map_param)|]*)
  map_params: ((int * int) * MapParam.map_param) list; 
}

(** Constructor of a map *)
let map_constructor 
    ~size ~name ~all_map_param = {
  size = size; (*total col * total rows *)
  name = name; (*this map name *)
  map_params = all_map_param; 
}

(**[size m] is the size of map [m]  *)
let size m = m.size

(**[bound_check m c r] returns whether column [c] and row [r] satisfy 
   is within the bounds of the map [m]. *)
let bound_check m c r = 
  let cols = fst m.size in 
  let rows = snd m.size in 
  c > 0 && c <= cols && r > 0 && r <= rows

(**[get_one_map_param_by_loc t loc] returns the map param at location [loc] 
   in map [t]. 
   Raises: [Failure] if [loc] is out of bounds with respect to map [t] *)
let get_one_map_param_by_loc t loc = 
  if bound_check t (fst loc) (snd loc)
  then 
    t.map_params 
    |> List.find (fun (single,_) -> single = loc)
    |> snd
  else failwith "the input loc exceeds the bound of this map"

(**[get_one_link_by_loc t loc] returns the link of the map param 
   at location [loc] in map [t]. 
   Raises: [Failure] if [loc] is out of bounds with respect to map [t] *)
let get_one_link_by_loc t loc =
  loc |> get_one_map_param_by_loc t |> MapParam.get_link