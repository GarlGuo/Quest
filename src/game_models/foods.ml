#mod_use "player.ml";;

open Player

(** [F] is the interface for foods*)
module type F = sig 
  (** The abstract type of values representing foods. *)
  type food 

  (** Constructor of a food *)
  val constructor : 
    row:int ->
    col:int ->
    health:int ->
    description:string -> 
    name:string -> 
    id:int -> 
    strength:int -> 
    gainables: Player.skill list ->
    food

  (**[get_name f] is the name of the food [f]  *)
  val get_name : food -> string

  (**[get_description f] is the description of the food [f]  *)
  val get_description : food -> string

  (**[get_strength f] is the strength of the food [f]  *)
  val get_strength : food -> int

  (**[get_health f] is the health of the food [f]  *)
  val get_health : food -> int

  (**[get_loc f] is the location of the food [f]  *)
  val get_loc : food -> int * int

  (**[set_loc f loc] sets the location of the food [f] to [loc]  *)
  val set_loc : food -> int * int -> unit

  (**[get_gainables f] is the skill gainables of food [f] *)
  val get_gainables: food -> Player.skill list
end

(** [Food] is the data abstraction type representing food*)
module Food : F = struct
  (** Documentation in F*)

  (** The abstract type of values representing foods. *)
  type food = {
    name : string;
    description : string;
    id : int;
    health : int;
    strength : int;
    gainables: Player.skill list;
    mutable location : int * int;
  }
  (** Constructor of a food *)
  let constructor 
      ~row ~col ~health ~description ~name ~id ~strength ~gainables = {
    name = name;
    id = id;
    strength = strength;
    health = health;
    location = (col,row);
    description = description;
    gainables = gainables;
  }

  (**[get_name f] is the name of the food [f]  *)
  let get_name f = f.name

  (**[get_description f] is the description of the food [f]  *)
  let get_description f = f.description

  (**[get_strength f] is the strength of the food [f]  *)
  let get_strength f = f.strength

  (**[get_health f] is the health of the food [f]  *)
  let get_health f = f.health

  (**[get_loc f] is the location of the food [f]  *)
  let get_loc f = f.location

  (**[set_loc f loc] sets the location of the food [f] to [loc]  *)
  let set_loc f loc = f.location <- loc

  (**[get_gainables f] is the skill gainables of food [f] *)
  let get_gainables f = f.gainables
end

