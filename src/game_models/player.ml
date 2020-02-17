#mod_use "maps.ml";;

open Maps

module type P = sig

  (** The abstract type of values representing a player's skill. *)
  type skill

  (** The abstract type of values representing a player. *)
  type t

  (** The exception type of an unknown skill. *)
  exception Unknownskill of string

  (** [constructor s h l e ()] constructs a new player module located at
      row 1, col 1, with strength [s], health [h], experience [e], 
      at level [l]. *)
  val constructor: 
    ?strength:int ->
    ?health:int -> 
    ?level:int -> 
    ?experience:int -> 
    ?loc: (int * int) ->
    unit -> t

  (**[level p] is the current level of player [p] *)
  val level : t -> int

  (** [location p] is the current location of player [p]. *)
  val location : t -> int * int

  (** [row p] is the current row coordinate of player [p]. *)
  val row : t -> int 

  (** [col p] is the current col coordinate of player [p]. *)
  val col : t -> int 

  (** [health p] is the current health of player [p]. *)
  val health : t -> int

  (** [max_health p] is the maximum health of a player [p]*)
  val max_health: t -> int

  (** [experience p] is the current experience value of player [p]. *)
  val experience : t -> int 

  (**[move_up p m] moves player [p] to [(col, row+1)] within the bounds
     of map [m]. The player [p] stays in its place when such a move causes
     him to move out of bounds. *)
  val move_up : t -> Maps.t -> unit

  (**[move_down p m] moves player [p] to [(col, row-1)] within the bounds
     of map [m]. The player [p] stays in its place when such a move causes
     him to move out of bounds. *)
  val move_down : t -> Maps.t -> unit

  (**[move_right p m] moves player [p] to [(col+1, row)] within the bounds
     of map [m]. The player [p] stays in its place when such a move causes
     him to move out of bounds. *)
  val move_right : t -> Maps.t -> unit

  (**[move_left p m] moves player [p] to [(col-1, row)] within the bounds
     of map [m]. The player [p] stays in its place when such a move causes
     him to move out of bounds. *)
  val move_left : t -> Maps.t -> unit

  (** [reduce_health p h] reduces the health of player [p] by [h].
      Requires: [h] >= 0 *)
  val reduce_health : t -> int -> unit

  (** [reduce_strength p s] reduces the strength of player [p] by [s].
      Requires: [s] >= 0 *)
  val reduce_strength : t -> int -> unit

  (** [increase_experience p e] increases the experience of player [p] by [e].
      Requires: [e] >= 0 *)
  val increase_experience : t -> int -> unit 

  (**[increase_health p h] increases player [p]'s health by [h].*)
  val increase_health: t -> int -> unit

  (**[increase_strength p s] increases player [p]'s strength by [s].*)
  val increase_strength: t -> int -> unit

  (**[switch_loc p loc] changes the location of player [p] to [loc].  *)
  val switch_loc: t -> int * int -> unit (*this method is pretty dangerous !!*)

  (**[update_skill p lst] appends the new skill list [lst] to the existing
     skills repertoire of player [p] *)
  val update_skill: t -> skill list -> unit

  (** [level_up_expereince p] is 
      the experience required to level up player [p]*)
  val level_up_expereince: t -> int

  (**[advance_level p] advances player [p] to the next level and updates
     player [p]'s experience as well. If [p] does not have enough experience
     to advance, no change will occur. *)
  val advance_level: t -> unit

  (*  skill-related methods *)

  (** [skill_constructor p n d s] adds a skill to player [p] with 
      name [n], description [d] and strength [s]. *)
  val skill_constructor: 
    name:string ->
    description:string -> 
    strength:int -> 
    cd:int->
    skill

  (**[get_skill_by_skill_name p n] returns skill [s] with 
     the name [n] of player [p]. 
     Raises [UnknownSkill ("skill name " ^ [n] ^ "does not exist")] 
     if the player [p] does not have the skill named [n]. *)
  val get_skill_by_skill_name: t -> string -> skill

  (**[available_skills_list p] is the skills that the player [p] posesses. *)
  val available_skills_list: t -> skill list

  (**[skill_name s] is the name of the skill [s]. *)
  val skill_name: skill -> string

  (**[skill_strength s] is the strength of the skill [s]. *)
  val skill_strength: skill -> int

  (**[skill_description s] is the description of the skill [s]. *)
  val skill_description: skill -> string

  (**[choose_skill p s] will update the cd of skill [s] in player [p]*)
  val choose_skill: t -> skill -> unit

  (**[get_all_skill_format s] is a list of (skill name, skill cd)*)
  val get_all_skill_format : t -> (string * int) list
end

(** The abstraction data type representing a player*)
module Player : P = struct
  (** Documentation in P*)

  (** The abstract type of values representing a player's skill. *)
  type skill = {
    name: string;
    description: string;
    old_cd: int;
    mutable strength: int;
    mutable cd: int;
  }

  (** The abstract type of values representing a player. *)
  type t = {
    mutable location : int * int;
    mutable health : int;
    mutable level : int;
    mutable experience : int;
    mutable skills: skill list;
  }

  (** The exception type of an unknown skill. *)
  exception Unknownskill of string

  (** [skill_constructor n d s] constructs a new skill of 
      strength [s], name [n], description [d].  *)
  let skill_constructor ~name ~description ~strength ~cd = {
    description = description;
    strength = strength;
    name = name;
    cd = cd;
    old_cd = cd;
  }

  (** [constructor s h l e ()] constructs a new player module located at
        row 1, col 1, with strength [s], health [h], experience [e], 
        at level [l]. *)
  let constructor 
      ?strength:(strength=10) ?health:(health=100) 
      ?level:(level=1) ?experience:(experience=0) ?loc: (loc = (1,1)) () = 
    {
      location = loc;
      health = health;
      level = level;
      experience = experience;
      skills = [{
          (* basic skill *)
          name = "punch";
          description = "Basic attacks. 
                        Player uses fists to challenge the evils!";
          strength = strength;
          cd = 0;
          old_cd = 0;
        }];
    }

  (** [location p] is the current location of player [p]. *)
  let location p = p.location

  (** [health p] is the current health of player [p]. *)
  let health p = p.health

  (** [max_health p] is the maximum health of a player [p]*)
  let max_health p = 70 + 30 * p.level (* this could be mutable *)

  (** [experience p] is the current experience value of player [p]. *)
  let experience p = p.experience

  (**[level p] is the current level of player [p] *)
  let level p = p.level

  (** [col p] is the current col coordinate of player [p]. *)
  let col p = fst p.location

  (** [row p] is the current row coordinate of player [p]. *)
  let row p = snd p.location

  (** [move p m col_diff row_diff] moves player [t] to [col_diff] right and
      [row_diff] up on map [m]*)
  let move p m col_diff row_diff = 
    let col' = col_diff + (col p) in
    let row' = row_diff + (row p) in 
    if Maps.bound_check m col' row' then 
      p.location <- (col', row')
    else 
      ()

  (**[move_left p m] moves player [p] to [(col-1, row)] within the bounds
       of map [m]. The player [p] stays in its place when such a move causes
       him to move out of bounds. *)
  let move_left p m = move p m (-1) 0

  (**[move_right p m] moves player [p] to [(col+1, row)] within the bounds
       of map [m]. The player [p] stays in its place when such a move causes
       him to move out of bounds. *)
  let move_right p m = move p m 1 0

  (**[move_up p m] moves player [p] to [(col, row+1)] within the bounds
       of map [m]. The player [p] stays in its place when such a move causes
       him to move out of bounds. *)
  let move_up p m = move p m 0 1

  (**[move_down p m] moves player [p] to [(col, row-1)] within the bounds
       of map [m]. The player [p] stays in its place when such a move causes
       him to move out of bounds. *)
  let move_down p m = move p m 0 (-1)

  (**[increase_health p h] increases player [p]'s health by [h].*)
  let increase_health t hp = 
    let new_health = t.health + hp in
    let max = max_health t in
    if new_health >= max_health t 
    then t.health <- max
    else t.health <- t.health + hp

  (**[increase_strength p s] increases player [p]'s strength by [s].*)
  let increase_strength t st =
    let incr = st / 4 in
    List.iter (fun s -> (s.strength <- s.strength + incr);) t.skills

  (** [reduce_health p h] reduces the health of player [p] by [h].
        Requires: [h] >= 0 *)
  let reduce_health p h = 
    let new_health = 
      if p.health - h >= 0 then p.health - h else 0 
    in p.health <- new_health

  (** [reduce_strength p s] reduces the strength of player [p] by [s].
        Requires: [s] >= 0 *)
  let reduce_strength p str = 
    let temp_minus = str / (List.length p.skills) in
    List.iter 
      (fun skill -> let temp = skill.strength - temp_minus in
        if temp <= 0 then skill.strength <- 0
        else skill.strength <- temp) p.skills

  (** [level_up_expereince p] is 
        the experience required to level up player [p]*)
  let level_up_expereince p = 30 + 30 * p.level

  (**[advance_level p] advances player [p] to the next level and updates
       player [p]'s experience as well. If [p] does not have enough experience
       to advance, no change will occur. *)
  let advance_level p = 
    let experience_qual = level_up_expereince p in 
    if p.experience >= experience_qual 
    then
      (p.level <- p.level + 1;
       increase_health p 35;
       p.experience <- p.experience mod experience_qual;
       increase_strength p 40)
    else 
      ()

  (** [increase_experience p e] increases the experience of player [p] by [e].
        Requires: [e] >= 0 *)
  let increase_experience p e = 
    p.experience <- p.experience + e;
    advance_level p

  (**[get_skill_by_skill_name p n] returns skill [s] with 
       the name [n] of player [p]. 
       Raises [UnknownSkill ("skill name " ^ [n] ^ "does not exist")] 
       if the player [p] does not have the skill named [n]. *)
  let get_skill_by_skill_name t name = 
    match List.filter (fun x -> x.name = name ) t.skills with
    | [] -> raise (Unknownskill 
                     (Printf.sprintf "skill name %s does not exist" name))
    | h::_ -> h

  (**[available_skills_list p] is the skills that the player [p] posesses. *)
  let available_skills_list t =
    List.filter (fun skill -> skill.cd = 0) t.skills

  (**[skill_name s] is the name of the skill [s]. *)
  let skill_name skill = skill.name

  (**[skill_strength s] is the strength of the skill [s]. *)
  let skill_strength skill =  skill.strength

  (**[skill_description s] is the description of the skill [s]. *)
  let skill_description skill = skill.description

  (**[assert_skill_name_NOT_in_list p n] returns [true] if there are no 
     skills in the skill list of player [p] with name [n], and [false]
     otherwise.  *)
  let assert_skill_name_NOT_in_list t name =
    (List.filter (fun s -> s.name = name) t.skills) = []

  (**[update_skill p lst] appends the new skill list [lst] to the existing
      skills repertoire of player [p] *)
  let update_skill t skill_lst = 
    let new_skill_list = 
      t.skills @ 
      (List.filter 
         (fun s -> assert_skill_name_NOT_in_list t s.name) skill_lst) in
    t.skills <- new_skill_list

  (**[switch_loc p loc] changes the location of player [p] to [loc].  *)
  let switch_loc t loc = t.location <- loc

  (**[choose_skill p s] will update the cd of skill [s] in player [p]*)
  let choose_skill t s = 
    List.iter (fun skill -> let new_cd = skill.cd - 1 in
                if new_cd < 0 then () else skill.cd <- new_cd) t.skills;
    s.cd <- s.old_cd

  (**[get_all_skill_format s] is a list of (skill name, skill cd)*)
  let get_all_skill_format s =  
    List.map (fun skill -> (skill.name, skill.cd)) s.skills
end

(** skill is a representation of player skills *)
type skill = Player.skill (* export to other modules*)