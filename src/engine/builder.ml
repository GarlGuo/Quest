(* #require "yojson";;
#mod_use "../game_models/maps.ml";;
#mod_use "../game_models/player.ml";;
#mod_use "../game_models/enemy.ml";;
#mod_use "../game_models/foods.ml";;
#mod_use "../game_models/weapons.ml";; *)

open Maps
open Player
open Enemy
open Foods
open Weapons
open Yojson.Basic.Util



(******************************* In-game State ********************************)



(*some constructors below required an id, which is created by the functions *)
(*instead of contained in json                                              *)

(** The abstract type of values representing an enemy in the game engine *)
type enemy = 
  | Enemy of Enemy.t
  | Deleted

(** The abstract type of values representing a player in the game engine *)
type player = 
  | Player of Player.t 
  | Died

(** The abstract type of values representing a food item in the game engine *)
type food_item = 
  | Food of Food.food
  | Eaten (*once a weapon or food has been taken, this weapon becomes null *)

(** The abstract type of values representing a weapon item in the game engine*)
type weapon_item =
  | Weapon of  Weapon.weapon
  | Empty

(** The abstract type of values representing a map param in the game engine *)
type map_param = MapParam.map_param

(** The abstract type of values representing the current map state *)
type current_map = Maps.t

(** The exception type of a successful food ate or weapon equipped.
    Used to notify that an operation is successful. *)
exception SuccessExit

(**The exception type of a player who is dead *)
exception PlayerDied


(** The abstract type of values representing the current game state *)
type state = {
  mutable player: player;
  mutable food_inventory: food_item array;
  mutable weapon_inventory: weapon_item array;
  mutable player_old_loc: (int * int);
  mutable current_map_in_all_maps: int;

  (* Array doesn't have a find function, so I use List instead *)
  mutable branched_map_info: ((int * int) * string) list; 
  mutable current_map: current_map;
  mutable all_enemies_in_current_map: enemy array;
  mutable all_foods_in_current_map: food_item array;
  mutable all_weapons_in_current_map: weapon_item array;

  mutable all_maps: current_map list; (*persistent map *)
  mutable all_foods: food_item array array;
  mutable all_weapons: weapon_item array array;
  (* each element represents an array of enemies in ONE map *)
  mutable all_enemies: enemy array array; 
}



(*------------------------------ helper methods ------------------------------*)

(**[branch_map_store] keeps track of the list of item names and their 
   corresponding locations.  *)
let branch_map_store = ref []

(**[update_branch_map_store (l,n)] updates [branch_map_store] by 
   appending [(l,n)] to the front of the list referenced 
   by [branch_map_store].  *)
let update_branch_map_store (loc, name) = (*look at the ref right above *)
  let tmp = !branch_map_store in
  if List.mem (loc, name) tmp 
  then ()
  else branch_map_store := (loc, name) :: tmp


(**[count ()] returns an #, which represents the # of times
   this function has been called. Each time this function gets called,
   the returns will be incremented. *)
let count = 
  let counter = ref 0 in fun () -> (incr counter; !counter)


(**[probabilty s] produces a bool based on probabilty [s]
   Requires:
   [s] mod 0.1. 0.1 <= [s] <= 1.0*)
let probability s = 
  (* 0 <= x <= 10 *)
  let x = Int.to_float (Random.int 10) in
  x <= s *. 10.0


(**[random_choice lst] is a random object chosen from list [lst]
   Requires:
   [lst] cannot be empty*)
let random_choice list = 
  list |> List.length |> Random.int |> List.nth list


(**[random_list_with_fixed_length lst len] is a randomly chosen list
   with length [len] and its elements from [lst]*)
let random_list_with_fixed_length list len =
  List.map (fun _ -> random_choice list) 
    (Array.make len 0 |> Array.to_list)


(**[contains s s1] is true if [s] contains substring [s1], [false] 
   otherwise *)
let contains s1 s2 =
  let rec counter count = 
    (*let re = Str.regexp_string s2
      in try ignore (Str.search_forward re s1 0); true
      with Not_found -> false*)
    if String.length s1 - count < String.length s2 
    then false
    else if String.sub s1 count (String.length s2) = s2
    then true
    else counter (count + 1) in
  counter 0


(**[random_int_array_for_enemies_and_items arr num] returns a 
   probability-driven random int array with the number [num] and the 
   location array [arr] *)
let random_int_array_for_enemies_and_items map_size_array number =
  let round f = truncate (f +. 0.5) in
  let raw_prob = map_size_array |> Array.to_list in
  let rec total_sum num = function
    | [] -> num
    | h::d -> total_sum (num + h) d in
  let sum = total_sum 0 raw_prob in
  let float_sum = float_of_int sum in
  let float_num = float_of_int number in
  let temp_random_number = (*the probability oper here is pretty messy *)
    List.map (fun s -> 
        (float_of_int s) /. float_sum *. float_num |> round) raw_prob in 
  let tl = List.tl temp_random_number in
  number - total_sum 0 tl :: tl
  |> Array.of_list


(**[sorted_list locs col row n] is [List.rev [(col, row), (col-1, row), ... , 
   (1, row), (col, row-1), (col-1, row-1), ...]] containing [n] elements,
   where none of the coordinates in [locs] get appended. 
   Requires: 0 <= [n] <= [col * row] *)
let sorted_list loc_list col row length = 
  let rec inner_looper col' row' finished count = 
    if count = 0 then finished
    else let r_loc = (col', row') in
      let check = List.mem r_loc loc_list in
      if check && col' = 1 && row' > 1 then
        inner_looper col (row' - 1) finished count
      else if check && col' <> 1 then (* col' != 1 *)
        inner_looper (col'-1) (row') finished count
      else if col' = 1 && row' > 1
      then 
        inner_looper col (row'-1) ((col', row')::finished) (count - 1)
      else if col'= 1 && row' = 1 && count = 1
      then 
        failwith "impossible length"
      else 
        inner_looper (col'-1) (row') ((col', row')::finished) (count - 1) in
  inner_looper col row [] length


(**[unique_location_list locs col row n] returns [sorted_list col row n] if 
   [col < n/2] or [row < n/2], otherwise it returns a randomly constructed
   set-like list of [(col, row)] coordinates with length [n]. Note that 
   none of the elements in [locs] will be appended to the returned list. *)
let unique_location_list ~loc_array ~col ~row ~number =
  let loc_list = loc_array |> Array.to_list in
  if  (List.length loc_list) + number > (col * row) - 4 then
    (* small map. A sorted list is better for minimizing time complexity*)
    sorted_list loc_list col row number 
  else
    let rec constructor finished count =
      if count = 0 then finished |> List.rev
      else let r_loc = (1 + Random.int col, 1 + Random.int row) in
        if List.mem r_loc finished || List.mem r_loc loc_list
        then constructor finished count (* try again *)
        else constructor (r_loc::finished) (count - 1) in
    constructor [] number


(**[parse_dims s] parses [s] and returns [(col, row)]. 
   Requires: [s] is in the form ["# cols, # rows"] *)
let parse_dims s = 
  let rows = List.nth (String.split_on_char ',' s) 0 in 
  let cols = List.nth (String.split_on_char ',' s) 1 in 
  cols |> int_of_string, rows |> int_of_string


(**[main_map_size_array map_array] is the list of total sizes 
   (product of dimensions) of all the maps in [map_array] *)
let main_map_size_array ~map_array : int array = 
  Array.map (fun map -> let x, y = size map in x * y) map_array


(**[main_map_col_row map_array] is the list of size dimensions of all the maps 
   in [map_array] *)
let main_map_col_row ~map_array = 
  Array.map (fun map -> map.size) map_array



(****************************** Models Builder ********************************)



(**[gainable_skill_constructor jsons] returns the list of gainable skills 
   parsed from the json list [jsons] *)
let gainable_skill_constructor jsons_list = 
  if jsons_list = [] then []
  else
    List.map (fun skill -> 
        let description = skill |> member "description" |> to_string in
        let strength = skill |> member "strength" |> to_int in
        let name = skill |> member "name" |> to_string in
        let cd = skill |> member "cd" |> to_int in
        Player.skill_constructor name description strength cd) jsons_list


(**[browse_dir_enemy h lst] is a list of enemy json files 
   extracted from the directory handler [h] and file list [lst]. 
   Requires: the files in the directory handler [h] must be in valid enemy 
   name format - ["enemy-*.json"], in which ["*"] represents different 
   enemy names*)
let rec browse_dir_enemy (handler: Unix.dir_handle) (lst: string list) =
  match Unix.readdir handler with
  | exception _ -> 
    Unix.closedir handler; 
    lst |> List.rev
  | h -> 
    let pos = String.length h in 
    if pos > 11
    && String.sub h 0 6 = "enemy-" 
    && String.sub h (pos-5) 5 = ".json" 
    then browse_dir_enemy handler (h::lst) 
    else browse_dir_enemy handler lst

(**[enemy_skill_constructor skills] returns the parsed list of enemy skills
   in the json list [skills]. *)
let enemy_skill_constructor skills =       
  List.map (fun x -> 
      let skill_name = x |> member "name" |> to_string in
      let skill_strength = x |> member "strength" |> to_int in
      let skill_probability = x |> member "probability" |> to_float in
      Enemy.single_skill_constructor ~skill_name ~skill_strength
        ~skill_probability) skills

(**[single_enemy_builder j (col, row)] constructs a new enemy represented 
   by the json [j] at location [(col, row)] *)
let single_enemy_builder j (col, row) =
  Enemy (
    let name = j |> member "name" |> to_string in
    let id = count () |> string_of_int in
    let descr = j |> member "description" |> to_string in
    let exp = j |> member "experience" |> to_int in
    let level = j |> member "level" |> to_int in
    let pos = (col, row) in  (* random init pos *)
    let hp = j |> member "HP" |> to_int in
    let max_hp = hp in
    let lst = j |> member "skills" |> to_list in
    let skills = enemy_skill_constructor lst in
    let gainables =
      j |> member "gainable"|> to_list |> gainable_skill_constructor in 
    Enemy.constructor ~pos ~level ~exp
      ~hp ~id ~name ~descr ~max_hp ~skills ~gainables)

(** [browse_one_enemy_json j col row] calls 
    [single_enemy_builder j col row] if [j] is a valid enemy json
    representation. 
    Raises: [Failure "something wrong with browse_dir_enemy. Check it"] if 
    [j] is not a valid enemy json representation. *)
let browse_one_enemy_json j ~col ~row = 
  if contains j "witch" || contains j "minion" || contains j "goblin"
  then single_enemy_builder (Yojson.Basic.from_file j) (col, row)
  else failwith "something wrong with browse_dir_enemy. Check it"

(**[main_engine_ememy_for_single_map locs num col row] returns an array 
   of enemies parsed from reading the json files in this main directory that 
   start with ["enemy-"]. 
   Raises: [Failure "NONE of 'enemy' json exists"] if none of the json 
   file names are contain ["enemy"] *)
let main_engine_ememy_for_single_map ~loc_array ~number ~col ~row = 
  try 
    let all_enemy_models = browse_dir_enemy (Unix.opendir ".") [] in
    let expected_enemy_models =
      random_list_with_fixed_length all_enemy_models number in
    List.map2  (fun x (col, row)-> browse_one_enemy_json x ~col ~row)
      expected_enemy_models
      (unique_location_list ~loc_array ~col ~row ~number) |> Array.of_list
  with Unix.Unix_error (Unix.ENOENT, _ ,_ ) ->
    raise (Failure "NONE of 'enemy' json exists")


(**[main_engine_enemy map_locs loc_arr num_arr] reads all enemy json files in 
   current directory with the corresponding locations in [loc_arr] and 
   numbers in [num_arr], and returns a mapped 2d array with this information *)
let main_engine_enemy 
    ~map_col_row_array ~loc_array final_number_array : enemy array array =
  Array.map2
    (fun number (col, row) -> 
       main_engine_ememy_for_single_map loc_array number col row)
    final_number_array map_col_row_array


(**[main_engine_player ()] is the main execution method for the 
   constructing the main player, which gets returned at the end of this
   function call. *)
let main_engine_player (): player =
  let rec read_map handler =
    match Unix.readdir handler with
    | exception _ -> Unix.closedir handler; 
      failwith "player.json is not in current directory"
    | s -> if s = "player.json"
      then let player_json = s |> Yojson.Basic.from_file in
        let level = player_json |> member "level" |> to_int in
        let strength = player_json |> member "strength" |> to_int in
        let health = player_json |> member "health" |> to_int in
        let experience = 0 in
        Player.constructor ~health ~level ~strength ~experience ()
      else read_map handler in
  Player (Unix.opendir "../json_models" |> read_map)


(**[food_array_builder loc_array col row j_arr] constructs a new food array 
   represented by the json array [j_arr] with the dimensions [col] by [row],
   without the locations in [loc_array]. *)
let food_array_builder ~loc_array ~col ~row jsons: food_item array = 
  let number = List.length jsons in
  jsons |> List.map2 
    (fun (col, row) j -> let id = count () in
      let health = j |> member "health" |> to_int in
      let strength = j |> member "strength" |> to_int in
      let name = j |> member "name" |> to_string in
      let description = j |> member "description" |> to_string in
      let gainables = j |> member "gainables" |> to_list 
                      |> gainable_skill_constructor in
      Food (Food.constructor ~col ~row ~health 
              ~description ~name ~id ~strength ~gainables))
    (unique_location_list ~loc_array ~col ~row ~number) 
  |> Array.of_list


(**[main_engine_food_for_single_map locs num cols rows] reads the file 
   ["foods.json"] from the current directory, and returns the food array parsed 
   from that file, with dimensions [cols] by [rows] and [num] elements, 
   without the locations in [locs].  *)
let main_engine_food_for_single_map ~loc_array ~number ~col ~row = 
  let rec read_food handler = 
    match Unix.readdir handler with 
    | exception _ -> Unix.closedir handler; 
      failwith "foods.json is not in current directory"
    | json ->  let pos = String.length json in 
      if String.length json >= 10
      && String.sub json (pos-5) 5 = ".json" 
      && contains json "foods"
      then 
        let j_lists = json 
                      |> Yojson.Basic.from_file 
                      |> to_list in
        let expected_food_models =
          random_list_with_fixed_length j_lists number in
        food_array_builder ~loc_array ~col ~row expected_food_models
      else read_food handler in
  read_food (Unix.opendir "../json_models")


(**[main_engine_food dims locs nums] calls [main_engine_food_for_single_map] 
   for each map dimension in [dims], location in [locs], and final number in 
   [nums], and returns the mapped 2d food array with this information.  *)
let main_engine_food ~map_col_row_array ~loc_array final_number_array =
  Array.map2
    (fun number (col, row) -> 
       main_engine_food_for_single_map loc_array number col row)
    final_number_array map_col_row_array


(**[weapon_array_builder locs cols rows j_arr] constructs a new weapon array 
   represented by the json array [j_arr] with the dimensions [cols] by [rows], 
   without the locations in [locs]. *)
let weapon_array_builder ~loc_array ~col ~row jsons: weapon_item array =
  let number = List.length jsons in
  jsons 
  |> List.map2 
    (fun (col, row) j -> let id = count () in
      let name = j |> member "name" |> to_string in
      let description = j |> member "description" |> to_string in
      let strength = j |> member "strength"|> to_int in
      let gainables = j |> member "gainables" |> to_list 
                      |> gainable_skill_constructor in
      Weapon (Weapon.constructor ~strength ~col ~row 
                ~description ~name ~id ~gainables))
    (unique_location_list ~loc_array ~col ~row ~number)
  |> Array.of_list


(**[main_engine_weapon_for_single_map locs cols rows num] reads the file 
   ["weapons.json"] in the current directory and returns the parsed weapon 
   item array with dimensions [cols] by [rows] and [num] elements, without
   the locations in [locs]. 

   Raises: [Failure "weapons.json is not in current directory"] if the 
   the current directory does not contain the file ["weapons.json"] *)
let main_engine_weapon_for_single_map ~loc_array ~col ~row ~number =
  let rec read_weapon handler = 
    match Unix.readdir handler with 
    | exception _ -> Unix.closedir handler; 
      failwith "weapons.json is not in current directory"
    | json -> 
      let pos = String.length json in 
      if String.length json >= 12
      && String.sub json (pos-5) 5 = ".json" 
      && contains json "weapons"
      then (let jsons_list = json |> Yojson.Basic.from_file 
                             |> to_list in
            let expected_w_models = 
              random_list_with_fixed_length jsons_list number in
            weapon_array_builder ~loc_array ~col ~row expected_w_models)
      else read_weapon handler in
  read_weapon (Unix.opendir "../json_models")

(**[main_engine_weapon map_col_row_array loc_array final_number_array] calls 
   [main_engine_weapon_for_single_map] for each dimension in 
   [map_col_row_array], each location in [loc_array] 
   and each final number in [final_number_array], and returns a proper 
   2d weapon array with this information. *)
let main_engine_weapon ~map_col_row_array ~loc_array final_number_array = 
  Array.map2 
    (fun number (col, row) -> 
       main_engine_weapon_for_single_map loc_array col row number)
    final_number_array map_col_row_array


(**[map_param_array_builder j_arr flag] constructs a new map param array 
   represented by the json array [j_arr]. If [flag] is [true], then the 
   locations will be appended to the parsed map link. *)
let map_param_array_builder jsons flag : ((int * int) * map_param) list = 
  jsons |> List.map (fun j ->
      let name = j |> member "name" |> to_string in 
      let loc = j |> member "loc" |> to_string in
      let link = j |> member "link" |> to_string in 
      (*updating branched loc*)
      let col = parse_dims loc |> fst in 
      let row = parse_dims loc |> snd in
      let _ = if flag && link <> "" 
        then update_branch_map_store ((col, row), link)
        else () in
      (* i don't think the later check is necessary *)
      (col,row), MapParam.single_map_element_constructor ~name ~link)


(**[build_one_map s] returns the constructed map from the json file name [s].
   Requires: [s] must be in the form ["map-param*.json"] where ["*"] denotes
   any string. *)
let build_one_map s = 
  let json = s |> Yojson.Basic.from_file in 
  let name = json |> member "name" |> to_string in 
  let size = json |> member "size" |> to_string |> parse_dims in 
  let picture_lists = json |> member "picture" |> to_list in
  let all_map_param = map_param_array_builder picture_lists (name = "main") in
  map_constructor ~size ~name ~all_map_param, size


(**[reformat_output_map comb_list] is the array representation of 
   [comb_list]. 

   Raises: [Failure "no main map in this directory"] if the main map does not 
   exist in this directory. *)
let reformat_output_map comb_list : current_map array * (int * int) array =
  let rec looper finished_map finished_loc = function
    | [] -> failwith "no main map in this directory"
    | (map, loc)::d ->
      if map.name = "main" then
        let remains_map, remains_loc = List.split d in
        (map::(finished_map @ remains_map)) |> Array.of_list, 
        (loc::(finished_loc @ remains_loc)) |> Array.of_list
      else looper (map::finished_map) (loc::finished_loc) d in
  looper [] [] comb_list


(**[main_engine_map_param ()] reads all files in the current directory 
   with the format ["map-param*.json"] where ["*"] can be any string, 
   and returns the parsed map representation. 

   Raises: [Failure "no map-param.json is in current directory"] if there
   is no file with the format ["map-param*.json"] *)
let main_engine_map_param () : (current_map array) * (int * int) array = 
  let rec read_map handler list = 
    match Unix.readdir handler with
    | exception _ -> Unix.closedir handler;
      if list = [] then failwith "no map-param.json is in current directory"
      (*the first map of the output MUST be main map*)
      else reformat_output_map list 
    | s -> let pos = String.length s in 
      if String.length s > 13 
      && String.sub s (pos-5) 5 = ".json" 
      && contains s "map-param"
      then 
        read_map handler ((build_one_map s)::list)
      else read_map handler list in 
  read_map (Unix.opendir "../json_models") []



(******************************** Game Getters ********************************)



(**[get_player s] returns the player if the player in game state [s]
   is alive. 
   Raises: [PlayerDied] if the player has already died.  *)
let get_player s = 
  match s.player with
  | Player p -> p
  | Died -> raise PlayerDied


(**[get_enemies s] is all the enemies in game state [s] *)
let get_enemies s = s.all_enemies_in_current_map


(**[get_map s] is the current map in game state [s] *)
let get_map s = s.current_map


(**[get_current_map_name s] is the name of the current map in game state [s] *)
let get_current_map_name s = s.current_map.name


(**[get_current_map_size s] is the size of the current map in game state [s] *)
let get_current_map_size s = s.current_map.size



(**************************** System Instructions *****************************)



(** [sys_json] is the json that represent file "instr.json"*)
let sys_json = Yojson.Basic.from_file "../json_models/instr.json"


(**[main_map_instr] is the instruction in which the player has entered the 
   main map. *)
let main_map_instr s = (* player *)
  let loc = Player.location (get_player s) in
  if (List.filter (fun (ent, _) -> ent = loc) s.branched_map_info) <> []
  then sys_json |> member "main_map_instr_to_branch" |> to_string
  else sys_json |> member "main_map_else" |> to_string


(**[branch_map_instr] is the instruction in which the player has entered
   a branched map.   *)
let branch_map_instr = 
  sys_json |> member "branch_map_instr" |> to_string


(**[string_of_loc (col,row)] is [" (col, row) "] *)
let string_of_loc (col, row) = 
  Printf.sprintf " (%d, %d) " col row

(**[enemy_instr_helper s store] loads the instruction regarding enemies at 
   state [s]. [store] keeps track of what the instruction is. 
   Raises: [Failure "All enemies are dead in this map. Congratulations! "] 
   if there all enemies in the map at state [s] have been killed. 
   Raises: [SuccessExit] if the enemy instruction has been loaded successfully
*)
let enemy_instr_helper s store =
  for i = 0 to Array.length s.all_enemies_in_current_map - 1 do
    match s.all_enemies_in_current_map.(i) with
    | Enemy e -> store := 
        "Enemy " 
        ^ (Enemy.get_name e) ^ " is in location" 
        ^ (e |> Enemy.get_pos |> string_of_loc)
        ^ "\nTry to defeat it!"; 
      raise SuccessExit
    | _ -> ()
  done;
  raise (Failure "All enemies are dead in this map. Congratulations! ")


(**[enemy_instr s] is the instruction regarding enemies at state [s] *)
let enemy_instr s =
  let store = ref "" in
  try
    enemy_instr_helper s store
  with 
  | SuccessExit -> !store
  | Failure s -> s


(**[food_instr_helper s store] loads the instruction regarding foods at 
   state [s]. [store] keeps track of what the instruction is. 
   Raises: [Failure "There are no food in this map now"] if there is no 
   such food in the map at state [s]. 
   Raises: [SuccessExit] if the food instruction has been loaded successfully
*)
let food_instr_helper s store =
  for i = 0 to Array.length s.all_foods_in_current_map - 1 do
    match s.all_foods_in_current_map.(i) with
    | Food f -> store := 
        "Food " 
        ^ (Food.get_name f) ^ " is in location" 
        ^ (f |> Food.get_loc |> string_of_loc)
        ^ "\nMove there and take it!"; 
      raise SuccessExit
    | _ -> ()
  done;
  raise (Failure "There are no food in this map now/")


(**[food_instr s] is the instruction regarding foods at state [s] *)
let food_instr s  =
  let store = ref "" in
  try
    food_instr_helper s store
  with 
  | SuccessExit -> !store
  | Failure s -> s


(**[weapon_instr_helper s store] loads the instruction regarding weapons at 
   state [s]. [store] keeps track of what the instruction is. 
   Raises: [Failure "There are no weapon in this map now"] if there is no 
   such weapon in the map at state [s]. 
   Raises: [SuccessExit] if the weapon instruction has been loaded successfully
*)
let weapon_instr_helper s store  =
  for i = 0 to Array.length s.all_weapons_in_current_map - 1 do
    match s.all_weapons_in_current_map.(i) with
    | Weapon w -> store := 
        ("Food " 
         ^ (Weapon.get_name w) ^ " is in location" 
         ^ (w |> Weapon.get_loc |> string_of_loc)
         ^ "\nMove there and equip it!"); 
      raise SuccessExit
    | _ -> ()
  done;
  raise (Failure "There are no weapon in this map now/")


(**[weapon_instr s] is the instruction regarding weapons at state [s] *)
let weapon_instr s  =
  let store = ref "" in
  try
    weapon_instr_helper s store
  with 
  | SuccessExit -> !store
  | Failure s -> s


(**[system_instr s] is the instruction to the player at state [s] *)
let system_instr s  =
  let basic_instr = 
    match get_current_map_name s, s.player with
    | "main", Player _ -> main_map_instr s
    | name, Player t -> branch_map_instr
    | _, Died -> "" in
  let enemy_ins = enemy_instr s in
  let food_ins = food_instr s in
  let weapon_ins = weapon_instr s in
  basic_instr ^ "\n" ^ enemy_ins ^ "\n" ^ food_ins ^ "\n" ^ weapon_ins

