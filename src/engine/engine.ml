(* #require "yojson";;
#cd "../json_models";;
#mod_use "../game_models/maps.ml";;
#mod_use "../game_models/player.ml";;
#mod_use "../game_models/enemy.ml";;
#mod_use "../game_models/foods.ml";;
#mod_use "../game_models/weapons.ml";;
#mod_use "../engine/builder.ml";; *)

open Enemy
open Player
open Enemy
open Foods
open Weapons
open Builder



(****************************** Helper Methods ********************************)



(**[choose_skill_random t] is a (skill name, skill strength) for enemy [t], 
   chosen randomly from all of enemy [t]'s skills *)
let choose_skill_random (s: Enemy.t) =
  let rec inner_chooser lst finished = 
    match lst, finished with
    | [], _ -> finished
    | one::d, m -> 
      let name, prob, strength = one in
      let name1, strength1 = m in
      if probability prob && strength > strength1
      then (name, strength) |> inner_chooser d
      else inner_chooser d finished in
  let skills = s |> 
               Enemy.get_all_skills_name_prob_and_strength_to_assoc_list in
  let first_name, _ , first_strength = List.hd skills in
  inner_chooser skills (first_name,first_strength)



(**[filter_one_element_out_from_array arr pos] returns the array [arr] 
   without the element at index [pos]. 
   Requires: [0 <= pos < Array.length arr]*)
let filter_one_element_out_from_array (array: 'a array) (pos: int) = 
  let store = [|[]|] in
  for i = 0 to (Array.length array) - 1 do
    if i <> pos
    then store.(0) <- array.(i)::store.(0)
    else ()
  done;
  store.(0) |> List.rev |> Array.of_list


(**[get_map_index_by_name s name] returns the index of the map in game state
   [s] with the corresponding name [name]. *)
let get_map_index_by_name (s: state) (name: string) = 
  let rec search acc = function 
    | [] ->failwith "invalid map name"
    | (h: Maps.t) :: d -> if h.name = name then acc else
        search (acc+1) d in 
  search 0 s.all_maps



(**************************** Initiate the Game *******************************)



(**[helper_init ()] is a helper function for [init ()] that gets the 
   necessary information for the initial state. *)
let helper_init () = 
  let map_array, loc_array = main_engine_map_param () in
  (*this number can be either artificially set or stored in json.*)
  let map_size_array = main_map_size_array ~map_array in
  let map_col_row_array = main_map_col_row ~map_array in
  let final_number_array = 
    random_int_array_for_enemies_and_items map_size_array 15 in
  let all_enemies = 
    main_engine_enemy ~map_col_row_array ~loc_array final_number_array in
  let final_number_array_1 = 
    random_int_array_for_enemies_and_items map_size_array 10 in
  let all_weapons = 
    main_engine_weapon ~map_col_row_array ~loc_array final_number_array_1 in
  let final_number_array_2 = 
    random_int_array_for_enemies_and_items map_size_array 12 in
  let all_foods = 
    main_engine_food ~map_col_row_array ~loc_array final_number_array_2 in
  map_array, all_enemies, all_foods, all_weapons

(** [init ()] is the init state of the entire game. 
      Invariant: the first map of all maps must be main map !!! *)
let init () : state =
  let map_array, all_enemies, all_foods, all_weapons = 
    helper_init () in 
  {
    player = main_engine_player ();
    food_inventory = [|Eaten; Eaten; Eaten|];
    weapon_inventory = [|Empty; Empty; Empty|]; 
    current_map_in_all_maps = 0; (* this shouldn't be changed *)
    current_map = map_array.(0);
    player_old_loc = (0,0);
    branched_map_info = !branch_map_store;
    all_enemies_in_current_map = all_enemies.(0); 
    all_foods_in_current_map = all_foods.(0); 
    all_weapons_in_current_map = all_weapons.(0);
    all_maps = map_array |> Array.to_list;
    all_foods = all_foods;
    all_weapons = all_weapons;
    all_enemies = all_enemies;
  }



(**************************** Game Initial State ******************************)



(**[game_state] is the initial game state *)
let game_state = init ()



(******************************** Move Player *********************************)



(** [move_player_left s] change the current pos (col', row') of player 
    in state [s] to (col'-1, row') within the map boundaries.*)
let move_player_left s = 
  match s.player with
  | Died -> ()
  | Player t -> Player.move_left t s.current_map


(** [move_player_right s] change the current pos (col', row') of player 
    in state [s] to (col'+1, row') within the map boundaries.*)
let move_player_right s = 
  match s.player with
  | Died -> ()
  | Player t -> Player.move_right t s.current_map 


(** [move_player_up s] change the current pos (col', row') of player 
    in state [s] to (col', row'+1) within the map boundaries.*)
let move_player_up s = 
  match s.player with
  | Died -> ()
  | Player t -> Player.move_up t s.current_map

(** [move_player_down s] change the current pos (col', row') of player 
    in state [s] to (col', row'-1) within the map boundaries.*)
let move_player_down s = 
  match s.player with
  | Died -> ()
  | Player t -> Player.move_down t s.current_map



(****************************** Game Operations *******************************)



(**[change_player p s] changes the player in game state [s] to [p] *)
let change_player player s = s.player <- player


(**[check_enemy s store] checks whether there is an enemy at the player
   location at states [s], and if so, appends the enemy in [store] and 
   raises [SuccessExit]. Otherwise, no change is made. *)
let check_enemy (s: state) (store: string ref) =
  let loc = s |> get_player |> Player.location in
  for i = 0 to (Array.length s.all_enemies_in_current_map) - 1 do
    match s.all_enemies_in_current_map.(i) with
    | Enemy e when Enemy.get_pos e = loc ->
      store := Enemy.get_id e;
      raise SuccessExit
    | _ -> ()
  done


(**[check_enemy_in_current_loc s] is [(true, str)] if there exists an 
   enemy at the current location of the player at state [s], and 
   [(false, "")] otherwise. *)
let check_enemy_in_current_loc (s: state) =
  let store = ref "" in
  try
    check_enemy s store;
    false, ""
  with SuccessExit ->
    true, !store


(**[find_one_map_by_name s name] is the map with its name as [name] from
   all maps in [s]
   Requires: map with name [map_name] must be inside [lst] *)
let find_one_map_by_name (s: state) (map_name: string) =
  List.find (fun (map: current_map) -> map.name = map_name) s.all_maps
  

(**[reduce_player_health s hp] reduces the health of player at state [s] by 
   [hp]. If calling this function makes the player's health 0 or negative,
   then the player dies. *)
let reduce_player_health (s: state) (hp: int) = 
  match s.player with
  | Player t -> 
    if Player.health t <= hp
    then s.player <- Died
    else Player.reduce_health t hp
  | _ -> ()

(**[strengthen_whole_map s] will strengthen every living enemy in all maps in
   state [s]*)
let strengthen_whole_map (s: state) =
  let strengthen_one_enemy_array (e_arr: enemy array) = 
    for i = 0 to Array.length e_arr - 1 do
      match e_arr.(i) with
      | Deleted -> ()
      | Enemy (e: Enemy.t) -> Enemy.strengthen e;
    done in
  Array.iter (fun arr -> strengthen_one_enemy_array arr;) s.all_enemies


(**[delete_one_enemy_from_state s] deletes the enemy with player's current
   location at state [s] *)
let delete_one_enemy_from_state (s: state) =
  let player = s |> get_player in
  let loc = player |> Player.location in
  for i = 0 to (Array.length s.all_enemies_in_current_map) - 1 do 
    match s.all_enemies_in_current_map.(i) with
    | Enemy t when Enemy.get_pos t = loc ->
      Player.increase_experience player (Enemy.get_experience t);
      Player.update_skill player (Enemy.get_gainable_skill t);
      s.all_enemies_in_current_map.(i) <- Deleted;
      strengthen_whole_map s;
    | _ -> ()
  done


(**[take_one_food s] takes food from the player's current location at state
   [s], if any, and adds it to the first empty slot in the player's food 
   inventory. 
   Raises: [SuccessExit] if the food from the player's location was 
   successfully eaten. *)
let take_one_food (s: state) (index: int ref) (safe: food_item ref) =
  let update_food_inventory f t =
    for j = 0 to (Array.length s.food_inventory) - 1 do
      match s.food_inventory.(j) with
      | Eaten -> s.food_inventory.(j) <- Food f; raise SuccessExit
      | _ -> ()
    done in
  let player = s |> get_player in
  let loc = player |> Player.location in
  for i = 0 to (Array.length s.all_foods_in_current_map) - 1 do
    match s.all_foods_in_current_map.(i) with
    | Food f when Food.get_loc f = loc ->
      s.all_foods_in_current_map.(i) <- Eaten;
      index := i; safe := (Food f);
      update_food_inventory f player
    | _ -> ()
  done


(**[take_one_food_in_current_location s] takes food on player's current 
   location at state [s],  if any, to the first empty slot in player's food 
   inventory. *)
let take_one_food_in_current_location (s: state) = 
  let index = ref 0 in
  let safe = ref (s.all_foods_in_current_map.(0)) in
  try
    take_one_food s index safe;
    s.all_foods_in_current_map.(!index) <- (!safe);
  with SuccessExit ->
    ()


(**[drop_one_food s pos] drops the food at index [pos] of the player's 
   food inventory to the player's current location at state [s]. 
   Raises: [SuccessExit] if the food at index [pos] is successfully dropped. *)
let drop_one_food s pos = 
  let search_food_array s f loc = 
    Food.set_loc f loc;
    let food = Food f in
    for j = 0 to (Array.length s.all_foods_in_current_map) - 1 do
      match s.all_foods_in_current_map.(j) with
      | Eaten -> 
        s.all_foods_in_current_map.(j) <- food;
        raise SuccessExit
      | _ -> ()
    done;
    s.all_foods_in_current_map <- Array.append 
        [| food |] s.all_foods_in_current_map; in
  match s.food_inventory.(pos) with
  | Food f -> 
    s.food_inventory.(pos) <- Eaten;
    search_food_array s f (s |> get_player |> Player.location);
  | Eaten -> ()


(**[drop_one_food_to_current_location s pos] drops the food at index [pos] 
   of the player's food inventory to the player's current location at state 
   [s]. *)
let drop_one_food_to_current_location (s: state) (pos: int) =
  try
    drop_one_food s pos
  with SuccessExit ->
    ()


(**[eat_one_food_in_inventory s pos] eats the food from the player's current
   location, if any, with the player increasing health and strength and the 
   food at index [pos] removed from the player's inventory at state [s].  *)
let eat_one_food_in_inventory s pos = 
  let eat_food food t = 
    let health = Food.get_health food
    and strength = Food.get_strength food in
    Player.increase_health t health;
    Player.increase_strength t strength;
    Player.update_skill t (Food.get_gainables food) in
  let player = s |> get_player in
  match s.food_inventory.(pos) with
  | Food f -> 
    eat_food f player;
    s.food_inventory.(pos) <- Eaten;
  | _ -> ()


(**[equip_one_weapon s] will check player's inventory and equip player with
   weapon of player's current location if possible. If the weapon inventory is
   already full, the weapon will not be equipped (the game state wouldn't 
   change).
   Raises: [SuccessExit] if the weapon is successfully equipped in the 
   player's inventory.  *)
let equip_one_weapon (s: state) (index: int ref) (safe: weapon_item ref) =
  let update_weapon_inventory w t =
    for j = 0 to (Array.length s.weapon_inventory) - 1 do
      match s.weapon_inventory.(j) with
      | Empty -> 
        s.weapon_inventory.(j) <- Weapon w;
        Player.increase_strength t (Weapon.get_strength w);
        Player.update_skill t (Weapon.get_gainables w);
        raise SuccessExit
      | _ -> ()
    done in
  let player = s |> get_player in
  let loc = player |> Player.location in
  for i = 0 to (Array.length s.all_weapons_in_current_map) - 1 do
    match s.all_weapons_in_current_map.(i) with
    | Weapon w when Weapon.get_loc w = loc ->
      index := i; safe := (Weapon w);
      s.all_weapons_in_current_map.(i) <- Empty;
      update_weapon_inventory w player;
    | _ -> ()
  done


(**[equip_weapon_in_current_loc s] will update the weapon inventory of 
   game state [s] if there is any empty slot and weapon in player's current 
   location will be equipped in that slot.  *)
let equip_weapon_in_current_loc s = 
  let index = ref 0 in
  let safe = ref (s.all_weapons_in_current_map.(0)) in
  try
    equip_one_weapon s index safe;
    s.all_weapons_in_current_map.(!index) <- !safe
  with SuccessExit ->
    ()


(**[drop_one_weapon s pos] drops the weapon at index [pos] of the player's 
   weapon inventory to the player's current location at state [s]. 
   Example: if the weapon inventory is [|Null; Null; dagger|]
   [drop_one_weapon s 2] drops [dagger], and the weapon inventory 
   becomes [|Null; Null; Empty|].
   Raises: [SuccessExit] if the weapon at index [pos] is successfully removed
   from the weapon inventory. *)
let drop_one_weapon s pos = 
  let search_weapon_array s w loc = 
    Weapon.set_loc w loc;
    let weapon = Weapon w in
    for j = 0 to (Array.length s.all_weapons_in_current_map) - 1 do
      match s.all_weapons_in_current_map.(j) with
      | Empty -> 
        s.all_weapons_in_current_map.(j) <- weapon;
        raise SuccessExit
      | _ -> ()
    done;
    s.all_weapons_in_current_map <- Array.append 
        [| weapon |] s.all_weapons_in_current_map; in
  match s.weapon_inventory.(pos) with
  | Weapon w -> 
    let player = s |> get_player in
    Player.reduce_strength player (Weapon.get_strength w);
    s.weapon_inventory.(pos) <- Empty;
    search_weapon_array s w (player |> Player.location);
  | _ -> ()


(**[drop_one_weapon_to_current_location s pos] drops the weapon at index 
   [pos] of the player's weapon inventory to the player's current location 
   at state [s]. 
   Example: if the weapon inventory is [|Null; Null; dagger|]
   [drop_one_weapon_to_current_location s 2] drops [dagger], and the 
   weapon inventory becomes [|Null; Null; Empty|]. *)
let drop_one_weapon_to_current_location s pos =
  try
    drop_one_weapon s pos
  with SuccessExit ->
    ()


(**[check_food_on_loc_and_return_name_list s loc] returns a list of food names
   (possibly empty) are at the location [loc] 
   in the current map in state [s] *)
let check_food_on_loc_and_return_name_list s loc =
  let store = [|[]|] in
  for i = 0 to (Array.length s.all_foods_in_current_map) - 1 do
    match s.all_foods_in_current_map.(i) with
    | Food f when Food.get_loc f = loc ->
      store.(0) <- (Food.get_name f)::store.(0)
    | _ -> ()
  done;
  store.(0)


(**[check_weapon_on_loc_and_return_name_list s loc] returns a list of 
   weapon names (possibly empty) that are at the location [loc] 
   in the current map in state [s] *)
let check_weapon_on_loc_and_return_name_list s loc =
  let store = [|[]|] in
  for i = 0 to (Array.length s.all_weapons_in_current_map) - 1 do
    match s.all_weapons_in_current_map.(i) with
    | Weapon w when Weapon.get_loc w = loc ->
      store.(0) <- (Weapon.get_name w)::store.(0)
    | _ -> ()
  done;
  store.(0)


(**[check_item_on_player_ground s] is a tuple of 
   (food_name list,  weapon_name list) at player's current position
   at state [s] 
   Requires: Player MUST BE Alive *)
let check_item_on_player_ground s =
  let loc = s |> get_player |> Player.location in
  (
    check_food_on_loc_and_return_name_list s loc,
    check_weapon_on_loc_and_return_name_list s loc
  )


(**[delete_map_pos s pos name] deletes the items of the item arrays 
   at index [pos] in state [s] with map name as [name]. *)
let delete_map_pos (s: state) (pos: int) (name: string) : unit = 
  s.all_enemies <- filter_one_element_out_from_array s.all_enemies pos;
  s.all_foods <- filter_one_element_out_from_array s.all_foods pos;
  s.all_weapons <- filter_one_element_out_from_array s.all_weapons pos;
  s.branched_map_info <- 
    List.filter (fun (_, map_name) -> name <> map_name) s.branched_map_info;
  s.all_maps <- List.filter (fun (map: current_map) -> map.name <> name) s.all_maps


(**[check_current_linked_map s] returns [(false, "")] if the current map in 
   state [s] is not ["main"] or if the player at state [s] is not found in the 
   branced map info. Otherwise, [(true, info)] is returned where [info] is 
   the information in the branched map *)
let check_current_linked_map s =
  if get_current_map_name s <> "main" then false, ""
  else 
    try
      let loc = s |> get_player |> Player.location in
      true, List.assoc loc s.branched_map_info
    with Not_found ->
      false, ""


(**[transfer_player_to_branch_map s] transfers the player at state [s] to the
   branch map. *)
let transfer_player_to_branch_map s = 
  let status, name =  check_current_linked_map s in
  if status = false then ()
  else 
    let map = find_one_map_by_name s name in
    let map_index = get_map_index_by_name s map.name in
    s.player_old_loc <- s |> get_player |> Player.location;
    s.current_map <- map;
    s.all_enemies_in_current_map <- s.all_enemies.(map_index);
    s.all_foods_in_current_map <- s.all_foods.(map_index);
    s.all_weapons_in_current_map <- s.all_weapons.(map_index);
    s.current_map_in_all_maps <- map_index;
    (* the init pos of player in branched map is (1,1) *)
    Player.switch_loc (get_player s) (1,1)


(**[check_branch_map_status s] returns whether the player at state [s] has 
   finished his current branched map. *)
let check_branch_map_status s = 
  s.current_map.name <> "main" &&
  Array.for_all (fun enemy -> enemy = Deleted) s.all_enemies_in_current_map


(**[transfer_player_to_main_map s] transfers the player at state [s] to the 
   main map and changes to the corresponding configurations in the map. *)
let transfer_player_to_main_map s =
  if check_branch_map_status s
  then 
    let map_name = s.current_map.name in
    let map_pos = get_map_index_by_name s map_name in
    s.current_map <- List.hd s.all_maps;
    Player.switch_loc (get_player s) s.player_old_loc;
    s.all_enemies_in_current_map <- s.all_enemies.(0);
    s.all_foods_in_current_map <- s.all_foods.(0);
    s.all_weapons_in_current_map <- s.all_weapons.(0);
    s.current_map_in_all_maps <- 0;
    delete_map_pos s map_pos map_name
  else
    ()


(**[list_of_entrance_loc_to_branch_map s] is the list of entrance locations
   to the branch map named [s]. *)
let list_of_entrance_loc_to_branch_map s =
  if get_current_map_name s <> "main"
  then []
  else
    s.branched_map_info |> List.split |> fst



(****************************** Win Condition *********************************)



(**[check_wins s] returns whether the player at state [s] is in the 
   winning state.  *)
let check_wins s =  
  Array.for_all (fun map -> Array.for_all (fun e -> e = Deleted) map)
    s.all_enemies
