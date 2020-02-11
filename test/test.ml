open OUnit2
open Player
open Enemy
open Engine
open Foods
open Maps
open Weapons

(**Test Plan: 

   All the basic functionality of the model objects - player, enemy, branched 
   map,food, weapon - is developed via OUnit, while any testing involving the 
   GUI is done manually. 

   We first test the functionality of the player module, making sure that 
   each player state is updated correctly after applying these functions. 
   We first tested (via blackbox) that moving the player in all four directions
   updates the player to its correct position, and attempting to move the 
   player out of the map does not change the player's location. We then attempt 
   to advance the player to the next level before having enough experience,
   in which no update should be made to the player. We then increase the 
   player's experience so that the player would advance to the
   next level, and test that the player's level has incremented, and that his 
   experience and health has updated accordingly. We then change the 
   player's health by a certain amount to check whether the player's health
   has increased or decreased by the correct amount; in boundary cases, when 
   increasing a player's health to more than his maximum health, the player's 
   health is his maximum health; similarly when his health gets decreased to 
   0 or a negative value, his health essentially becomes zero. 
   Most of these test units are tested via black box testing, with some 
   glass box testing aspects associated to it for more complex 
   operations (i.e. advancing the player to next level changes his current 
   level, experience, and health). 
   
   We omitted the tests for testing player's skills changes during game.
   Player's skill contains a cd which limits the times player could use this
   skill. However, we don't want to expose cd to the engine (instead we write 
   other functions to automate the selctions and export of player's avaiable
   skills). In addition, player's gainable skills are collected in game
   which depends on the random location of weapons, foods, and enemies. 
   For example, player's skill strength will be incremented by defeating one
   enemy but it is difficult to "determine" which enemy is defeated in player's
   current location. Instead, we manually test skills in our GUI to ensure 
   the correctness.

   We then added the enemy test suite. We first tested whether reducing the
   enemy's hp level by a certain amount resulted in the correct hp level. We
   then ensure that the list skills of the enemy are nonempty (there must be
   skills that enemies possess). We also checked that the enemy level does 
   not change under any circumstances, as it is a static field. We mostly 
   used black box testing to test that these properties hold. 
   
   We omited the tests for system's automatic selection of enemy's skills
   by probability. We believed this part is better fitted in gui (when
   we played the real game) instead of hardcoding in unit tests.

   Next, we tested the food states via black box testing. 
   For the food objects, we tested that when the food gets moved to a different 
   location its strength and health value does not get changed. These are the 
   only properties tested for the food objects, since these are the only values 
   that matter for food model objects. All the other info for the food - name,
   description, gainables, id - do not change under any circumstance, yet we 
   did not need to test this since there are no functions that mutate these 
   properties. This demonstrates correctness since it covers all possible 
   updates to the food information. 

   We follow a similar approach for testing the weapon states as 
   stated in the above paragraph. We specifically
   tested that the weapon names and strength are consistent when the 
   weapon moves to a different location on the map, and that location of the
   weapon gets updated properly. All other info for the weapon - description, 
   id, gainables - do not change under any circumstance as no other code within
   the module references them other than the getter methods. This demonstrates
   correctness since it covers all possible updates to the weapon information. 

   For the branched map tests, we first made sure that switching to one of the
   branched maps changes to a nonzero index (index = 0 indicates the main map),
   and the name of the map is no longer "main". We then made sure that when 
   searching finding the set of weapons in the current map through 
   all sets of weapons, the index of the set of weapons is also non-zero 
   (weapon set at index 0 indicates the weapons in the main map). We made sure
   the same thing was true for finding the set of foods and enemies through 
   all sets of foods and enemies (we have used parallel arrays for these
   sets). We then made sure that switching back to the  main map produces 0 as the 
   indices mentioned above. 

  Finally, we tested the engine state. We took one food 
  and weapon randomly from the main map. Equipping would increment player's 
  skill strength and health (but the exact incrementation depends on
  which food or weapon we took). Since we had already tested the correctness of
  weapon and food, we only needed to test whether player's health, exp, and 
  strength really increased. After all, engine is responsible to update 
  in-game player's status and we wanted to check the correctness of such
  updates.
  
  Our tests are basially glass box. We tested backend models (player, foods,
  weapons, and enemies) by testing the correctness of our interface functions.
  Ex: increment player's health, equip one weapon. Each tests integrate some
  simple but representative game state. We then test the engine heavily in GUI.

  All implementation involving the GUI is tested manually. 
  Our game involves in switch of a lot of game states (branch map to map), and
  we tested our game by 'make play' on our gui. We believe unit tests ensured
  our basic functionalities in our backend should be correct (such as updating 
  player, strengthening single enemy), while gui testing ensured our backend 
  integration is correct. (such as switching one map, strengthening the whole
  map)
*)

(**[get_player s] returns [t] if the player at state [s] yields [Player t].
   Otherwise, fails with "died" *)
let get_player s = 
  match s.player with
  | Player t -> t
  | Died -> failwith "died"

(**[get_player_prop s f] is the property obtained by calling the function [f] 
   on the player at state [s]. 
   Requires: [f] is a defined function inside the [Player] module *)
let get_player_prop s f = s |> get_player |> f

(**[get_pos s] is the player's location at state [s]  *)
let get_pos s = get_player_prop s Player.location 

(**[get_health s] is the player's health at state [s]  *)
let get_health s = get_player_prop s Player.health

(**[get_max_health s] is the player's maximum health at state [s] *)
let get_max_health s = get_player_prop s Player.max_health

(**[get_experience s] is the player's experience at state [s]  *)
let get_experience s = get_player_prop s Player.experience

(**[get_level s] is the player's level at state [s]  *)
let get_level s = get_player_prop s Player.level

(**[map_cols s] is the # of cols of the player's map at state [s]  *)
let map_cols s = s.current_map.size |> fst

(**[map_rows s] is the # of rows of the player's map at state [s]  *)
let map_rows s = s.current_map.size |> snd

(**[delete_all_enemies_in_current_map s] removes all enemies in the 
   current map in state [s].   *)
let delete_all_enemies_in_current_map s = 
  for i = 0 to Array.length s.all_enemies_in_current_map - 1 do
    s.all_enemies_in_current_map.(i) <- Deleted
  done

(**[array_loc e arr] is the index of [e] in array [arr]

   Raises:
   Failure if [e] is not in [arr]*)
let array_loc element arr = 
  let store' = ref 0 in 
  let inner_searcher store = 
    for i = 0 to Array.length arr - 1 do
      if arr.(i) = element 
      then (store := i; raise SuccessExit )
      else ()
    done in
  try
    inner_searcher store';
    raise (Failure "not in this array")
  with SuccessExit ->
    !store'

(**[switch_to_different_map s pos] switches to the map indexed at 
   [pos]. *)
let switch_to_different_map s pos = 
  s.current_map <- List.nth s.all_maps pos;
  s.all_enemies_in_current_map <- s.all_enemies.(pos);
  s.all_foods_in_current_map <- s.all_foods.(pos);
  s.all_weapons_in_current_map <- s.all_weapons.(pos);
  s.current_map_in_all_maps <- pos


(** Player state update tests *)

(**[get_enemy t] returns the enemy [e] if [t] is a valid enemy type, 
   and raises [Failure "invalid enemy"] otherwise. *)
let get_enemy = function
  | Deleted -> failwith "invalid enemy"
  | Enemy e -> e

(**[get_food t] returns the food [f] if [t] is a valid food type, 
   and raises [Failure "invalid food"] otherwise. *)
let get_food = function
  | Eaten -> failwith "invalid food"
  | Food f -> f

(**[get_weapon t] returns the weapon [w] if [t] is a valid weapon type, 
   and raises [Failure "invalid weapon"] otherwise. *)
let get_weapon = function
  | Empty -> failwith "invalid weapon"
  | Weapon w -> w

(**[enemy_is_alive t] returns [true] if [t] is a valid enemy and [false]
   otherwise.  *)
let enemy_is_alive = function
  | Deleted -> false
  | Enemy e -> true

(**[get_one_enemy_pos s] returns the location of the first enemy 
   in the current map at state [s].  *)
let get_one_enemy_pos s = 
  s.all_enemies_in_current_map.(0) |> get_enemy |> Enemy.get_pos

(**[get_first_alive_enemy_at_index s pos] returns the first alive enemy of
   all the enemies indexed map [pos] at state [s]. 
   Raises: [Failure "all enemies are dead"] if all enemies are dead.  *)
let get_first_alive_enemy_at_index s pos =
  let store = ref s.all_enemies_in_current_map.(0) in
  let rec inner_searcher enemies_array = 
    for i = 0 to Array.length enemies_array - 1 do
      match enemies_array.(i) with
      | Enemy e -> store := Enemy e;
        raise SuccessExit
      | _ -> ()
    done in
  try
    inner_searcher s.all_enemies.(pos);
    failwith "all enemies are dead"
  with SuccessExit ->
    !store |> get_enemy

(**[get_first_available_food_at_index s pos] returns the first available food 
   of all the foods indexed map [pos] at state [s].
   Raises: [Failure "no foods"] if no food is available.  *)
let get_first_available_food_at_index s pos =
  let store = ref s.all_foods_in_current_map.(0) in
  let rec inner_searcher food_array = 
    for i = 0 to Array.length food_array - 1 do
      match food_array.(i) with
      | Food f -> 
        store := Food f;
        raise SuccessExit
      | _ -> ()
    done in
  try
    inner_searcher s.all_foods.(pos);
    failwith "no foods"
  with SuccessExit ->
    !store |> get_food

(**[get_first_available_weapon_at_index s pos] returns the first available 
   weapon of all the weapons indexed map [pos] at state [s]. 
   Raises: [Failure "no weapons"] if no food is available.  *)
let get_first_available_weapon_at_index s pos =
  let store = ref s.all_weapons_in_current_map.(0) in
  let rec inner_searcher weapon_array = 
    for i = 0 to Array.length weapon_array - 1 do
      match weapon_array.(i) with
      | Weapon w -> 
        store := Weapon w;
        raise SuccessExit
      | _ -> ()
    done in
  try
    inner_searcher s.all_weapons.(pos);
    failwith "no weapons"
  with SuccessExit ->
    !store |> get_weapon

(**[delete_one_enemy_from_map_index s e pos] delete the enemy object [e]
   from map indexed [pos] in game state [s]*)
let delete_one_enemy_from_map_index s enemy pos = 
  let deleter () =
    for i = 0 to Array.length s.all_enemies.(pos) - 1 do
      match s.all_enemies.(pos).(i) with
      | Enemy e when e = enemy -> 
        s.all_enemies.(pos).(i) <- Deleted;
        raise SuccessExit
      | _ -> ()
    done in
  try
    deleter ()
  with SuccessExit ->
    ()

(*                initiate testing param                    *)


(* initial pos -> 1,1 
   initial map -> 0 *)
let state = Engine.init () 
let init_health = get_health state
let init_experience = get_experience state
let init_level = get_level state
let init_pos = get_pos state

(* move right -> 2,1 *)
let _ = Engine.move_player_right state
let state1_loc = get_pos state

(* move left -> 1,1 *)
let _  = Engine.move_player_left state 
let state2_loc = get_pos state

(* move left -> 1,1 *)
let _  = Engine.move_player_left state 
let state3_loc = get_pos state

(* move down -> 1,1 *)
let _  = Engine.move_player_down state 
let state4_loc = get_pos state

(* move up -> 1,2 *)
let _  = Engine.move_player_up state
let state5_loc = get_pos state

(* move up -> 1,3 *)
let _  = Engine.move_player_up state 
let state6_loc = get_pos state

(* move down -> 1,2 *)
let _  = Engine.move_player_down state 
let state7_loc = get_pos state


(* increase experience -> init_experience+10 *)
let _ = Player.increase_experience (get_player state) 10
let state11_experience = get_experience state
let state11_level = get_level state


let _ = Player.advance_level (get_player state)
let state11'_experience = get_experience state
let state11'_level = get_level state
let state11'_health = get_health state (* 100 *)

(* increase experience enough to advance *)
let _ = Player.increase_experience (get_player state) 
    (state |> get_player |> Player.level_up_expereince)
let state12_experience = get_experience state 
let state12_level = get_level state 
let state12_health = get_health state (* 130 *)

(* move right towards rightmost column, upmost row *)
let _ = 
  for i = 1 to (map_cols state) + 15 do 
    Engine.move_player_up state;
    Engine.move_player_right state;
  done
let state15_loc = get_pos state

(* TODO tests: *)
(* get_skill_by_skill_name *)
(* skill_name *)

(* reduce health by 12 *)
let _ = Player.reduce_health (state |> get_player) 12 

let state_f3_health = get_health state


(* increase health to max *)
let check_health = get_max_health state
let _ = Player.increase_health (get_player state) check_health
let state_f2_health = get_health state (* 118 *)


(* reduce health -> 0, DIED *)
let _ = Player.reduce_health (get_player state) (get_max_health state)
let state_f_health = get_health state
let player_f = state.player


(** Enemy state update tests *)
let new_s = init ()

let one_e = get_first_alive_enemy_at_index new_s 0

let init_1_hp = Enemy.get_hp one_e


let init_1_level = Enemy.get_level one_e
let skill_list_1 = 
  Enemy.get_all_skills_name_prob_and_strength_to_assoc_list one_e <> []

let _ = Enemy.reduce_hp one_e 10

let init_2_hp = Enemy.get_hp one_e


let init_2_level = Enemy.get_level one_e
let skill_list_2 = 
  Enemy.get_all_skills_name_prob_and_strength_to_assoc_list one_e <> []

(** Food state update tests *)

let new_s1 = init ()

let one_f = get_first_available_food_at_index new_s1 0

let init_1f_strength = Food.get_strength one_f
let init_1f_health = Food.get_health one_f

let _ = Food.set_loc one_f (2,3)

let init_2f_strength = Food.get_strength one_f
let init_2f_health = Food.get_health one_f
let f_new_loc = Food.get_loc one_f


(** Weapon state update tests *)
let new_s2 = init ()

let one_w = get_first_available_weapon_at_index new_s2 0
let init_1w_name = Weapon.get_name one_w
let init_1w_strength = Weapon.get_strength one_w


let _ = Weapon.set_loc one_w (2,3)
let init_2w_name = Weapon.get_name one_w
let init_2w_strength = Weapon.get_strength one_w
let w_new_loc = Weapon.get_loc one_w


(** Engine update tests *)
let new_e = init ()
let new_e_fir_e = get_first_alive_enemy_at_index new_e 0
let new_e_fir_level = new_e_fir_e |> Enemy.get_level
let new_e_fir_health = new_e_fir_e |> Enemy.get_hp
let new_e_fir_pos = new_e_fir_e |> Enemy.get_pos

let _ = strengthen_whole_map new_e

let new_e_sec_e = get_first_alive_enemy_at_index new_e 0

let new_e_sec_level = new_e_sec_e |> Enemy.get_level
let new_e_sec_health = new_e_sec_e |> Enemy.get_hp
let new_e_sec_pos = new_e_sec_e |> Enemy.get_pos

let e_loc = Enemy.get_pos new_e_fir_e
let _ = Player.switch_loc (new_e |> get_player) e_loc
let _ = delete_one_enemy_from_state new_e

let new_exp = new_e |> get_player |> Player.experience

let branch_map_num = new_e |> list_of_entrance_loc_to_branch_map |> List.length

let w1 = get_first_available_weapon_at_index new_e 0
let w1_loc = Weapon.get_loc w1
let _ = Player.switch_loc (new_e |> get_player) w1_loc
let _ = equip_weapon_in_current_loc new_e
let equip_result = new_e.weapon_inventory.(0) <> Empty
let _ = drop_one_weapon_to_current_location new_e 0
let drop_w_result = new_e.weapon_inventory.(0) = Empty

let f1 = get_first_available_food_at_index new_e 0
let f1_loc = Food.get_loc f1
let _ = Player.switch_loc (new_e |> get_player) f1_loc
let _ = take_one_food_in_current_location new_e
let take_result = new_e.food_inventory.(0) <> Eaten
let _ = drop_one_food_to_current_location new_e 0
let drop_f_result = new_e.food_inventory.(0) = Eaten
let _ = take_one_food_in_current_location new_e

(* eat food *)
let _ = Player.reduce_health (new_e |> get_player) 20
let before_eating_food = new_e |> get_player |> Player.health
let _ = eat_one_food_in_inventory new_e 0
let eat_food_health = new_e |> get_player |> Player.health


(** Branched map tests *)
let branch_1_loc = new_e |> list_of_entrance_loc_to_branch_map |> List.hd
let _ = Player.switch_loc (get_player new_e) branch_1_loc

let new_e_branch_1 = Engine.transfer_player_to_branch_map new_e

let new_e_index = new_e.current_map_in_all_maps (* != 0 *)
let new_e_name = new_e.current_map.name 
let new_e_food = array_loc new_e.all_foods_in_current_map new_e.all_foods
let new_e_enemy = array_loc new_e.all_enemies_in_current_map 
    new_e.all_enemies
let new_e_weapon = array_loc new_e.all_weapons_in_current_map
    new_e.all_weapons

let _ = delete_all_enemies_in_current_map new_e
let _ = transfer_player_to_main_map new_e (* transform back to main map *)

let new_e_index_1 = new_e.current_map_in_all_maps (* != 0 *)
let new_e_name_1 = new_e.current_map.name 
let new_e_food_1 = array_loc new_e.all_foods_in_current_map new_e.all_foods
let new_e_enemy_1 = array_loc new_e.all_enemies_in_current_map 
    new_e.all_enemies
let new_e_weapon_1 = array_loc new_e.all_weapons_in_current_map
    new_e.all_weapons

let new_e_map_len = List.length new_e.all_maps

(**[make_test n i o] constructs a test [n] to check whether [i] is equal 
   to [o]. *)
let make_test n i o = 
  n >:: (fun _ ->  assert_equal i o)

(**[make_exc_test n f e] constructs a test [n] where applying function [f]
   must produce an exception [e]. *)
let make_exc_test n f e = 
  let func = fun () -> f in 
  n >:: (fun _ -> assert_raises e func)


(** Test suite for player states  *)
let player_state_tests = [
  make_test "init" init_pos (1,1);

  make_test "R" state1_loc (2,1);

  make_test "RL" state2_loc (1,1);

  make_test "RLL = RL" state3_loc (1,1);

  make_test "RLLD = RL" state4_loc (1,1);

  make_test "RLU" state5_loc (1,2);

  make_test "RLUU" state6_loc (1,3);

  make_test "RLUUD" state7_loc (1,2);

  make_test "advance level failed" state11_experience state11'_experience;

  make_test "advance level failed" state11_level state11'_level;

  make_test "increase exp" state11_experience 
    (init_experience + 10);

  make_test "increase exp to advance 1" state12_experience 
    (init_experience + 10);

  make_test "increase exp to advance 2" state12_level (init_level+1);

  make_test "upper right bound" state15_loc 
    (map_cols state, map_rows state);

  make_test "health before advanced level" state11'_health init_health;

  make_test "health after advanced level" state12_health (init_health + 30);

  make_test "reduce health by 12" state_f3_health 118;

  make_test "increase all health" state_f2_health check_health;

  make_test "reduce all health" state_f_health 0;
]

(** Test suite for enemy states  *)
let enemy_state_tests = [
  make_test "reduce enemy hp" (init_1_hp - init_2_hp) 10;

  make_test "always ensure the skills output of enemies are valid"
    (skill_list_2) true;

  make_test "always ensure the skills output of enemies are valid"
    (skill_list_1) true;

  make_test "enemy level is a static field and it shouldn't be changed"
    (init_1_level = init_2_level) true;
]

(** Test suite for food states  *)
let food_state_tests = [
  make_test "food's gainable strength should never be changed when we move food"
    (init_1f_strength = init_2f_strength) true;

  make_test "food's gainable health should never be changed when we move food"
    (init_1f_health = init_2f_health) true;

  make_test "change food's location" f_new_loc (2,3);
]

(** Test suite for weapon states  *)
let weapon_state_tests = [
  make_test "weapon's name should be consistent throughout moving"
    (init_1w_name = init_2w_name) true;

  make_test "weapon's strength should be consistent throughout moving"
    (init_1w_strength = init_2w_strength) true;

  make_test "weapon's new location should be correct after moving"
    w_new_loc (2, 3);
]

(** Test suite for branched map states  *)
let branched_map_tests = [
  (* branched map *)
  make_test "sucessfully transfer to branch map. Map index updated"
    (new_e_index <> 0) true;

  make_test "sucessfully transfer to branch map. Enemy index updated"
    (new_e_name <> "main") true;

  make_test "sucessfully transfer to branch map. Weapon index updated"
    (new_e_weapon <> 0) true;

  make_test "sucessfully transfer to branch map. 
    Weapon shared the same index as food"
    (new_e_food) new_e_weapon;

  make_test "sucessfully transfer to branch map.
    Weapon shared the same index as enemy"
    new_e_weapon new_e_enemy;

  (* main map *)
  make_test "sucessfully transfer to main map. Map index updated"
    new_e_index_1 0;

  make_test "sucessfully transfer to main map. Enemy index updated"
    new_e_name_1 "main";

  make_test "sucessfully transfer to main map. Weapon index updated"
    new_e_weapon_1 0;

  make_test "sucessfully transfer to main map. 
    Weapon shared the same index as food"
    new_e_food_1 new_e_weapon_1;

  make_test "sucessfully transfer to main map.
    Weapon shared the same index as enemy"
    new_e_weapon_1 new_e_enemy_1;

  make_test "sucessfully transfer to main map.
    Weapon shared the same index as enemy"
    new_e_map_len 3;
]

(** Test suites for engine states *)
let engine_state_tests = [
  make_test "successfully strengthen enemy's health after play"
    new_e_sec_health (new_e_fir_health+5);

  make_test "successfully strengthen enemy's level after play"
    new_e_sec_level (new_e_fir_level+1);

  make_test "enemy's pos shouldn't be changed after strengthening"
    new_e_fir_pos new_e_sec_pos;

  make_test "delete enemy would automatically increase the exp of player"
    (new_exp > init_experience) true; 

  make_test "the length of branch map is correct"
    branch_map_num 3;

  (* drop and take *)
  make_test "successfully equip one weapon"
    equip_result true;

  make_test "successfully drop one weapon"
    drop_w_result true;

  make_test "successfully take one food"
    take_result true;

  make_test "successfully drop one food"
    drop_f_result true; 

  make_test "successfully eat one food"
    (eat_food_health > before_eating_food) true; 
]


(** All the test suites  *)
let suite =
  "test suite for A2" >::: 
  List.flatten [
    player_state_tests;
    enemy_state_tests;
    food_state_tests;
    weapon_state_tests;
    branched_map_tests;
    engine_state_tests;
  ]

(** Run all the test suites  *)
let _ = run_test_tt_main suite