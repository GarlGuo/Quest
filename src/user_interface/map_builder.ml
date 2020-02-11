(* #require "graphics";;
#require "yojson";;
#cd "../json_models";;
#mod_use "../user_interface/color_convert.ml";;
#mod_use "../game_models/maps.ml";;
#mod_use "../game_models/player.ml";;
#mod_use "../game_models/enemy.ml";;
#mod_use "../game_models/foods.ml";;
#mod_use "../game_models/weapons.ml";;
#mod_use "../engine/builder.ml";;
#mod_use "../engine/engine.ml";; *)

open Graphics
open Yojson.Basic.Util
open Color_convert
open Maps
open Engine
open Builder

(** [purple_red] is color purple*)
let purple_red = Graphics.rgb 140 67 86

(** [field] is the field for map with image "field", it is going to be 
    filled with Graphics.image*)
let field = ref None

(** [beach] is the field for map with image "sand", it is going to be 
    filled with Graphics.image*)
let beach = ref None

(** [mountain] is the field for map with image "mountain", it is going to be 
    filled with Graphics.image*)
let mountain = ref None

(** [lake] is the field for map with image "lake", it is going to be 
    filled with Graphics.image*)
let lake = ref None

(** [snow] is the field for map with image "snow", it is going to be 
    filled with Graphics.image*)
let snow = ref None

(** [grass] is the field for map with image "grass", it is going to be 
    filled with Graphics.image*)
let grass = ref None

(** [portal] is the field for portal, it is going to be 
    filled with Graphics.image*)
let portal = ref None

(** [text_init()] loads the graphics data for map
    Require: graph must be opened*)
let text_init()=
  field := Some (Graphics.make_image Color_convert.dirt_120);
  beach := Some (Graphics.make_image Color_convert.sand_120);
  mountain := Some (Graphics.make_image Color_convert.stone_120);
  lake := Some (Graphics.make_image Color_convert.water_120);
  snow := Some(Graphics.make_image Color_convert.snow_120);
  grass := Some (Graphics.make_image Color_convert.grass_120);
  portal := Some(Graphics.make_image Color_convert.portal)

(**[map_size_cal t] scales the size of the map model object [t] 
   to the size of the GUI screen and returns the dimensions of the 
   scaled map w.r.t the GUI. 
*)
let map_size_cal (t:Maps.t) = 
  let (col,row) = t.size in 
  match col,row with
  |5,5 -> ((300,200),120)
  |3,3 -> ((375,200),150)
  |5,10 -> ((0,200),120)
  |5,7 -> ((180,200),120)
  |_ -> failwith("wrong size of map"^string_of_int col^string_of_int row)

(**[picture_getter s] returns the image RGB matrix given by the 
   picture name [s]. 
   Fails with ["wrong name of picture, broken json or code"] if [s] is not 
   a valid picture name. *)
let picture_getter s size = 
  match s,size with
  |"field",size -> Option.get !field
  |"beach",size -> Option.get !beach
  |"mountain",size -> Option.get !mountain
  |"lake",size -> Option.get !lake
  |"snow",size -> Option.get !snow
  |"grass",size -> Option.get !grass
  |_ -> failwith "wrong name of picture, broken json or code"

(**[map_text_build ()] draws the map on the GUI based on the map params 
   and returns [()] *)
let map_text_build () = 
  let t = Builder.get_map Engine.game_state in
  let ((rr,cr),interval) = map_size_cal t in 
  let photo_data =  t.map_params in
  let rec draw_pic (data:((int * int) * Maps.MapParam.map_param) list) 
      rs cs inter = 
    match data with 
    |((r,c),p)::t -> (let pic = picture_getter p.name p in 
                      Graphics.draw_image pic (rs + (c - 1)*inter) 
                        (cs + (r - 1)*inter);
                      draw_pic t rs cs inter )
    |[] -> () in 
  draw_pic photo_data rr cr interval

(**[draw_player ()] draws the player on the GUI and returns [()]. *)
let draw_player () : unit  = 
  let t = Builder.get_map Engine.game_state in
  let ((rr,cr),interval) = map_size_cal t in 
  let player = Builder.get_player Engine.game_state in 
  let (row,col) = Player.Player.location player in 
  Graphics.set_color Graphics.red;
  Graphics.fill_circle (rr + (col - 1)*interval + interval/2) 
    (cr + (row - 1)*interval + (interval/2)) (interval/4)

(** [draw_weapon rr cr interval w] draws everything in [w] with
    [rr,cr] as the lower-left point and each points have a interval of 
    [interval]
    Requires: [rr],[cr],[interval] are positive int or 0
    [w] is a vaild weapon list*)
let draw_weapon rr cr interval w = 
  match w with 
  |Builder.Weapon w -> 
    let (row,col) = Weapons.Weapon.get_loc w in
    Graphics.set_color Graphics.yellow;
    Graphics.fill_circle (rr + (col - 1)*interval + interval/2) 
      (cr + (row - 1)*interval + (interval/2)) (interval/8);
    Graphics.set_color Graphics.black;
    let name = "Weapon" in
    let pixel = (String.length name) - 1 in
    Graphics.moveto ((rr + (col - 1)*interval + interval/2) - pixel*3)
      ((cr + (row - 1)*interval + (interval/2)) - row/2);
    Graphics.draw_string name
  |Builder.Empty -> ()

(** [draw_food rr cr interval f] draws everything in [f] with
    [rr,cr] as the lower-left point and each points have a interval of 
    [interval]
    Requires: [rr],[cr],[interval] are positive int or 0
    [f] is a vaild food list*)
let draw_food rr cr interval = function
  |Builder.Food f -> 
    let (row,col) =  Foods.Food.get_loc f in
    Graphics.set_color Graphics.green;
    Graphics.fill_circle (rr + (col - 1)*interval + interval/2) 
      (cr + (row - 1)*interval + (interval/2)) (interval/8);
    Graphics.set_color Graphics.black;
    let name = "Food" in
    let pixel = (String.length name) - 1 in
    Graphics.moveto ((rr + (col - 1)*interval + interval/2) - pixel*3)
      ((cr + (row - 1)*interval + (interval/2)) - row/2);
    Graphics.draw_string name
  |Builder.Eaten -> ()

(** [draw_food rr cr interval e] draws everything in [e] with
    [rr,cr] as the lower-left point and each points have a interval of 
    [interval]
    Requires: [rr],[cr],[interval] are positive int or 0
    [e] is a vaild enemy list*)
let draw_enemy rr cr interval = function
  |Builder.Enemy e -> (let (row,col) = Enemy.Enemy.get_pos e in 
                      Graphics.set_color purple_red;
                      Graphics.fill_circle 
                        (rr + (col - 1)*interval + interval/2) 
                        (cr + (row - 1)*interval + (interval/2)) (interval/4);
                      Graphics.set_color Graphics.black;
                      let name = Enemy.Enemy.get_name e in
                      let pixel = (String.length name) - 1 in
                      Graphics.moveto 
                        ((rr + (col - 1)*interval + interval/2) - pixel*3)
                        ((cr + (row - 1)*interval +(interval/2)) - row/2);
                      Graphics.draw_string name )
  |Builder.Deleted -> ()

(** [draw_items()] draws every items, including foods, weapons, enemies,
    and portals, on the map*)
let draw_items () = 
  let map_t = Builder.get_map Engine.game_state in
  let ((rr,cr),interval) = map_size_cal map_t in 
  let state = Engine.game_state in 
  let weapons = Array.to_list state.all_weapons_in_current_map in 
  let foods = Array.to_list state.all_foods_in_current_map in
  let enemy = Array.to_list state.all_enemies_in_current_map in
  let portal_lst = 
    Engine.list_of_entrance_loc_to_branch_map Engine.game_state in
  let rec draw_portal lst = 
    match lst with 
    |(row,col)::t -> Graphics.draw_image (Option.get !portal) 
                       (rr + (col - 1)*interval + interval/2 - 22) 
                       (cr + (row - 1)*interval + (interval/2) - 22);
      draw_portal t 
    |[] -> ()in 
  List.iter (draw_enemy rr cr interval) enemy;
  List.iter (draw_weapon rr cr interval) weapons;
  List.iter (draw_food rr cr interval) foods;
  draw_portal portal_lst

(** [draw()] draws items, player and maps*)
let draw () = 
  map_text_build();
  draw_items();
  draw_player()




