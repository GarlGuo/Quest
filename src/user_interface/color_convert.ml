(* #require "yojson";;
#require "graphics";;
#cd "../json_models" *)


open Graphics
open Yojson.Basic.Util

(** The abstract type of values representing an image with image json data*)
type oimage = {
  name: string;
  matrix: int list list list;
}

(** The abstract type of values representing multiple images *)
type images = {
  one_image: oimage list;
}

(** The abstract type of values representing an image with RGB data*)
type eimage = {
  name_data:string;
  image_data: Graphics.color array array;
}

(**[list_color l acc i] recursively converts each element of [l] into 
   an rgb matrix starting with an accumulator [acc] and index [i]. *)
let rec list_color (lst:int list list list) acc int2 = 
  match lst with 
  |h::t -> 
    let row_size = List.length h in
    let init = (match h with
        |[r;g;b]::t -> Array.make row_size (Graphics.rgb r g b)
        |h::t -> failwith"not a three items list"
        |[] -> failwith"should not reach the end")in
    let rec row_color (h:int list list) arr flag int: color array = 
      match h with
      |[r;g;b]::t -> if flag then 
          let _ = (arr.(int) <- (Graphics.rgb r g b)) in
          (row_color t arr true (int+1)) else 
          row_color t arr true (int+1)
      |h::t -> failwith"broken code"
      |[] -> arr in
    acc.(int2) <- (row_color h init false 0);
    (list_color t acc (int2+1))
  |[] -> acc

(**[array_color l] converts the image json data [l] into an rgb matrix [m], 
   and returns [m].  *)
let array_color lst = 
  let color_size = List.length lst in
  let array = Array.make color_size [|16777215;|] in
  list_color lst array 0

(**[string_tolll m] is [m] in a 3d list representation.  *)
let string_tolll matrix = 
  matrix |> to_list |> List.map to_list
  |> List.map (List.map to_list) |> List.map (List.map (List.map to_int))

(**[image_of_images i] is the image that [i] represents
   Requires: [i] is a valid JSON image representation. *)
let image_of_images i = {
  name = i |> member"name" |> to_string;
  matrix = i |> member"matrix" |> string_tolll;
}

(**[images_of_json j] is the image list that [j] represents
   Requires: [j] is a valid JSON image list representation. *)
let images_of_json json = 
  json |> member"images" |> to_list |> List.map image_of_images

(** [image_convert json name] is the color matrix in file [json]
    with [name]
    Requires: [json] is a vaild json file
    [name] is a vaild name in [json]*)
let image_convert json name = 
  let matrix = images_of_json (Yojson.Basic.from_file json) in 
  (array_color (List.find (fun x -> x.name = name) matrix).matrix)

(**[enemy_punch] is the RGB color 2d matrix represented by the image 
   ["stab"] *)
let enemy_punch = image_convert "punch_enemy.json" "stab"

(**[enemy_scar] is the RGB color 2d matrix represented by the image 
   ["scar-200"] *)
let enemy_scar = image_convert "scar.json" "scar-200"

(**[cute_cat] is the RGB color 2d matrix represented by the image 
   ["cute cat"] *)
let cute_cat = image_convert"cute cat.json" "cute cat"

(**[the_player] is the RGB color 2d matrix represented by the image 
   ["player"] *)
let the_player = image_convert "player_image.json" "player"

(**[the_minion] is the RGB color 2d matrix represented by the image 
   ["minion"] *)
let the_minion = image_convert "minion-image.json" "minion"

(**[player_in_combat] is the RGB color 2d matrix represented by the image 
   ["player_combat"] *)
let player_in_combat = image_convert"player_combat.json" "player_combat"

(**[the_stab] is the RGB color 2d matrix represented by the image 
   ["stab"] *)
let the_stab = image_convert"stab.json" "stab"

(**[sand_120] is the RGB color 2d matrix represented by the image 
   ["sand-120"] *)
let sand_120 = 
  image_convert "sand-120.json" "sand-120"

(**[grass_120] is the RGB color 2d matrix represented by the image 
   ["grass-120"] *)
let grass_120 = image_convert "grass-120.json" "grass-120"

(**[stone_120] is the RGB color 2d matrix represented by the image 
   ["stone-120"] *)
let stone_120 = image_convert "stone-120.json" "stone-120"

(**[snow_120] is the RGB color 2d matrix represented by the image 
   ["snow-120"] *)
let snow_120 = image_convert "snow-120.json" "snow-120"

(**[water_120] is the RGB color 2d matrix represented by the image 
   ["water-120"] *)
let water_120 = image_convert "water-120.json" "water-120"

(**[dirt_120] is the RGB color 2d matrix represented by the image 
   ["dirt-120"] *)
let dirt_120 = image_convert "dirt-120.json" "dirt-120"

(**[bread_80] is the RGB color 2d matrix represented by the image 
   ["bread_80"] *)
let bread_80 = image_convert "bread-80.json" "bread-80"

(**[coffee_80] is the RGB color 2d matrix represented by the image 
   ["coffee_80"] *)
let coffee_80 = image_convert "coffee-80.json" "coffee-80"

(**[coffee_80] is the RGB color 2d matrix represented by the image 
   ["coffee-80"] *)
let sword_80 = image_convert "sword-80.json" "sword-80"

(**[dagger_80] is the RGB color 2d matrix represented by the image 
   ["dagger_80"] *)
let dagger_80 = image_convert "dagger-80.json" "dagger-80"

(**[the_goblin] is the RGB color 2d matrix represented by the image 
   ["goblin"] *)
let the_goblin = image_convert "goblin-image.json" "goblin"

(**[the_witch] is the RGB color 2d matrix represented by the image 
   ["witch"] *)
let the_witch = image_convert "witch-image.json" "witch"

(**[fireball] is the RGB color 2d matrix represented by the image 
   ["fireball"] *)
let fireball = image_convert "fireball.json" "fireball"

(**[curse] is the RGB color 2d matrix represented by the image 
   ["curse"] *)
let curse = image_convert "curse.json" "curse"

(**[portal] is the RGB color 2d matrix represented by the image 
   ["portal"] *)
let portal = image_convert "portal.json" "portal"

(**[hit] is the RGB color 2d matrix represented by the image 
   ["hit"] *)
let hit = image_convert "hit.json" "hit"

(**[fire] is the RGB color 2d matrix represented by the image 
   ["fore"] *)
let fire = image_convert "fire.json" "fire"

(**[trial] is the RGB color 2d matrix represented by the image 
   ["trial"] *)
let trial = image_convert "trial.json" "trial"

(**[lighting] is the RGB color 2d matrix represented by the image 
   ["lighting"] *)
let lighting = image_convert "lighting.json" "lighting"

(**[logo] is the RGB color 2d matrix represented by the image 
   ["logo"] *)
let logo = image_convert "logo.json" "logo"

(**[enemy_data] is the list that contains a single [eimage] with the minion 
   image.  *)
let enemy_data = [{name_data = "minion";image_data = the_minion};
                  {name_data = "goblin";image_data = the_goblin};
                  {name_data = "witch";image_data = the_witch}
                 ]
