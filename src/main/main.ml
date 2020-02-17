#require "yojson";;
#require "graphics";;
#require "threads.posix";;
#cd "src/json_models";;
#mod_use "../game_models/maps.ml";;
#mod_use "../game_models/player.ml";;
#mod_use "../game_models/enemy.ml";;
#mod_use "../game_models/foods.ml";;
#mod_use "../game_models/weapons.ml";;
#mod_use "../engine/builder.ml";;
#mod_use "../engine/engine.ml";;
#mod_use "../user_interface/color_convert.ml";;
#mod_use "../user_interface/map_builder.ml";;
#mod_use "../user_interface/gui.ml"

open Engine
open Gui


let backend_init_state = Engine.game_state

let run = Gui.init

(** start the game *)
let () = run ()

