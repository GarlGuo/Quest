opam install dune

dune build src/game_models/maps.ml
dune build src/game_models/player.ml
dune build src/game_models/weapons.ml
dune build src/game_models/enemy.ml
dune build src/game_models/foods.ml
dune build src/engine/builder.ml
dune build src/engine/engine.ml
dune build src/user_interface/map_builder.ml
dune build src/user_interface/color_convert.ml
dune build src/user_interface/gui.ml
