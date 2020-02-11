OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=oUnit,graphics,threads,yojson,threads.posix


MAPS_FILE = src/game_models/maps.ml
PLAYER_FILE = src/game_models/player.ml
WEAPONS_FILE = src/game_models/weapons.ml
ENEMY_FILE = src/game_models/enemy.ml
FOODS_FILE = src/game_models/foods.ml
BUILDER_FILE = src/engine/builder.ml
ENGINE_FILE = src/engine/engine.ml
MAP_BUILDER_FILE = src/user_interface/map_builder.ml
GUI_FILE = src/user_interface/gui.ml
COLOR_CONVERT_FILE = src/user_interface/color_convert.ml


build:
	bash build.sh

clean:
	ocamlbuild -clean

play:
	ocamlbuild -use-ocamlfind gui.byte && ./gui.byte

test: 
	ocamlbuild -use-ocamlfind -tag 'debug' test.byte && ./test.byte

docs: build
	mkdir -p doc
	ocamlfind ocamldoc -I _build -package -open -slash $(PKGS) \
		-html  -d doc \
		-m  A $(MAPS_FILE) $(PLAYER_FILE) $(FOODS_FILE) $(ENEMY_FILE) \
		 $(WEAPONS_FILE) $(BUILDER_FILE) $(ENGINE_FILE) $(COLOR_CONVERT_FILE) \
		 $(MAP_BUILDER_FILE) $(GUI_FILE)