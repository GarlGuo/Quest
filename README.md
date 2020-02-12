# Quest
@author: Ruiqi Song, Wentao Guo, Michael Zhou

************************************************************************************************************************
IMPORTANT:
Currently, I am modifying the dependencies between each the backend and frontend.

************************************   MakeFile will NOT work at this moment. ******************************************

However, on each file in src, I have commented out first several line but actually,
there are preprocessor commands for toplevel (like OCaml utop). 
To initiate the game, you can open the 
```
src/user_interface/gui.ml
```
uncomment the preprocessor commands and enter 
```
#use "gui.ml"
``` 
in utop.

************************************************************************************************************************






This is an adventure game mainly implemented in OCaml (for game engine and backend models) and Python (for converting images to JSON files)

************************************************************************************************************************
***Please ignore these instructions on MakeFile currently.***
MakeFile:

'make docs' will generate all documentations for OCaml's implementation in html form.

'make build' will build the binary files for OCaml codes.

'make play' will launch the game.

************************************************************************************************************************



During the game, a player can move throughout the map by pressing 'wasd' keys. The Player could also pick up or drop food and weapons in their current locations. The Player could gain skills by equipping weapons, eating food, and defeating enemies. Each skill has a cooling time and the player could use it when it is available. The wining condition is that the player defeats all enemies in all maps. 

The storm-like icon in map represents a branched map. Once all enemies in one branched map have been defeated, the player will return to the main map and that branched map will be removed.

For more detailed info of this game, please refer to the 'User manual.docx'

Enjoy your adventure in this game!
