# Quest
@author: 

Ruiqi Song @email: rs927@cornell.edu  

Wentao Guo @email: wg247@cornell.edu

Michael Zhou @email: mgz27@cornell.edu

************************************************************************************************************************
MakeFile:

```make checkenv```  will check all dependencies and the running environment.

```make play```  will launch the game.

```make engineTest``` will test the backend engine.

```make clean``` will clear all generated bytecodes and logs/digests.

************************************************************************************************************************

Load single file to OCaml TopLevel:

First, open the directory in which the file exists.

Then, open OCaml TopLevel (like utop) and enter ```#use "some file.ml"```.

For example, to load ```src/engine/engine.ml``` to the TopLevel. 
You need to open ```src/engine``` and enter ```#use "engine.ml"``` in OCaml TopLevel.


************************************************************************************************************************
This is an adventure game mainly implemented in OCaml (for backend models, game engine and GUI) and Python (for converting images to JSON files). The Python Codes are ***NOT*** uploaded into this repository. 
However, all generated JSON files (which each represents a picture) are contained in ```src/json_models```

During the game, a player can move throughout the map by pressing '***w*** ***a*** ***s*** ***d***' keys. The Player could also pick up or drop food and weapons in their current locations. The Player could gain skills by equipping weapons, eating food, and defeating enemies. Each skill has a cooling time and the player could use it when it is available. The wining condition is that the player defeats all enemies in all maps. 

The storm-like icon in map represents a branched map. Once all enemies in one branched map have been defeated, the player will return to the main map and that branched map will be removed.

For more detailed info of this game instructions, please refer to the ***player manual.pdf***

There are some game screenshots in the ***'game screenshot'*** folder.

Enjoy your adventure in this game!
