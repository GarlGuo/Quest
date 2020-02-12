**************************************************************************************************************************
IMPORTANT:
Currently, I am modifying the dependencies between each the backend and frontend.

************************************   MakeFile WILL NOT work at this moment. ********************************************

However, on each file in src, I have commented out first several line but actually,
there are preprocessor commands for toplevel (like OCaml utop). 
To initiate the game, you can open the src/user_interface/gui.ml, uncomment the preprocessor commands and enter 
#use "gui.ml" in utop.

**************************************************************************************************************************




How to install:
1.make build to compile 
2.make play to play

Note: This program currently only work on MacBook with Ocaml Graphics library and Xqartz installed. 
The feature of opening window doesn't work on Windows. If Graphics is not installed, you need to remove ocaml and then reinstall it, please type 
1. opam switch remove 4.08.1
2.opam init --bare -a -y
3.opam switch create 4.08.1 ocaml-base-compiler.4.08.1
eval $(opam env)
4.opam install -y utop ounit qtest yojson lwt lwt_ppx menhir ansiterminal lambda-term merlin ocp-indent user-setup bisect_ppx-ocamlbuild
opam user-setup install
5. Opam install -y graphics

Graphics library should be preinstalled with ocaml at the beginning of the semester. However, I saw a few people don't have it preinstalled. So, you always want to check by ocamlfind list to see whether the library was installed or not.
