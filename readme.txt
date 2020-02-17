@author: Ruiqi Song, Wentao Guo, Michael Zhou

Note: This program currently only work on Unix native enviroment with Ocaml Graphics library and Xqartz installed. 
The feature of opening window might not work on WSL. If graphics is not installed or not activated, you need to remove ocaml and then reinstall it, please follow these instructions to reinstall OCaml and all dependencies

1. opam switch remove 4.08.1
2. opam init --bare -a -y
3. opam switch create 4.08.1 ocaml-base-compiler.4.08.1
and you can check opam enviroment by type 'opam env' in terminal
4. opam install -y utop ounit yojson merlin graphics
