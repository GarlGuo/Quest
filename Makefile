play:
	utop src/main/main.ml

checkenv:
	bash checkenv.sh

clean:
	rm -rf doc
	rm -rf */_build
	rm -rf */_digest
	rm -rf */_log
	rm -rf *.byte
	rm -rf *.cmo
	rm -rf *.cmi
	rm -rf .DS_Store
	rm -rf *.cache
	rm -rf *.log

engineTest: 
	utop test/test.ml