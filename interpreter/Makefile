run: _build/bin/main.exe
	dune exec bin/main.exe

conflicts: _build/bin/main.exe
	-menhir --explain _build/default/lib/parser.mly
	cat _build/default/lib/parser.conflicts

_build/bin/main.exe:
	dune build bin/main.exe


