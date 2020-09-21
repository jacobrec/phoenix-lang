BIN=_build/default
NATIVE_TARGET=bin/main.exe
BYTE_TARGET=bin/main.bc



run: $(BIN)/$(BYTE_TARGET)
	dune exec $(BYTE_TARGET)

run_native: $(BIN)/$(NATIVE_TARGET)
	dune exec $(NATIVE_TARGET)

conflicts: $(BIN)/$(BYTE_TARGET)
	-menhir --explain _build/default/lib/parser.mly
	cat _build/default/lib/parser.conflicts

$(BIN)/$(NATIVE_TARGET):
	dune build $(NATIVE_TARGET)

$(BIN)/$(BYTE_TARGET):
	dune build -g $(BYTE_TARGET)


