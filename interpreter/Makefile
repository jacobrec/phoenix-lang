BIN=_build/default
NATIVE_TARGET=bin/main.exe
BYTE_TARGET=bin/main.bc




run: $(BIN)/$(BYTE_TARGET)
	rlwrap dune exec $(BYTE_TARGET)

run_native: $(BIN)/$(NATIVE_TARGET)
	rlwrap dune exec $(NATIVE_TARGET)

tests: clean $(BIN)/$(BYTE_TARGET) $(BIN)/$(NATIVE_TARGET)
	cd ../ && ./run_tests.sh

conflicts: $(BIN)/$(BYTE_TARGET)
	-menhir --explain _build/default/lib/parser.mly
	cat _build/default/lib/parser.conflicts

clean:
	rm -rf _build

$(BIN)/$(NATIVE_TARGET):
	dune build $(NATIVE_TARGET)

$(BIN)/$(BYTE_TARGET):
	dune build $(BYTE_TARGET)


.PHONY: clean run run_native tests conflicts
