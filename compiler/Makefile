run:
	make runtime
	./runtime

runtime: tmp.s main.c
	gcc -g -o $@ $^

tmp.s: test.phx main.phx
	/home/jacob/phoenix-lang/interpreter/_build/default/bin/main.exe main.phx


clean:
	rm runtime -f
	rm tmp.s -f

.PHONY: clean run
