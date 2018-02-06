TEST = test

all:
	ocamlbuild -use-menhir -r -tag thread -use-ocamlfind -pkg core main.native
	./main.native $(TEST).txt > result.txt
	cat result.txt

clean:
	rm -f main.native result.txt
	rm -rf _build
	clear