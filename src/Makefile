# "make all" builds the executable

.PHONY : all
all : pythonpp.native

# "make pythonpp.native" compiles the compiler

pythonpp.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind pythonpp.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf ocamlllvm *.diff
	rm -rf \
	*.cmi *.cmo parser.ml parser.mli parser.output scanner.ml \
        repl.out repl a.out __pycache__

