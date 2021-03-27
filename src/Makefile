# "make all" builds the executable

.PHONY : all
all : boomslang.native

# "make boomslang.native" compiles the compiler

boomslang.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind boomslang.native

repl : parser.cmo scanner.cmo ast.cmo repl.cmo
	ocamlc -w +a -o repl $^

%.cmo : %.ml
	ocamlc -w +a -c $<

%.cmi : %.mli
	ocamlc -w +a -c $<

%.cmi : %.ml
	ocamlc -w +a -c $<

scanner.ml : scanner.mll
	ocamllex $^

parser.ml parser.mli : parser.mly
	ocamlyacc $^

# Depedencies from ocamldep
repl.cmo : scanner.cmo parser.cmi ast.cmi
repl.cmx : scanner.cmx parser.cmx ast.cmi
parser.cmo : ast.cmi parser.cmi
parser.cmx : ast.cmi parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf ocamlllvm *.diff
	rm -rf \
	*.cmi *.cmo parser.ml parser.mli parser.output scanner.ml \
        repl.out repl a.out __pycache__ _build boomslang.native \
	*.ll *.exe *.s

TARFILES = README.md Makefile \
	scanner.mll parser.mly ast.ml sast.ml semant.ml \
	codegen.ml boomslang.ml repl.ml repl.tb run_tests.py \
        _tags boom

zip :
	zip boomslang $(TARFILES)

test :
	python3 -m unittest run_tests
