# "make all" builds the executable

.PHONY : all
all : boomslang.native

# "make boomslang.native" compiles the compiler

boomslang.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind -pkgs str boomslang.native

repl : parser.cmo scanner.cmo ast.cmo sast.cmo semant.cmo repl.cmo
	ocamlc str.cma -w +a-4 -o repl $^

%.cmo : %.ml
	ocamlc -w +a-4 -c $<

%.cmi : %.mli
	ocamlc -w +a-4 -c $<

%.cmi : %.ml
	ocamlc -w +a-4 -c $<

scanner.ml : scanner.mll
	ocamllex $^

parser.ml parser.mli : parser.mly
	ocamlyacc $^

# Depedencies from ocamldep
repl.cmo : scanner.cmo parser.cmi ast.cmi sast.cmi semant.cmi
repl.cmx : scanner.cmx parser.cmx ast.cmi sast.cmi semant.cmi
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
        repl.out repl *.out __pycache__ _build boomslang.native \
	*.ll *.exe *.s *.o testall.log

TARFILES = README.md Makefile \
	scanner.mll parser.mly ast.ml sast.ml semant.ml \
	codegen.ml boomslang.ml repl.ml repl.tb run_tests.py \
        _tags boom

zip :
	zip boomslang $(TARFILES)

test :
	../test/testall.sh

pythontest :
	python3 -m unittest run_tests
