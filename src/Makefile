all : repl.out

##############################
#
# Compile the project
#

# "ocamlbuild repl.native" will also build the REPL

repl : parser.cmo scanner.cmo repl.cmo
	ocamlc -w +a -o repl $^

%.cmo : %.ml
	ocamlc -w +a -c $<

%.cmi : %.mli
	ocamlc -w +a -c $<

scanner.ml : scanner.mll
	ocamllex $^

parser.ml parser.mli : parser.mly
	ocamlyacc $^

repl.out : repl repl.tb
	./repl < repl.tb > repl.out

# Depedencies from ocamldep
repl.cmo : scanner.cmo parser.cmi ast.cmi
repl.cmx : scanner.cmx parser.cmx ast.cmi
parser.cmo : ast.cmi parser.cmi
parser.cmx : ast.cmi parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx


##############################

TARFILES = README Makefile \
	scanner.mll ast.mli parser.mly repl.ml repl.tb

pythonplusplus.tar.gz : $(TARFILES)
	cd .. && tar zcf project/pythonplusplus.tar.gz $(TARFILES:%=hw1/%)

.PHONY : clean
clean :
	rm -rf \
	*.cmi *.cmo parser.ml parser.mli parser.output scanner.ml \
        repl.out repl __pycache__

test :
	python3 -m unittest run_tests
