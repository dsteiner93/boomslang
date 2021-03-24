# coms4115project
The final project for COMS 4115 - Programming Languages and Translators

Our language is called Python++ and is loosely based on Python.

Our group is made up of Nathan Cuevas (njc2150), Robert Kim (rk3145), Nikhil Min Kovelamudi (nmk2146), and David Steiner (ds3816).

For the Hello World milestone, we have the ability to generate programs in our language using our built in print function, println. For instance, a file can be created named foo.ppp with the contents println("Hello, world!").

To compile and run this file, the following steps must be executed.
* make
* ./pythonpp.native foo.ppp > foo.ll
* llc -relocation-model=pic foo.ll > foo.s
* cc -o foo.exe foo.s

Calling ./foo.exe will then print the string "Hello, world!" to the console.

The code for sast.ml, semant.ml, codegen.ml, pythonpp.ml, the Makefile, and E2E test suite are still a work in progress. They currently only work for simple println programs.

The code for the scanner, parser, and AST is much more complete and clean (they are essentially 100% done). These can be tested by running "make test" in the source directory. This will run our Python test suite that currently builds a REPL and tests full programs can generate valid ASTs.

We also have a pretty printer written for the AST using the graphviz dot language. Users can also interact with our language through the AST stage using our repl. Running "make repl" will create a repl program where users can type programs and get the graphviz AST representation.
