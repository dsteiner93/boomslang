# Boomslang
Boomslang is a general-purpose imperative programming language inspired by Python that aims to create a language as easy to use as Python, but with the added type safety of Java. The language is strongly and statically typed, has no type inferencing, and supports object-oriented programming without requiring that all functions be part of objects.

Boomslang uses syntactically significant whitespace as opposed to curly braces and semicolons. In addition to adding static typing to a Python-style syntax, Boomslang aims to add quality of life features such as automatic constructor generation for data classes, a new syntax for loops, and a better operator overloading syntax.

The language was developed as the final project for COMS 4115 - Programming Languages and Translators, Spring 2021 semester at Columbia University. The original project team consists of Nathan Cuevas (njc2150), Robert Kim (rk3145), Nikhil Min Kovelamudi (nmk2146), and David Steiner (ds3816).

The name Boomslang was chosen because Boomslang is the name of a snake (evoking Python) and it ends in "lang", evoking the word language, as in "programming language."

For the Hello World milestone, we have the ability to generate programs in our language using our built in print function, println. For instance, a file can be created named foo.boom with the contents println("Hello, world!").

To compile and run this file, the following steps must be executed.
* make clean
* make
* ./boomslang.native helloworld.boom > helloworld.ll
* llc -relocation-model=pic helloworld.ll > helloworld.s
* cc -o helloworld.exe helloworld.s

Calling ./helloworld.exe will then print the string "Hello, world!" to the console.

A convenience script inside file "boomc" has been provided to automatically execute the above steps. Calling "./boomc helloworld.boom" will generate the .exe in one step. Calling "./boomc -r helloworld.boom" will generate the .exe and automatically run it.

The code for sast.ml, semant.ml, codegen.ml, boomslang.ml, the Makefile, and E2E test suite are still a work in progress. They currently only work for simple println programs.

The code for the scanner, parser, and AST is much more complete and clean (they are essentially 100% done). These can be tested by running "make test" in the source directory. This will run our Python test suite that currently builds a REPL and tests full programs can generate valid ASTs.

We also have a pretty printer written for the AST using the graphviz dot language. Users can also interact with our language through the AST stage using our repl. Running "make repl" will create a repl program where users can type programs and get the graphviz AST representation.
