# Boomslang
Boomslang is a general-purpose imperative programming language inspired by Python that aims to create a language as easy to use as Python, but with the added type safety of Java. The language is strongly and statically typed, has no type inferencing, and supports object-oriented programming without requiring that all functions be part of objects.

Boomslang uses syntactically significant whitespace as opposed to curly braces and semicolons. In addition to adding static typing to a Python-style syntax, Boomslang aims to add quality of life features such as automatic constructor generation for data classes, a new syntax for loops, and a better operator overloading syntax.

The language was developed as the final project for COMS 4115 - Programming Languages and Translators, Spring 2021 semester at Columbia University. The original project team consists of Nathan Cuevas (njc2150), Robert Kim (rk3145), Nikhil Min Kovelamudi (nmk2146), and David Steiner (ds3816).

The name Boomslang was chosen because Boomslang is the name of a snake (evoking Python) and it ends in "lang", evoking the word language, as in "programming language."

To compile a program, use the boomc utility. "./boomc helloworld.boom" will compile the contents of helloworld.boom to LLVM IR. Using the flag "-r" with boomc as in "./boomc -r helloworld.boom" will compile the program and automatically execute it in one step.

To compile the compiler, run "make" inside the src directory. This will produce a "boomslang.native" program that can be used to generate graphviz representations of the abstract syntax tree or semantically checked abstract syntax tree, as well as LLVM IR.

To run the test suite, run "make test" inside the src directory.
