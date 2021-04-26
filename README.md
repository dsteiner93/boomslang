# Boomslang
Boomslang is a general-purpose imperative programming language inspired by Python and Java that aims to create a language as easy to use as Python, but with the added type safety of Java. The language is strongly and statically typed, has no type inferencing, and supports object-oriented programming without requiring that all functions be part of objects. Boomslang is written in OCaml and compiles to LLVM IR.

Boomslang uses syntactically significant whitespace as opposed to curly braces and semicolons. In addition to adding static typing to a Python-style syntax, Boomslang aims to add quality of life features such as built-in support for data classes (automatic constructor generation and meaningful automatic string conversion), a new syntax for loops, and a better operator overloading syntax.

Boomslang (or something similar) means "tree snake" in several Germanic languages. The name Boomslang was chosen because its snake roots pay homage to Python, while also ending in "lang", evoking the word language, as in "programming language".

The language was developed as the final project for COMS 4115 - Programming Languages and Translators, Spring 2021 semester at Columbia University. The original project team consists of Nathan Cuevas (njc2150), Robert Kim (rk3145), Nikhil Min Kovelamudi (nmk2146), and David Steiner (ds3816).

# Compiling programs
To compile a program, use the boomc utility. "./boomc helloworld.boom" will compile the contents of helloworld.boom to LLVM IR. Using the flag "-r" with boomc as in "./boomc -r helloworld.boom" will compile the program and automatically execute it in one step.

To compile the compiler, run "make" inside the src directory. This will produce a "boomslang.native" program that can be used to generate graphviz representations of the abstract syntax tree or semantically checked abstract syntax tree, as well as LLVM IR.

To run the test suite, run "make test" inside the src directory.

The Boomslang compiler requires OCaml and LLVM to be installed on the compiling machine. The following Docker image can also be used: https://hub.docker.com/r/columbiasedwards/plt

# Writing programs
Boomslang has the following types built in: int, long, float, char, string, and boolean. Users may define their own types (called classes), as well as make arrays of any type. In addition to the previously mentioned types, functions may return a special "void" type, indicating there is no return value, and objects may be assigned NULL. (No type other than objects may be NULL.)

Here are some examples of how to use the types in Boomslang:
```
int a = 5
long b = 100L
float c = 2.2
char d = 'd'
string str = "foo"
boolean is_boomslang_a_good_language = true
MyObject my_object = NULL
int[] arr = [1, 2, 3, 4, 5]
```

By convention, function names and variable names should be snake_case rather than camelCase. By necessity, class names must start with an uppercase letter and only contain letters (no numbers). **Also by necessity, all indentation must be done using tabs. Spaces will not work.**

-Arrays
-Binops
-println

-Ifs
-Loops
-Functions
-Classes
-Generic classes
-Must use tabs
