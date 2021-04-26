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
## Basics
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

Arrays can also be initialized with a default value, similar to the "new" keyword in Java. Once initialized, you can access their contents using a standard syntax. The length of an array is obtained using the built-in len keyword.
```
int[] arr = default[10]
arr[5] = 5
int array_len = len(arr)
```

Finally, once a variable is properly be initialized, it can be updated to another value, as long as that value has the same type as the original initialization.
```
int a = 5
a = 10  # this is legal
string str = "foo"
str = 10  # this is illegal
```

To inspect output of the program, the println() built-in function can be used. println() is polymorphic and can take any type. If the type is not a string, it will be automatically converted to a string.
```
# All of these work
println("Hello, world")
println(10)
println(10.0)
println(true)
```

Comments use the following syntax:
```
# This is a single-line comment
/# This a multi-line
comment #/
```

Our language supports the following mathematical and boolean operators. Ints, longs, and floats can be automatically coerced to each other in binary operators.
```
+ - * / % == > < >= <= or and not

int a = 5 + 10  # 15
float b = 2.0 + 2  # 4.0
boolean test = 10 > 5  # true

boolean foo = true
boolean not_foo = not foo
boolean another_bool = foo and not_foo or foo
```

Boomslang also supports the following convenience operators for performing updates:
```
+= -= *= /=

int a = 5
a += 5  # a is now 10. this line is equivalent to a = a + 5
```

## Control flow
Boomslang contains two types of control flow statements: if/elif/else and loops. Ifs follow the same syntax and semantics as Python, meaning all of the following are supported:
```
if a == 0:
  println(a)
    
if a == 0:
  println("a == 0")
else:
  println("a != 0")
    
if a == 0:
  println("a == 0")
elif a == 1:
  println("a == 1")
else:
  println("a != 0")
    
if a == 0:
  println("a == 0")
elif a == 1:
  println("a == 1")
elif a == 2:
  println("a == 2")
else:
  println("a != 0")
    
if a == 0:
  println("a == 0")
elif a == 1:
  println("a == 1")
elif a == 2:
  println("a == 2")
```

Note that for **all** indentation in the language **tabs must be used** with **exactly one tab per indentation level**. Markdown support for tab characters is limited so copy/pasting from this tutorial may not work as tabs get auto-converted to spaces.

Loops in Boomslang use a novel syntax not seen in any other known programming language. Loops can be defined in two different ways using the "loop" keyword.
```
int i = 0
loop i += 1 while i < 100:
  println(i)
    
int i = 0
loop while i < 100:
  println(i)
  i += 1
```

## Functions
By convention, function names and variable names should be snake_case rather than camelCase. By necessity, class names must start with an uppercase letter and only contain letters (no numbers). **Also by necessity, all indentation must be done using tabs. Spaces will not work.**

Functions are defined in a syntax similar to a strongly typed version of Python. A unique aspect of the syntax is that the return type is indicated via the "returns" keyword. Here are some examples of functions in Boomslang:
```
def my_func(int a) returns int:
  return a

def print_hello_world():
  println("hello world")
    
def gcd(int a, int b) returns int:
  loop while a != b:
    if a > b:
      a = a - b
    else:
      b = b - a
  return a
```

Functions do not need to be defined before use. Functions are allowed to be self-recursive or mutually-recurisve with other functions.

## Classes
### Class basics
While Boomslang does not require all functions and variables to be part of a class, it does support classes and object-oriented features.

Fields within a class always appear under the header "static", "required", or "optional". These headers must always appear in that order (static, then required, then optional). Any or all of the three can be omitted for brevity.

A basic class looks like this:
```
class MyClass:
  static:
    int a = 0

  required:
    string b

  optional:
    boolean c = true
   
  def my_method() returns string:
    return self.b
```

All fields and methods on a class used within a class must always be prefixed with the "self" keyword. Objects are instantiated using the following syntax:
```
MyClass my_class1 = MyClass("new string")
my_class1.b  # "new_string"
my_class1.c  # true

MyClass my_class2 = MyClass("other string", false)
my_class2.b  # "other string"
my_class2.c  # false
```

Objects can also be initialized as NULL. Only the object type in Boomslang may be NULL.
```
MyClass foo = NULL  # this is legal
foo.b  # this will throw a NullPointerException
```

How do the above instantiations work, considering there was no constructor defined by the user? Boomslang automatically generates default constructors for dataclasses. Two constructors will be generated: One that takes just the required fields and initializes the optional ones to their default, and one that takes all of the fields (both optional and required). So for the above class, the following two constructor methods are automatically added to the class.
```
def construct(string b):
  self.b = b
  self.c = true
  
def construct(string b, boolean c):
  self.b = b
  self.c = c
```

Constructors can be overwritten using the "construct" keyword. Objects can also be printed, which will call the object's to_string() method. This is a special function that takes no arguments and returns a string. By default, Boomslang will add a meaningful to_string to each user-defined class so the user doesn't have to. However, similar to constructors, the user can overwrite it if they wish.
```
println(my_class1)  # this will print the contents of all the class fields

/#
MyClass:
a:0
b:new string
c:true
#/
```

### Operator overloading for objects
Boomslang supports the standard way to call methods on an object:
```
myobject.mymethod()
```

In addition to this, users can overwrite built-in binary operators that work on primitives to work on objects as well. This is especially useful for overwriting the double equals (==) function.
```
class IntLinkedList:
  required:
    int element

  optional:
    IntLinkedList next = NULL

  def _==(IntLinkedList other) returns boolean:
    # Custom defined equality function
    if self == NULL:
      return other == NULL
    elif other == NULL:
      return false
    else:
      return (self.element == other.element) and (self.next == other.next)

IntLinkedList a = IntLinkedList(1)
IntLinkedList b = IntLinkedList(1)
IntLinkedList c = IntLinkedList(2)
println(a == b)  # true
println(b == c)  # false
```

### Generic classes
The int linked list class above is nice, but it would be tedious to rewrite the linked list class for every possible type of linked list. To make this more convenient, the user is allowed to define generic classes. Generic classes cannot be instantiated directly as objects, but can be used to declare classes very succinctly. The following is a generic form of the above linked list.

```
class LinkedList[T]:
  required:
    T element

  optional:
    LinkedList next = NULL
    
  def _==(LinkedList other) returns boolean:
    # Custom defined equality function
    if self == NULL:
      return other == NULL
    elif other == NULL:
      return false
    else:
      return (self.element == other.element) and (self.next == other.next)
```

To use the generic class, it must be used in a real class declaration, using the following syntax:
```
class IntLinkedList = LinkedList(int)
class StringLinkedList = LinkedList(string)

IntLinkedList my_list1 = IntLinkedList(1)
StringLinkedList my_list2 = StringLinkedList("string")
```
