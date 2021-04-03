"""Unit tests for lexer and parser.

Usage: python3 -m unittest run_tests
"""
import os
import subprocess
import unittest

_PASSED = b"Passed\n"

class TestBoomslang(unittest.TestCase):

  def setUp(self):
    self.makeClean()

  def tearDown(self):
    self.makeClean()

  def makeClean(self):
    process = subprocess.Popen(["make", "clean"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE)
    _, _ = process.communicate()
    process.terminate()

  def make(self):
    process = subprocess.Popen(["make", "repl"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE)
    _, _ = process.communicate()
    process.terminate()

  def assertPassed(self, stdout, stderr):
    self.assertIn(_PASSED, stdout)
    self.assertNotIn(b"Stdlib.Parsing.Parse_error", stderr)

  def assertFailed(self, stdout, stderr):
    self.assertEqual(b"", stdout)
    self.assertTrue(b"lexing: empty token" in stderr or
                    b"Stdlib.Parsing.Parse_error" in stderr or
                    b"Illegal" in stderr or
                    b"Fatal error" in stderr)

  def assertProgram(self, program, passes=True):
    self.make()
    process = subprocess.Popen(["./repl"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE)
    stdout, stderr = process.communicate(input=program)
    process.terminate()
    if passes:
      self.assertPassed(stdout, stderr)
    else:
      self.assertFailed(stdout, stderr)

  def assertProgramPasses(self, program):
    self.assertProgram(program, passes=True)

  def assertProgramFails(self, program):
    self.assertProgram(program, passes=False)

  def test_grammar_is_not_ambiguous(self):
    process = subprocess.Popen(["ocamlyacc", "-v", "parser.mly"],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE)
    stdout, stderr = process.communicate()
    process.terminate()
    self.assertNotIn(b"shift/reduce conflicts", stdout)
    self.assertNotIn(b"shift/reduce conflicts", stderr)

  def test_objoperator_decl_1(self):
    program = b"""
class Horse:
	def _+#(Horse other) returns Horse:
		return Horse()
Horse yak = Horse()
Horse saddle = Horse()
Horse winne = yak+#saddle
"""
    self.assertProgramPasses(program)

  def test_objoperator_decl_2(self):
    program = b"""
class Horse:
	def _^&%$(Horse other) returns Horse:
		return Horse()

	def _$%(Horse other) returns Horse:
		return Horse()

Horse yak = Horse()
Horse saddle = Horse()
Horse poop = Horse()
Horse winne = yak ^&%$ saddle $% poop
"""
    self.assertProgramPasses(program)

  def test_objoperator_ambiguity_1(self):
    program = b"""
class Horse:
	def _+#(Horse other) returns int:
		return 5

Horse yak = Horse()
Horse saddle = Horse()
int winne = 8 + yak+#saddle + 6
"""
    self.assertProgramPasses(program)

  def test_empty_program_passes(self):
    program = b""
    self.assertProgramPasses(program)

  def test_simple_assignment_passes_1(self):
    program = b"int x = 5 \n"
    self.assertProgramPasses(program)

  def test_simple_assignment_passes_2(self):
    program = b"float yEs = .5 \n"
    self.assertProgramPasses(program)

  def test_simple_assignment_passes_3(self):
    program = b'''
def myfunc(int x) returns string:
	return "hey"

string foo = myfunc(1+1+2+3+5)
'''
    self.assertProgramPasses(program)

  def test_simple_assignment_passes_4(self):
    program = b"float yEs = -.5 \n"
    self.assertProgramPasses(program)

  def test_simple_assignment_passes_5(self):
    program = b"float yEs = -2.5 \n"
    self.assertProgramPasses(program)

  def test_assignment_without_type_passes(self):
    program = b"""
int x = 0
x = 5
"""
    self.assertProgramPasses(program)

  def test_object_assignment_passes(self):
    program = b"""
class MyObject:
	required:
		int x
		string y

def myfunc(int x) returns string:
	return "hey"

MyObject foo = MyObject(2, myfunc(2+2))
"""
    self.assertProgramPasses(program)

  def test_arithmetic_passes(self):
    program = b"2 + 3 * 5 / 4\n"
    self.assertProgramPasses(program)

  def test_array_declaration_passes_1(self):
    program = b"int[6] array = [1, 2, 3, 4, 5, 6]\n"
    self.assertProgramPasses(program)

  def test_array_declaration_passes_2(self):
    program = b"[]\n"
    self.assertProgramPasses(program)

  def test_array_declaration_passes_3(self):
    program = b"[[1, 2, 3]]\n"
    self.assertProgramPasses(program)

  def test_array_access_passes(self):
    program = b"""
int[3] array = [0,1,2]
int x = array[2]
"""
    self.assertProgramPasses(program)

  def test_null_assignment(self):
    program = b'''
class MyObject:
	def myfunc():
		println("hey")

MyObject x = NULL
'''
    self.assertProgramPasses(program)

  def test_single_comments_1(self):
    program = b"#comments\n"
    self.assertProgramPasses(program)

  def test_single_comments_2(self):
    program = b"int x = 2 \n int foo = 2+2 # comment\n\n\n int y = 2\n"
    self.assertProgramPasses(program)

  def test_multi_comments(self):
    program = b"/#comments\ncomments#/\n"
    
  def test_double_eq(self):
    program = b"int x = 2\n3 == x\n"
    self.assertProgramPasses(program)

  def test_newlines_1(self):
    program = b"\n"
    self.assertProgramPasses(program)

  def test_newlines_2(self):
    program = b"\n\n\n\n\n"
    self.assertProgramPasses(program)

  def test_newlines_3(self):
    program = b"\n\n\n\n int x = 5 \n\n int y = 6 \n\n"
    self.assertProgramPasses(program)

  def test_newlines_4(self):
    program = b"\n\n\n\n int x = 5\n"
    self.assertProgramPasses(program)

  def test_newlines_5(self):
    program = b"int x = 5 \n\n\n\n"
    self.assertProgramPasses(program)

  def test_self_access_with_field(self):
    program = b"""
class Foo:
	optional:
		string x = "str"

	def myfunc():
		self.x
"""
    self.assertProgramPasses(program)

  def test_self_assignment_with_field(self):
    program = b"""
class MyClass:
	required:
		int x

	def myfunc():
		self.x = 5
"""
    self.assertProgramPasses(program)

  def test_self_with_expression(self):
    program = b'''
class MyClass:
	required:
		int foo

	def myfunc():
		int x = self.foo * 5
'''
    self.assertProgramPasses(program)

  def test_self_access_with_function(self):
    program = b"""
class MyClass:

	def myfunction(int x):
		println("hey")

	def myfunction2():
		self.myfunction(2 % 3)
"""
    self.assertProgramPasses(program)

  def test_object_variable_access(self):
    program = b"""
class MyObject:
	static:
		int x = 5

MyObject myobject = MyObject()
myobject.x
"""
    self.assertProgramPasses(program)

  def test_object_variable_assignment(self):
    program = b"""
class MyObject:
	required:
		boolean fOOo12345

MyObject myobject = MyObject(false)

myobject.fOOo12345 = true
"""
    self.assertProgramPasses(program)

  def test_object_variable_with_expression(self):
    program = b'''
class MyObject:
	static:
		boolean is_this_true = true

MyObject myobject = MyObject()
boolean x = true and myobject.is_this_true
'''
    self.assertProgramPasses(program)

  def test_object_function_call(self):
    program = b'''
class MyObject:
	def myfunction(int a, string b, boolean c):
		println("hey")

int a = 5
string b = "foo"
boolean c = false
MyObject myobject = MyObject()
myobject.myfunction(a, b, c)
'''
    self.assertProgramPasses(program)

  def test_long_initialization_passes(self):
    program = b"long lo = 500000000000L\n"
    self.assertProgramPasses(program)

  def test_valid_minus_eq(self):
    program = b"long fOo = 10L\nfOo -= 500L\n"
    self.assertProgramPasses(program)

  def test_valid_divide_eq_with_object(self):
    program = b'''
class MyObject:
	required:
		int heY
MyObject myobject = MyObject(5)
myobject.heY /= 20
'''
    self.assertProgramPasses(program)

  def test_valid_char_literal_1(self):
    program = b"char c = 'a'\n"
    self.assertProgramPasses(program)

  def test_valid_char_literal_2(self):
    program = b"char c = '''\n"
    self.assertProgramPasses(program)

  def test_valid_string_literal(self):
    program = b'string foo = "foooooOOOoo !@$%^&*()_-+={}|[]:;/<,.>"\n'
    self.assertProgramPasses(program)

  def test_complicated_program_1(self):
    program = b"""
int x = 5
def myfunc(int x, MyObject foo) returns string:
	int y = 5
	int z = 7
	if x > 5:
		if x > 10:
			int z = 20
		elif x > 20:
			println("hey")
		elif x > 30:
			println("hey")
			println("hey")
		else:
			println("hey")
	return "hey"



int x = 50
"""
    self.assertProgramPasses(program)

  def test_complicated_program_2(self):
    program = b"""
int x = 5
def myfunc(int x, MyObject foo) returns string:
	int y = 5
	int z = 7
	if x > 5:
		if x > 10:
			int z = 20
			return "hey"
		elif x > 20:
			println("hey")
			return "hey"
		elif x > 30:
			println("hey")
			println("hey")
			return "hey"
		else:
			println("hey")
			return "hey"
	else:
		return "hey"


def myfunc2(int x, MyObject foo) returns string:
	int y = 5
	int z = 7
	if x > 5:
		if x > 10:
			int z = 20
		elif x > 20:
			println("hey")
		elif x > 30:
			println("hey")
			println("hey")
		else:
			println("hey")
	return "hey"



int x = 50
"""
    self.assertProgramPasses(program)

  def test_complicated_program_3(self):
    program = b"""
int x = 5
def myfunc(int x, MyObject foo) returns void:
	int y = 5
	int z = 7
	if x > 5:
		if x > 10:
			int z = 20
		elif x > 20:
			println("hey")
		elif x > 30:
			println("hey")
			println("hey")
		else:
			println("hey")
"""
    self.assertProgramPasses(program)

  def test_class_declaration_1(self):
    program = b"""
class MyObject:
	static:
		int x = 5
		string foo = "bar"


	required:
		int z
		float fOOOO



	optional:
		boolean boo = true
"""
    self.assertProgramPasses(program)

  def test_class_declaration_2(self):
    program = b"""
class MyObject:
	static:
		int x = 5
		string foo = "bar"


	required:
		int z
		float fOOOO



	optional:
		boolean boo = true

"""
    self.assertProgramPasses(program)

  def test_class_declaration_3(self):
    program = b"""
class MyObject:
	static:
		int x = 5
		string foo = "bar"


	required:
		int z
		float fOOOO



	optional:
		boolean boo = true


	def mymethod():
		self.x = 5

	def _++%%(MyObject other) returns MyObject:
		return MyObject(1, 5.0)


"""
    self.assertProgramPasses(program)

  def test_class_declaration_4(self):
    program = b"""
class MyObject:
	static:
		int x = 5
		string foo = "bar"


	required:
		int z
		float fOOOO



	optional:
		boolean boo = true


	def mymethod():
		self.x = 10







	def _++%%(MyObject other) returns MyObject:
		return MyObject(1, 5.0, false)
	def mymethod2(int x) returns int:
		return 5
"""
    self.assertProgramPasses(program)

  def test_class_declaration_5(self):
    program = b"""
class MyObject:
	def _++%%(MyObject other) returns MyObject:
		return MyObject()
	def mymethod2(int x) returns int:
		return 5
"""
    self.assertProgramPasses(program)

  def test_class_declaration_6(self):
    program = b"""
class MyObject:
	static:
		int x = 5
"""
    self.assertProgramPasses(program)

  def test_class_declaration_7(self):
    program = b"""
class MyObject:
	required:
		int x
"""
    self.assertProgramPasses(program)

  def test_class_declaration_8(self):
    program = b"""
class MyObject:
	optional:
		int x = 5
"""
    self.assertProgramPasses(program)

  def test_class_declaration_9(self):
    program = b"""
class MyObject:
	static:
		int x = 5
	required:
		int y
"""
    self.assertProgramPasses(program)

  def test_class_declaration_10(self):
    program = b"""
class MyObject:
	static:
		int x = 5
	optional:
		int y = 5
"""
    self.assertProgramPasses(program)

  def test_class_declaration_11(self):
    program = b"""
class MyObjectTwo:
	required:
		string x


	optional:
		string z = "foo"
"""
    self.assertProgramPasses(program)

  def test_loop_1(self):
    program = b"""
int x = 0
loop x+=1 while x<100:
	println("hey")
"""
    self.assertProgramPasses(program)

  def test_loop_2(self):
    program = b"""
int x = 0
loop while x<100:
	println("hey")
"""
    self.assertProgramPasses(program)

  def test_valid_statement_without_newline_succeeds_1(self):
    program = b"2 + 3 * 5 / 4"
    self.assertProgramPasses(program)

  def test_valid_statement_without_newline_succeeds_2(self):
    program = b'println("hello world")'
    self.assertProgramPasses(program)

  def test_valid_returns_succeeds(self):
    program = b'''
def my_func(int x) returns int:
	if x > 5:
		println("hey")
	else:
		return 5
	return 1
'''
    self.assertProgramPasses(program)

  def test_valid_construct_succeeds(self):
    program = b'''
class MyClass:
	def print_5():
		println("5")

MyClass my_object = MyClass()
MyClass x = my_object
x.print_5()
'''
    self.assertProgramPasses(program)

  def test_valid_array_succeeds(self):
    program = b'''
def my_func(int x) returns int:
	return x

int[5] my_array = [1, (2+2), my_func(1), my_func(1) + 1, 2 * (3 + 1)]
'''
    self.assertProgramPasses(program)

  def test_valid_not_succeeds(self):
    program = b'''
boolean x = true
not x
'''
    self.assertProgramPasses(program)

  def test_valid_neg_succeeds(self):
    program = b'''
int x = -1
x = -x
'''
    self.assertProgramPasses(program)

  def test_valid_self_return_succeeds(self):
    program = b'''
class MyClass:
	def foo() returns MyClass:
		return self
'''
    self.assertProgramPasses(program)

  def test_constructor_overwriting_is_allowed(self):
    program = b'''
class MyClass:
	required:
		int x
	def construct(int x):
		println("i am overwriting the default constructor")

'''
    self.assertProgramPasses(program)

  def test_assigning_to_array_index_succeeds(self):
    program = b'''
string[3] arr = ["one", "two", "three"]
arr[1] = "newvalue"
arr[1]
'''
    self.assertProgramPasses(program)

  def test_array_update_succeeds(self):
    program = b'''
int[2] arr = [1, 2]
arr[0] += 5
'''
    self.assertProgramPasses(program)

  def test_nonsense_fails(self):
    program = b"%-$_? !?\n"
    self.assertProgramFails(program)

  def test_invalid_assignment_fails_1(self):
    program = b"int x = \n"
    self.assertProgramFails(program)

  def test_assert_invalid_function_call_fails(self):
    program = b"myfunc(1,2,) \n"
    self.assertProgramFails(program)

  def test_invalid_array_literal_fails_1(self):
    program = b"[1, 2, 3,]\n"
    self.assertProgramFails(program)

  def test_invalid_array_literal_fails_2(self):
    program = b"[[1, 2, 3]\n"
    self.assertProgramFails(program)

  def test_invalid_arithmetic_fails(self):
    program = b"5 *\n"
    self.assertProgramFails(program)

  def test_invalid_null_assignment(self):
    program = b"NULL = int x\n"
    self.assertProgramFails(program)

  def test_invalid_single_comment(self):
    program = b"/ comment\n"
    self.assertProgramFails(program)

  def test_invalid_multi_comment(self):
    program = b"comment #/\n"
    self.assertProgramFails(program)

  def test_invalid_double_eq(self):
    program = b"x ==\n"
    self.assertProgramFails(program)

  def test_invalid_newlines(self):
    program = b"\n = x\n"
    self.assertProgramFails(program)

  def test_invalid_self_usage(self):
    program = b"self.\n"
    self.assertProgramFails(program)

  def test_invalid_object_usage(self):
    program = b"myobject.\n"
    self.assertProgramFails(program)

  def test_invalid_primitive_type_fails(self):
    program = b"short x = 500\n"
    self.assertProgramFails(program)

  def test_invalid_long_assignment_fails(self):
    program = b"long fOoo = 5000LL\n"
    self.assertProgramFails(program)

  def test_invalid_floating_point_fails(self):
    program = b"float v = 1.\n"
    self.assertProgramFails(program)

  def test_invalid_plus_eq(self):
    program = b"int x += 5\n"
    self.assertProgramFails(program)

  def test_invalid_times_eq(self):
    program = b"int x *= 6\n"
    self.assertProgramFails(program)

  def test_simple_objop(self):
    program = b"Wfoueb $^@#&@ Wefoudvn\n"
    self.assertProgramFails(program)

  def test_multi_objop(self):
    program = b"Wfoueb $^@#&@ Wefoudvn %&@@$^ HdfEFow\n"
    self.assertProgramFails(program)

  def test_objoperator_nofirsteq_1(self):
    program = b"woof = woofwoof =^&$# harry\n"
    self.assertProgramFails(program)

  def test_objoperator_nofirsteq_2(self):
    program = b"woof = woofwoof ?= harry\n"

  def test_invalid_char_literal_fails(self):
    program = u"char c = '😀'".encode('utf-16')
    self.assertProgramFails(program)

  def test_invalid_string_literal_fails(self):
    program = u'string x = "fOOOO 😀 foo"\n'.encode('utf-16')
    self.assertProgramFails(program)

  def test_bad_indentation_fails_1(self):
    program = b"""
int x = 5
def myfunc(int x, MyObject foo) returns string:
	int y = 5
	int z = 7
	if x > 5:
		if x > 10:
			int z = 20
		elif x > 20:
			println("hey")
		elif x > 30:
			println("hey")
			println("hey")
		else:
				println("hey")




int x = 50
"""
    self.assertProgramFails(program)

  def test_bad_indentation_fails_2(self):
    program = b"""
int x = 5
def myfunc(int x, MyObject foo) returns string:
	int y = 5
	int z = 7
	if x > 5:
		if x > 10:
			int z = 20
		elif x > 20:
			println("hey")
		elif x > 30:
			println("hey")
			println("hey")
		else:
		println("hey")




int x = 50
"""
    self.assertProgramFails(program)

  def test_bad_loop_fails(self):
    program = b"""
loop x+2 while:
	2+2
"""
    self.assertProgramFails(program)

  def test_bad_returns_1(self):
    program = b'''
def my_func() returns int:
	if x > 5:
		println("hey")
	else:
		return 5
'''
    self.assertProgramFails(program)

  def test_bad_returns_2(self):
    program = b'''
def my_func() returns int:
	println("hey")
'''
    self.assertProgramFails(program)

  def test_bad_constructor_1(self):
    program = b'''
class MyClass:
	static:
		my_object.x = 5
'''
    self.assertProgramFails(program)

  def test_bad_constructor_2(self):
    program = b'''
class MyClass:
	static:
		void x = NULL
'''
    self.assertProgramFails(program)

  def test_void_assignment_fails(self):
    program = b"""void x = 5"""
    self.assertProgramFails(program)

  def test_null_assignment_fails(self):
    program = b"""NULL x = NULL"""
    self.assertProgramFails(program)

  def test_duplicate_function_args_fails(self):
    program = b"""
def my_func(int x, float x) -> returns int:
	return 5
"""
    self.assertProgramFails(program)

  def test_invalid_array_access_fails(self):
    program = b"""
int x = 5
x[5]
"""
    self.assertProgramFails(program)

  def test_bad_negation_fails(self):
    program = b"""
boolean x = true
-x
"""
    self.assertProgramFails(program)

  def test_bad_not_fails(self):
    program = b"""
int x = 5
not x
"""
    self.assertProgramFails(program)

  def test_bad_array_init_fails(self):
    program = b"""
def my_func(int x) returns string:
	return "hey"

int[5] my_array = [1, (2+2), my_func(1), my_func(1) + 1, 2 * (3 + 1)]
"""
    self.assertProgramFails(program)

  def test_invalid_type_return_fails_1(self):
    program = b"""
def my_func() returns int:
	if x > 5:
		return
	else:
		return 5
"""
    self.assertProgramFails(program)

  def test_invalid_type_return_fails_2(self):
    program = b"""
def my_func() returns int:
	if x > 5:
		return "string"
	else:
		return 5
"""
    self.assertProgramFails(program)

  def test_nothing_comes_after_return(self):
    program = b"""
def myfunc():
  return
  println("hey")
"""
    self.assertProgramFails(program)

  def test_no_matching_signature_fails(self):
    program = b"""
def myfunc(string x) returns int:
  return 5

myfunc(5)
"""
    self.assertProgramFails(program)

  def test_invalid_self_return_fails(self):
    program = b"""
class MyClass:
	def foo() returns int:
		return self
"""

  def test_self_on_its_own_fails(self):
    program = b"""self\n"""
    self.assertProgramFails(program)

  def test_self_as_identifier_fails(self):
    program = b"""
int self = 5
self
"""
    self.assertProgramFails(program)

  def test_constructors_must_not_have_returns(self):
    program = b"""
class MyClass:
	def construct() returns int:
		return 5
"""
    self.assertProgramFails(program)

  def test_bad_array_assign_fails(self):
    program = b"""
string[3] arr = ["one", "two", "three"]
arr[1] = 50.0
"""
    self.assertProgramFails(program)


if __name__ == '__main__':
    unittest.main()  
