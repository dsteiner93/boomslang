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

if __name__ == '__main__':
    unittest.main()  
