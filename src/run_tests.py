"""Unit tests for lexer and parser.

Usage: python3 -m unittest run_tests
"""
import os
import subprocess
import unittest

_PASSED = b"Passed\n"

class TestLexerAndParser(unittest.TestCase):

  def setUp(self):
    os.system("make clean")

  def tearDown(self):
    os.system("make clean")

  def assertPassed(self, stdout, stderr):
    self.assertIn(_PASSED, stdout)
    self.assertEqual(b"", stderr)

  def assertFailed(self, stdout, stderr):
    self.assertEqual(b"", stdout)
    self.assertIn(b"Stdlib.Parsing.Parse_error", stderr)

  def assertProgram(self, program, passes=True):
    os.system("make")
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

  def test_simple_assignment_passes_1(self):
    program = b"int x = 5 \n"
    self.assertProgramPasses(program)

  def test_simple_assignment_passes_2(self):
    program = b"double yEs = -2.5 \n"
    self.assertProgramPasses(program)

  def test_simple_assignment_passes_3(self):
    program = b"string foo = myfunction(1+1+2+3+5)\n"
    self.assertProgramPasses(program)

  def test_object_assignment_passes(self):
    program = b"MyObject foo = MyObject(2, myfunc(2+2)) \n"
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
    program = b"int x = array[6]\n"
    self.assertProgramPasses(program)

  def test_null_assignment(self):
    program = b"int x = NULL\n"
    self.assertProgramPasses(program)

  def test_double_eq(self):
    program = b"3 == x\n"
    self.assertProgramPasses(program)

  def test_nonsense_fails(self):
    program = b"%-$_? !?\n"
    self.assertProgramFails(program)

  def test_valid_statement_without_newline_fails(self):
    program = b"2 + 3 * 5 / 4"
    self.assertProgramFails(program)

  def test_invalid_assignment_fails_1(self):
    program = b"x = 5 \n"
    self.assertProgramFails(program)

  def test_invalid_assignment_fails_2(self):
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

  def test_invalid_double_eq(self):
    program = b"x ==\n"
    self.assertProgramFails(program)

if __name__ == '__main__':
    unittest.main()  
