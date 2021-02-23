"""Unit tests for lexer and parser.

Usage: python3 -m unittest run_tests
"""
import os
import subprocess
import unittest

_PASSED = b"Passed\n"

class TestLexerAndParser(unittest.TestCase):

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
    process = subprocess.Popen(["make"],
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
    self.assertIn(b"Stdlib.Parsing.Parse_error", stderr)

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

  def test_empty_program_passes(self):
    program = b""
    self.assertProgramPasses(program)

  def test_simple_assignment_passes_1(self):
    program = b"int x = 5 \n"
    self.assertProgramPasses(program)

  def test_simple_assignment_passes_2(self):
    program = b"float yEs = -2.5 \n"
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

  def test_single_comments_1(self):
    program = b"#comments\n"
    self.assertProgramPasses(program)

  def test_single_comments_2(self):
    program = b"int x = 2 \n MyObject foo = 2+2 # comment\n\n\n int y = 2\n"
    self.assertProgramPasses(program)

  def test_multi_comments(self):
    program = b"/#comments\ncomments#/\n"
    
  def test_double_eq(self):
    program = b"3 == x\n"
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
    program = b"self.x\n"
    self.assertProgramPasses(program)

  def test_self_assignment_with_field(self):
    program = b"self.x = 5\n"
    self.assertProgramPasses(program)

  def test_self_with_expression(self):
    program = b"int x = self.foo * 5\n"
    self.assertProgramPasses(program)

  def test_self_access_with_function(self):
    program = b"self.myfunction(2 % 3) \n"
    self.assertProgramPasses(program)

  def test_object_variable_access(self):
    program = b"myobject.x \n"
    self.assertProgramPasses(program)

  def test_object_variable_assignment(self):
    program = b"myobject.fOOo12345 = true\n"
    self.assertProgramPasses(program)

  def test_object_variable_with_expression(self):
    program = b"boolean x = true and myobject.is_this_true\n"
    self.assertProgramPasses(program)

  def test_object_function_call(self):
    program = b"myobject.myfunction(a, b, c)\n\n"
    self.assertProgramPasses(program)

  def test_long_initialization_passes(self):
    program = b"long lo = 500000000000L\n"
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
    program = b"float v = .5\b"
    self.assertProgramFails(program)


if __name__ == '__main__':
    unittest.main()  
