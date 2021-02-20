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
    self.assertEqual(_PASSED, stdout)
    self.assertEqual(b"", stderr)

  def assertFailed(self, stdout, stderr):
    self.assertEqual(b"", stdout)
    self.assertIn(b"Stdlib.Parsing.Parse_error", stderr)

  def assertProgramPasses(self, program):
    os.system("make")
    process = subprocess.Popen(["./repl"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE)
    stdout, stderr = process.communicate(input=program)
    process.terminate()
    self.assertPassed(stdout, stderr)

  def assertProgramFails(self, program):
    os.system("make")
    process = subprocess.Popen(["./repl"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE)
    stdout, stderr = process.communicate(input=program)
    process.terminate()
    self.assertFailed(stdout, stderr)

  def test_grammar_is_not_ambiguous(self):
    process = subprocess.Popen(["ocamlyacc", "-v", "parser.mly"],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE)
    stdout, stderr = process.communicate()
    process.terminate()
    self.assertNotIn(b"shift/reduce conflicts", stdout)
    self.assertNotIn(b"shift/reduce conflicts", stderr)

  def test_simple_assignment_passes(self):
    program = b"int x = 5 \n"
    self.assertProgramPasses(program)

  def test_object_assignment_passes(self):
    program = b"MyObject foo = MyObject(2, myfunc(2+2)) \n"
    self.assertProgramPasses(program)

  def test_invalid_assignment_fails(self):
    program = b"x = 5"
    self.assertProgramFails(program)

  def test_assert_invalid_function_call_fails(self):
    program = b"myfunc(1,2,)"
    self.assertProgramFails(program)


if __name__ == '__main__':
    unittest.main()  
