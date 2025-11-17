module SimpleTest

import IO;
import Syntax;
import ParseTree;

void testParse() {
  println("=== Testing Comparison Operators Parsing ===\n");
  
  str code = readFile(|project://proyecto3/instance/test_comparison.alu|);
  
  println("Code:");
  println(code);
  println("\nParsing...");
  
  try {
    Program tree = parse(#Program, code);
    println("✓ SUCCESS! Parse tree created.");
    println("\nParse tree:");
    println(tree);
  } catch value err: {
    println("✗ FAILED: <err>");
  }
}

void main() {
  testParse();
}
