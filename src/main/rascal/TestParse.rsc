module TestParse

import IO;
import Syntax;
import ParseTree;
import Exception;

void testComparisons() {
  println("=== Testing Comparison Operators ===\n");
  
  // Test each operator individually
  testOperator("less than", "function test() do result = 5 \< 10 end test");
  testOperator("greater than", "function test() do result = 10 \> 5 end test");
  testOperator("less or equal", "function test() do result = 5 \<= 10 end test");
  testOperator("greater or equal", "function test() do result = 10 \>= 5 end test");
  testOperator("not equal", "function test() do result = 5 \<\> 10 end test");
  testOperator("equal", "function test() do result = 5 = 5 end test");
  
  println("\n=== Testing from file ===");
  testFile();
}

void testOperator(str name, str code) {
  try {
    println("Testing <name>...");
    Program tree = parse(#Program, code);
    println("   SUCCESS: Parsed correctly!");
  } catch value err: {
    println("   ERROR: <err>");
  }
  println("");
}

void testFile() {
  try {
    loc file = |project://proyecto3/instance/test_comparison.alu|;
    println("Reading: <file>");
    str code = readFile(file);
    println("Code:\n<code>");
    println("\nParsing...");
    Program tree = parse(#Program, code);
    println(" SUCCESS: File parsed correctly!");
  } catch value err: {
    println(" Error: <err>");
  }
}

void main() {
  testComparisons();
}
