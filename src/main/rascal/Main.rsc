module Main

import IO;
import Syntax;
import ParseTree;
import AST;
import Interpreter;
import String;
import Implode;

void main() {
  println("=== ALU Language Demo ===\n");

  list[str] testFiles = [
    "test1.alu",
    "test_comparison.alu",
    "test_cond.alu",
    "test_loop.alu",
    "test_for_in.alu",
    "test_logic.alu",
    "test_math.alu",
    "test_invocation.alu",
    "test_iterator_range.alu",
    "test_struct_expr.alu",
    "test_data_constructor.alu",
    "test_data_function.alu"
  ];

  for (file <- testFiles) {
    println("=== Running file: <file> ===\n");
    runFile(|project://proyecto3/instance/<file>|);
    println("\n---\n");
  }
}

void runExample(str title, str code) {
  println(title);
  println("Code: \n<code>");
  println("\nExecuting...");
  runProgram(code);
  println("\n---\n");
}

Tree resolveAmb(Tree t) {
  return bottom-up visit(t) {
    case a:amb({Tree first, *Tree rest}) => first
  };
}

void runProgram(str code) {
  try {
    println("Parsing...");
    pt = parse(#start[Program], trim(code), allowAmbiguity=true);
    println("Resolving ambiguities...");
    pt = resolveAmb(pt);
    println("Imploding to AST...");
    AST::Program ast = implodeFromTree(pt.top);
    println("Executing...");
    evalProgram(ast);
    println("Done!");
  } catch ParseError(loc l): {
    println("Parse Error at <l>"); 
  } catch err: {
    println("Error: <err>"); 
  }
}

void runFile(loc file) {
  try {
    println("Reading file: <file>"); 
    str code = readFile(file);
    runProgram(code);
  } catch IO(str msg): {
    println("File error: <msg>"); 
  } catch err: {
    println("Error reading file: <err>"); 
  }
}
