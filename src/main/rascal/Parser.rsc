module Parser

import Syntax;
import ParseTree;
import IO;
import AST; 
import Implode; 

public AST::Program parseProgram(str code) {
  Tree t = parse(#start[Program], code);
  return implode(#AST::Program, t);
}

public AST::Program parseFile(str path) {
  str code = readFile(|file://<path>|);
  return parseProgram(code);
}

