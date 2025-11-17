module Implode

import Syntax;
import AST;
import ParseTree;

public AST::Program implodeProgram(str code) {
  Tree t = parse(#start[Program], code);
  return implode(#AST::Program, t);
}

public AST::Program implodeTree(Tree t) {
  return implode(#AST::Program, t);
}
