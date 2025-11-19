module Test
import Syntax;
extend analysis::typepal::TestFramework;
extend Checker;
import ParseTree;

// ---- Testing ---------------------------------------------------------------

TModel aluTModelForTree(Tree pt){
    return collectAndSolve(pt);
}

TModel aluTModelFromStr(str text){
    pt = parse(#start[Program], text).top;
    return aluTModelForTree(pt);
}

test bool aluTests() {
     return runTests([|project://proyecto3/src/main/rascal/test.ttl|], 
                     #Program, 
                     aluTModelForTree, 
                     runName="ALU");
}

bool main() = aluTests();
