module Test

import Syntax;
import Checker;
import ParseTree;
extend analysis::typepal::TestFramework;

// ---- Testing ---------------------------------------------------------------

TModel aluTModelForTree(Tree pt){
    if (pt has top) pt = pt.top;
    return collectAndSolve(pt);
}

TModel aluTModelFromStr(str text){
    pt = parse(#start[Program], text).top;
    return aluTModelForTree(pt);
}

test bool aluTests() {
    return runTests([|project://proyecto3/src/main/rascal/test.ttl|], 
                    #start[Program], 
                    aluTModelForTree, 
                    runName="ALU");
}

bool main() = aluTests();