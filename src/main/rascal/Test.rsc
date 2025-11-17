module Test
import Syntax;
extend analysis::typepal::TestFramework;
extend Checker;
import ParseTree;

TModel calcTModelForTree(Tree pt){
    return collectAndSolve(pt, modelName = "calc");
}

TModel calcTModelFromStr(str text){
    pt = parse(#start[Calc], text).top;
    return aluTModelForTree(pt); 
}

test bool calcTests() {
     return runTests([|project://proyecto3/src/main/rascal/tests.ttl|], 
                     calcTModelForTree, 
                     runName="ALU");
}

bool main() = calcTests();
