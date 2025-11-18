module Test

import analysis::typepal::TypePal;
import Checker; 
import ParseTree;
import AST;
import Implode;
import IO;
import String;
import List;
import Message;

bool runTest(str testName, str code, bool shouldPass, str expectedError) {
    println("\n--- Test: <testName> ---");
    println("Code: <code>");
    try {
        Tree pt = parse(#start[Program], code);
        AST::Program ast = implode(#AST::Program, pt.top);
        TModel tm = typeCheck(ast);
        
        list[Message] errors = [m | m <- getMessages(tm), isError(m)];
        
        if (shouldPass) {
            if (size(errors) == 0) {
                println("✓ PASS: No errors (as expected)");
                return true;
            } else {
                println("✗ FAIL: Expected no errors but got:");
                for (e <- errors) println("  - <e>");
                return false;
            }
        } else {
            if (size(errors) > 0) {
                bool foundExpected = false;
                for (e <- errors) {
                    str msg = "<e>";
                    if (findFirst(toLowerCase(msg), toLowerCase(expectedError)) >= 0) {
                        foundExpected = true;
                    }
                }
                if (foundExpected) {
                    println("✓ PASS: Found expected error containing \'<expectedError>\'");
                    for (e <- errors) println("  Error: <e>");
                    return true;
                } else {
                    println("✗ FAIL: Expected error containing \'<expectedError>\' but got:");
                    for (e <- errors) println("  - <e>");
                    return false;
                }
            } else {
                println("✗ FAIL: Expected error but got none");
                return false;
            }
        }
    } catch e: {
        println("✗ FAIL: Exception: <e>");
        return false;
    }
}

void main() {
    println("==============================================");
    println("    ALU TYPE CHECKER - TEST SUITE");
    println("==============================================");
    
    int passed = 0;
    int total = 0;
    
    // Test 1: Valid integer assignment
    total += 1;
    if (runTest(
        "OkInteger1",
        "function testCond() do x:Int = 2 end testCond",
        true,
        ""
    )) passed += 1;
    
    // Test 2: Valid integer assignment with variable
    total += 1;
    if (runTest(
        "OkInteger2",
        "function testCond() do y:Int = 4 x:Int = y end testCond",
        true,
        ""
    )) passed += 1;
    
    // Test 3: Undefined variable
    total += 1;
    if (runTest(
        "NoOkInteger1",
        "function testCond() do y:Int = 4 x:Int = z end testCond",
        false,
        "undefined"
    )) passed += 1;
    
    // Test 4: Type mismatch
    total += 1;
    if (runTest(
        "NoOkInteger2",
        "function testCond() do y:String = \"HolaMundo\" x:Int = y end testCond",
        false,
        "type"
    )) passed += 1;
    
    // Test 5: Valid float assignment
    total += 1;
    if (runTest(
        "OkFloat1",
        "function testCond() do x:Float = 2.5 end testCond",
        true,
        ""
    )) passed += 1;
    
    // Test 6: Valid float assignment with variable
    total += 1;
    if (runTest(
        "OkFloat2",
        "function testCond() do y:Float = 4.2 x:Float = y end testCond",
        true,
        ""
    )) passed += 1;
    
    // Test 7: Boolean type
    total += 1;
    if (runTest(
        "OkBool1",
        "function testCond() do x:Bool = true end testCond",
        true,
        ""
    )) passed += 1;
    
    // Test 8: String type
    total += 1;
    if (runTest(
        "OkString1",
        "function testCond() do x:String = \"Hello\" end testCond",
        true,
        ""
    )) passed += 1;
    
    println("\n==============================================");
    println("RESULTS: <passed>/<total> tests passed");
    if (passed == total) {
        println("✓✓✓ ALL TESTS PASSED! ✓✓✓");
    } else {
        println("✗ <total - passed> test(s) failed");
    }
    println("==============================================");
}