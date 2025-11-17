module Checker
import IO;
import Syntax;
extend analysis::typepal::TypePal;

data AType = 
    intType()
    | boolType()
    | charType()
    | stringType()
    | floatType()
    ;

data IdRole =
    fieldId()
    |structID()
    ;

str prettyATYpe(boolType()) = "bool";
str prettyATYpe(intType()) = "int";
str prettyATYpe(charType()) = "char";
str prettyATYpe(stringType()) = "string";
str prettyATYpe(floatType()) = "float";

