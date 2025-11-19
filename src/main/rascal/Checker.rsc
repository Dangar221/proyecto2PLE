module Checker

import Syntax;
import ParseTree;
extend analysis::typepal::TypePal;

// =======================
// Type definitions
// =======================
data AType
  = intType()
  | boolType()
  | charType()
  | stringType()
  | floatType()
  | customType(str name)
  | unknownType()
  ;

str prettyAType(intType()) = "int";
str prettyAType(boolType()) = "bool";
str prettyAType(charType()) = "char";
str prettyAType(stringType()) = "str";
str prettyAType(floatType()) = "float";
str prettyAType(customType(name)) = name;
str prettyAType(unknownType()) = "unknown";

AType syntaxTypeToAType(Type t) {
  if ((Type) `Int` := t) return intType();
  if ((Type) `Bool` := t) return boolType();
  if ((Type) `Char` := t) return charType();
  if ((Type) `String` := t) return stringType();
  if ((Type) `Float` := t) return floatType();
  return unknownType();
}

// Helper to validate custom types in field declarations
void validateFieldType(Type t, Tree src, Collector c) {
  AType atype = syntaxTypeToAType(t);
  // For now, we only validate built-in types
  // Custom types would need additional validation logic
  if (atype == unknownType()) {
    c.report(error(src, "Unknown type: <t>"));
  }
}

// =======================
// IdRole definitions for TypePal
// =======================
data IdRole
  = variableId()
  | functionId()
  | dataId()
  | fieldId()
  ;

tuple[list[str] typeNames, set[IdRole] idRoles] getTypeNamesAndRole(customType(str name)) {
  return <[name], {dataId()}>;  
}

default tuple[list[str] typeNames, set[IdRole] idRoles] getTypeNamesAndRole(AType _) {
  return <[], {}>;
}

// =======================
// Config
// =======================
TypePalConfig config() = tconfig(
  verbose = false,
  logTModel = false
);

// =======================
// Entry point - collect and solve constraints
// =======================
void collect(current: (Program) `<Module+ modules>`, Collector c) {
  for (m <- modules) {
    collect(m, c);
  }
}

// =======================
// Module collection
// =======================
void collect(current: (Module) `<FunctionDef fd>`, Collector c) {
  collect(fd, c);
}

void collect(current: (Module) `<Data d>`, Collector c) {
  collect(d, c);
}

// =======================
// Data declarations
// =======================
void collect(current: (Data) `<Id assignName> = data with <{TypedId ","}+ vars> <DataBody body> end <Id endName>`, Collector c) {
  c.define("<assignName>", dataId(), assignName, defType(customType("<endName>")));
  c.enterScope(current);
    for (v <- vars) {
      if ((TypedId) `<Id n> : <Type t>` := v) {
        validateFieldType(t, v, c);
        c.define("<n>", fieldId(), n, defType(syntaxTypeToAType(t)));
      }
      else if ((TypedId) `<Type t> <Id n>` := v) {
        validateFieldType(t, v, c);
        c.define("<n>", fieldId(), n, defType(syntaxTypeToAType(t)));
      }
      else if ((TypedId) `<Id n>` := v) {
        c.define("<n>", fieldId(), n, defType(unknownType()));
        c.report(warning(v, "Field <n> has no type annotation"));
      }
    }
    collect(body, c);
  c.leaveScope(current);
  if ("<assignName>" != "<endName>") {
    c.report(error(current, "Data definition end name mismatch"));
  }
}

void collect(current: (Data) `data with <{TypedId ","}+ vars> <DataBody body> end <Id endName>`, Collector c) {
  c.define("<endName>", dataId(), endName, defType(customType("<endName>")));
  c.enterScope(current);
    for (v <- vars) {
      if ((TypedId) `<Id n> : <Type t>` := v) {
        validateFieldType(t, v, c);
        c.define("<n>", fieldId(), n, defType(syntaxTypeToAType(t)));
      }
      else if ((TypedId) `<Type t> <Id n>` := v) {
        validateFieldType(t, v, c);
        c.define("<n>", fieldId(), n, defType(syntaxTypeToAType(t)));
      }
      else if ((TypedId) `<Id n>` := v) {
        c.define("<n>", fieldId(), n, defType(unknownType()));
        c.report(warning(v, "Field <n> has no type annotation"));
      }
    }
    collect(body, c);
  c.leaveScope(current);
}

// =======================
// DataBody collection
// =======================
void collect(current: (DataBody) `<Constructor cons>`, Collector c) {
  collect(cons, c);
}

void collect(current: (DataBody) `<FunctionDef fd>`, Collector c) {
  collect(fd, c);
}

// =======================
// Constructor collection
// =======================
void collect(current: (Constructor) `<Id name> = struct ( <{TypedId ","}+ vars> )`, Collector c) {
  c.define("<name>", functionId(), name, defType(unknownType()));
  c.enterScope(current);
    for (v <- vars) {
      if ((TypedId) `<Id n> : <Type t>` := v) {
        validateFieldType(t, v, c);
        c.define("<n>", fieldId(), n, defType(syntaxTypeToAType(t)));
      }
      else if ((TypedId) `<Type t> <Id n>` := v) {
        validateFieldType(t, v, c);
        c.define("<n>", fieldId(), n, defType(syntaxTypeToAType(t)));
      }
      else if ((TypedId) `<Id n>` := v) {
        c.define("<n>", fieldId(), n, defType(unknownType()));
        c.report(warning(v, "Field <n> has no type annotation"));
      }
    }
  c.leaveScope(current);
}

// TypedId collectors removed - handled inline in context

// =======================
// Function collection
// =======================
void collect(current: (FunctionDef) `function <Id name> ( <{Id ","}* params> ) do <Statement* body> end <Id endName>`, Collector c) {
  c.define("<name>", functionId(), name, defType(unknownType()));
  c.enterScope(current);
    for (p <- params) {
      c.define("<p>", variableId(), p, defType(unknownType()));
    }
    for (s <- body) {
      collect(s, c);
    }
  c.leaveScope(current);
  if ("<name>" != "<endName>") {
    c.report(error(current, "Function end name mismatch"));
  }
}

// =======================
// Statement collection
// =======================
void collect(current: (Statement) `<TypedId tid> = <Expression val>`, Collector c) {
  collect(val, c);
  if ((TypedId) `<Id n> : <Type t>` := tid) {
    validateFieldType(t, tid, c);
    c.define("<n>", variableId(), n, defType(syntaxTypeToAType(t)));
    c.requireEqual(syntaxTypeToAType(t), val, error(val, "Type mismatch: expected %t, got %t", syntaxTypeToAType(t), val));
  }
  else if ((TypedId) `<Type t> <Id n>` := tid) {
    validateFieldType(t, tid, c);
    c.define("<n>", variableId(), n, defType(syntaxTypeToAType(t)));
    c.requireEqual(syntaxTypeToAType(t), val, error(val, "Type mismatch: expected %t, got %t", syntaxTypeToAType(t), val));
  }
  else if ((TypedId) `<Id n>` := tid) {
    c.define("<n>", variableId(), n, defType(unknownType()));
    c.report(warning(tid, "Variable <n> has no type annotation"));
  }
}

void collect(current: (Statement) `<Type typeAnn> <Id varName> = <Expression val>`, Collector c) {
  validateFieldType(typeAnn, current, c);
  AType atype = syntaxTypeToAType(typeAnn);
  c.define("<varName>", variableId(), varName, defType(atype));
  collect(val, c);
  c.requireEqual(atype, val, error(val, "Type mismatch: expected %t, got %t", atype, val));
}

void collect(current: (Statement) `<ConditionalStmt ifs>`, Collector c) {
  collect(ifs, c);
}

void collect(current: (Statement) `<LoopStmt loop>`, Collector c) {
  collect(loop, c);
}

void collect(current: (Statement) `<Invocation inv>`, Collector c) {
  collect(inv, c);
}

void collect(current: (Statement) `<TypedId varName> = iterator ( <{Id ","}* inVars> ) yielding ( <{Id ","}* outVars> )`, Collector c) {
  collect(varName, c);
}

void collect(current: (Statement) `<TypedId varName> = from <Expression fromP> to <Expression toP>`, Collector c) {
  collect(varName, c);
  collect(fromP, c);
  collect(toP, c);
}

void collect(current: (Statement) `from <Expression fromP> to <Expression toP>`, Collector c) {
  collect(fromP, c);
  collect(toP, c);
}

// =======================
// ConditionalStmt collection
// =======================
void collect(current: (ConditionalStmt) `<IfStmt ifStmt>`, Collector c) {
  collect(ifStmt, c);
}

void collect(current: (ConditionalStmt) `<CondStmt condStmt>`, Collector c) {
  collect(condStmt, c);
}

void collect(current: (IfStmt) `if <Expression cond> then <Statement+ thenBlock> end`, Collector c) {
  collect(cond, c);
  c.requireEqual(boolType(), cond, error(cond, "Condition must be Bool, got %t", cond));
  c.enterScope(current);
    for (s <- thenBlock) collect(s, c);
  c.leaveScope(current);
}

void collect(current: (CondStmt) `cond <Expression cond> do <CondClause+ clauses> end`, Collector c) {
  collect(cond, c);
  for (clause <- clauses) {
    collect(clause, c);
  }
}

void collect(current: (CondClause) `<Expression cond> -\> <Statement+ body>`, Collector c) {
  collect(cond, c);
  c.requireEqual(boolType(), cond, error(cond, "Condition must be Bool, got %t", cond));
  c.enterScope(current);
    for (s <- body) collect(s, c);
  c.leaveScope(current);
}

// =======================
// LoopStmt collection
// =======================
void collect(current: (LoopStmt) `for <Id var> from <Expression fromExpr> to <Expression toExpr> do <Statement* body> end`, Collector c) {
  collect(fromExpr, c);
  collect(toExpr, c);
  c.requireEqual(intType(), fromExpr, error(fromExpr, "Range start must be Int, got %t", fromExpr));
  c.requireEqual(intType(), toExpr, error(toExpr, "Range end must be Int, got %t", toExpr));
  c.enterScope(current);
    c.define("<var>", variableId(), var, defType(intType()));
    for (s <- body) collect(s, c);
  c.leaveScope(current);
}

void collect(current: (LoopStmt) `for <Id var> in <Expression expr> do <Statement* body> end`, Collector c) {
  collect(expr, c);
  c.enterScope(current);
    c.define("<var>", variableId(), var, defType(unknownType()));
    for (s <- body) collect(s, c);
  c.leaveScope(current);
}

// =======================
// Invocation collection
// =======================
void collect(current: (Invocation) `<Id name> $ ( <{Id ","}* vars> )`, Collector c) {
  // Don't enforce resolution of function names
}

void collect(current: (Invocation) `<Id recv> . <Id method> ( <{Id ","}* vars> )`, Collector c) {
  // Don't enforce resolution of receiver variable
}

// =======================
// Expression collection
// =======================
void collect(current: (Expression) `<OrExpr expr>`, Collector c) {
  collect(expr, c);
}

void collect(current: (OrExpr) `<AndExpr expr>`, Collector c) {
  collect(expr, c);
}

void collect(current: (OrExpr) `<OrExpr left> or <AndExpr right>`, Collector c) {
  c.fact(current, boolType());
  collect(left, c);
  collect(right, c);
  c.requireEqual(boolType(), left, error(left, "Operand must be Bool, got %t", left));
  c.requireEqual(boolType(), right, error(right, "Operand must be Bool, got %t", right));
}

void collect(current: (AndExpr) `<CmpExpr expr>`, Collector c) {
  collect(expr, c);
}

void collect(current: (AndExpr) `<AndExpr left> and <CmpExpr right>`, Collector c) {
  c.fact(current, boolType());
  collect(left, c);
  collect(right, c);
  c.requireEqual(boolType(), left, error(left, "Operand must be Bool, got %t", left));
  c.requireEqual(boolType(), right, error(right, "Operand must be Bool, got %t", right));
}

void collect(current: (CmpExpr) `<AddExpr expr>`, Collector c) {
  collect(expr, c);
}

void collect(current: (CmpExpr) `<AddExpr left> <CmpOp op> <AddExpr right>`, Collector c) {
  c.fact(current, boolType());
  collect(left, c);
  collect(right, c);
  c.requireEqual(intType(), left, error(left, "Comparison operand must be Int, got %t", left));
  c.requireEqual(intType(), right, error(right, "Comparison operand must be Int, got %t", right));
}

void collect(current: (AddExpr) `<MulExpr expr>`, Collector c) {
  collect(expr, c);
}

void collect(current: (AddExpr) `<AddExpr left> <AddOp op> <MulExpr right>`, Collector c) {
  c.fact(current, intType());
  collect(left, c);
  collect(right, c);
  c.requireEqual(intType(), left, error(left, "Operand must be Int, got %t", left));
  c.requireEqual(intType(), right, error(right, "Operand must be Int, got %t", right));
}

void collect(current: (MulExpr) `<PowExpr expr>`, Collector c) {
  collect(expr, c);
}

void collect(current: (MulExpr) `<MulExpr left> <MulOp op> <PowExpr right>`, Collector c) {
  c.fact(current, intType());
  collect(left, c);
  collect(right, c);
  c.requireEqual(intType(), left, error(left, "Operand must be Int, got %t", left));
  c.requireEqual(intType(), right, error(right, "Operand must be Int, got %t", right));
}

void collect(current: (PowExpr) `<UnaryExpr expr>`, Collector c) {
  collect(expr, c);
}

void collect(current: (PowExpr) `<UnaryExpr left> ** <PowExpr right>`, Collector c) {
  c.fact(current, intType());
  collect(left, c);
  collect(right, c);
  c.requireEqual(intType(), left, error(left, "Operand must be Int, got %t", left));
  c.requireEqual(intType(), right, error(right, "Operand must be Int, got %t", right));
}

void collect(current: (UnaryExpr) `<Postfix postfixExpr>`, Collector c) {
  collect(postfixExpr, c);
}

void collect(current: (UnaryExpr) `neg <UnaryExpr operand>`, Collector c) {
  c.fact(current, boolType());
  collect(operand, c);
  c.requireEqual(boolType(), operand, error(operand, "Operand must be Bool, got %t", operand));
}

void collect(current: (UnaryExpr) `- <UnaryExpr operand>`, Collector c) {
  c.fact(current, intType());
  collect(operand, c);
  c.requireEqual(intType(), operand, error(operand, "Operand must be Int, got %t", operand));
}

void collect(current: (Postfix) `<Primary primaryExpr>`, Collector c) {
  collect(primaryExpr, c);
}

void collect(current: (Primary) `( <Expression expr> )`, Collector c) {
  collect(expr, c);
}

void collect(current: (Primary) `<Literal lit>`, Collector c) {
  collect(lit, c);
}

void collect(current: (Primary) `<Id name>`, Collector c) {
  c.use("<name>", {variableId()});
}

void collect(current: (Primary) `<ConstructorCall ctor>`, Collector c) {
  collect(ctor, c);
}

void collect(current: (Primary) `<Invocation inv>`, Collector c) {
  collect(inv, c);
}

// =======================
// Literal collection
// =======================
void collect(current: (Literal) `<Float realValue>`, Collector c) {
  c.fact(current, floatType());
}

void collect(current: (Literal) `<Integer intValue>`, Collector c) {
  c.fact(current, intType());
}

void collect(current: (Literal) `<BooleanLit boolValue>`, Collector c) {
  c.fact(current, boolType());
}

void collect(current: (Literal) `<Char charValue>`, Collector c) {
  c.fact(current, charType());
}

void collect(current: (Literal) `<String strValue>`, Collector c) {
  c.fact(current, stringType());
}

// =======================
// ConstructorCall collection
// =======================
void collect(current: (ConstructorCall) `sequence [ <{Expression ","}* items> ]`, Collector c) {
  for (e <- items) collect(e, c);
}

void collect(current: (ConstructorCall) `tuple ( <{Expression ","}* items> )`, Collector c) {
  for (e <- items) collect(e, c);
}

void collect(current: (ConstructorCall) `struct ( <{Expression ","}* args> )`, Collector c) {
  for (e <- args) collect(e, c);
}

public TModel typeCheck(Program p) {
  return collectAndSolve(p, config());
}

public TModel collectAndSolve(Tree pt) {
  start[Program] parsed = pt;
  return collectAndSolve(parsed.top, config());
}
