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

// =======================
// IdRole definitions
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
TypePalConfig myConfig() = tconfig(
    isAcceptableSimple = bool (loc _, AType _) { return true; },
    isAcceptableLub = bool (loc _, AType _, AType _) { return true; },
    getLub = AType (loc _, AType t1, AType t2) { 
        if (t1 == t2) return t1;
        if (t1 == floatType() || t2 == floatType()) return floatType();
        if (t1 == intType() || t2 == intType()) return intType();
        return unknownType();
    }
);

// =======================
// Entry point
// =======================
void collect(current: (Program) `<Module+ modules>`, Collector c) {
  c.enterScope(current);
  for (m <- modules) {
    collect(m, c);
  }
  c.leaveScope(current);
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
void collect(current: (Data) `<Id assignName> : <Type dataType> = data with <{TypedId ","}+ vars> <DataBody body> end <Id endName>`, Collector c) {
  AType dtype = syntaxTypeToAType(dataType);
  c.define("<assignName>", dataId(), assignName, defType(customType("<assignName>")));
  
  c.enterScope(current);
    set[str] definedFields = {};
    for (v <- vars) {
      if ((TypedId) `<Id n> : <Type t>` := v) {
        c.define("<n>", fieldId(), n, defType(syntaxTypeToAType(t)));
        definedFields += "<n>";
      }
      else if ((TypedId) `<Type t> <Id n>` := v) {
        c.define("<n>", fieldId(), n, defType(syntaxTypeToAType(t)));
        definedFields += "<n>";
      }
      else if ((TypedId) `<Id n>` := v) {
        c.define("<n>", fieldId(), n, defType(unknownType()));
        definedFields += "<n>";
      }
    }
    
    // Validar campos en constructor
    if ((DataBody) `<Constructor cons>` := body) {
      if ((Constructor) `<Id cname> = struct ( <{TypedId ","}+ cvars> )` := cons) {
        for (cv <- cvars) {
          str fieldName = "";
          if ((TypedId) `<Id n> : <Type t>` := cv) fieldName = "<n>";
          else if ((TypedId) `<Type t> <Id n>` := cv) fieldName = "<n>";
          else if ((TypedId) `<Id n>` := cv) fieldName = "<n>";
          
          if (fieldName != "" && fieldName notin definedFields) {
            c.report(error(cv, "Field \'<fieldName>\' not defined in data structure"));
          }
        }
      }
    }
    
    collect(body, c);
  c.leaveScope(current);
  
  if ("<assignName>" != "<endName>") {
    c.report(error(current, "Data definition end name mismatch"));
  }
}

void collect(current: (Data) `data <Type dataType> <Id name> with <{TypedId ","}+ vars> <DataBody body> end <Id endName>`, Collector c) {
  c.define("<name>", dataId(), name, defType(customType("<name>")));
  
  c.enterScope(current);
    set[str] definedFields = {};
    for (v <- vars) {
      if ((TypedId) `<Id n> : <Type t>` := v) {
        c.define("<n>", fieldId(), n, defType(syntaxTypeToAType(t)));
        definedFields += "<n>";
      }
      else if ((TypedId) `<Type t> <Id n>` := v) {
        c.define("<n>", fieldId(), n, defType(syntaxTypeToAType(t)));
        definedFields += "<n>";
      }
      else if ((TypedId) `<Id n>` := v) {
        c.define("<n>", fieldId(), n, defType(unknownType()));
        definedFields += "<n>";
      }
    }
    
    if ((DataBody) `<Constructor cons>` := body) {
      if ((Constructor) `<Id cname> = struct ( <{TypedId ","}+ cvars> )` := cons) {
        for (cv <- cvars) {
          str fieldName = "";
          if ((TypedId) `<Id n> : <Type t>` := cv) fieldName = "<n>";
          else if ((TypedId) `<Type t> <Id n>` := cv) fieldName = "<n>";
          else if ((TypedId) `<Id n>` := cv) fieldName = "<n>";
          
          if (fieldName != "" && fieldName notin definedFields) {
            c.report(error(cv, "Field \'<fieldName>\' not defined in data structure"));
          }
        }
      }
    }
    
    collect(body, c);
  c.leaveScope(current);
  
  if ("<name>" != "<endName>") {
    c.report(error(current, "Data definition end name mismatch"));
  }
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
        c.use(n, {fieldId()});
      }
      else if ((TypedId) `<Type t> <Id n>` := v) {
        c.use(n, {fieldId()});
      }
      else if ((TypedId) `<Id n>` := v) {
        c.use(n, {fieldId()});
      }
    }
  c.leaveScope(current);
}

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
void collect(current: (Statement) `<Id lhs> = <Expression val>`, Collector c) {
  collect(val, c);
  c.define("<lhs>", variableId(), lhs, defType(unknownType()));
}

void collect(current: (Statement) `<Type typeAnn> <Id varId> = <Expression val>`, Collector c) {
  AType atype = syntaxTypeToAType(typeAnn);
  c.define("<varId>", variableId(), varId, defType(atype));
  collect(val, c);
  c.requireEqual(atype, val, error(val, "Type mismatch: expected <prettyAType(atype)>, got %t", val));
}

void collect(current: (Statement) `<Id varId> : <Type typeAnn> = <Expression val>`, Collector c) {
  AType atype = syntaxTypeToAType(typeAnn);
  c.define("<varId>", variableId(), varId, defType(atype));
  collect(val, c);
  c.requireEqual(atype, val, error(val, "Type mismatch: expected <prettyAType(atype)>, got %t", val));
}

void collect(current: (Statement) `<ConditionalStmt ifs>`, Collector c) {
  collect(ifs, c);
}

void collect(current: (Statement) `<LoopStmt loop>`, Collector c) {
  collect(loop, c);
}

void collect(current: (Statement) `<Invocation inv>`, Collector c) {
  // No validation for invocations
}

void collect(current: (Statement) `<Id varName> = iterator ( <{Id ","}* inVars> ) yielding ( <{Id ","}* outVars> )`, Collector c) {
  c.define("<varName>", variableId(), varName, defType(unknownType()));
}

void collect(current: (Statement) `<Id varName> = from <Expression fromP> to <Expression toP>`, Collector c) {
  c.define("<varName>", variableId(), varName, defType(unknownType()));
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

void collect(current: (IfStmt) `if <Expression cond> then <Statement* thenBlock> end`, Collector c) {
  collect(cond, c);
  c.requireEqual(boolType(), cond, error(cond, "Condition must be Bool"));
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
  c.requireEqual(boolType(), cond, error(cond, "Condition must be Bool"));
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
  c.requireEqual(intType(), fromExpr, error(fromExpr, "Range start must be Int"));
  c.requireEqual(intType(), toExpr, error(toExpr, "Range end must be Int"));
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
  // No validation
}

void collect(current: (Invocation) `<Id recv> . <Id method> ( <{Id ","}* vars> )`, Collector c) {
  // No validation
}

// =======================
// Expression collection
// =======================
void collect(current: (Expression) `<OrExpr expr>`, Collector c) {
  collect(expr, c);
  c.fact(current, expr);
}

void collect(current: (OrExpr) `<AndExpr expr>`, Collector c) {
  collect(expr, c);
  c.fact(current, expr);
}

void collect(current: (OrExpr) `<OrExpr left> or <AndExpr right>`, Collector c) {
  collect(left, c);
  collect(right, c);
  c.requireEqual(boolType(), left, error(left, "Operand must be Bool"));
  c.requireEqual(boolType(), right, error(right, "Operand must be Bool"));
  c.fact(current, boolType());
}

void collect(current: (AndExpr) `<CmpExpr expr>`, Collector c) {
  collect(expr, c);
  c.fact(current, expr);
}

void collect(current: (AndExpr) `<AndExpr left> and <CmpExpr right>`, Collector c) {
  collect(left, c);
  collect(right, c);
  c.requireEqual(boolType(), left, error(left, "Operand must be Bool"));
  c.requireEqual(boolType(), right, error(right, "Operand must be Bool"));
  c.fact(current, boolType());
}

void collect(current: (CmpExpr) `<AddExpr expr>`, Collector c) {
  collect(expr, c);
  c.fact(current, expr);
}

void collect(current: (CmpExpr) `<AddExpr left> <CmpOp op> <AddExpr right>`, Collector c) {
  collect(left, c);
  collect(right, c);
  c.fact(current, boolType());
}

void collect(current: (AddExpr) `<MulExpr expr>`, Collector c) {
  collect(expr, c);
  c.fact(current, expr);
}

void collect(current: (AddExpr) `<AddExpr left> <AddOp op> <MulExpr right>`, Collector c) {
  collect(left, c);
  collect(right, c);
  c.calculate("binary add", current, [left, right],
    AType(Solver s) {
      AType lt = s.getType(left);
      AType rt = s.getType(right);
      if (lt == floatType() || rt == floatType()) return floatType();
      return intType();
    }
  );
}

void collect(current: (MulExpr) `<PowExpr expr>`, Collector c) {
  collect(expr, c);
  c.fact(current, expr);
}

void collect(current: (MulExpr) `<MulExpr left> <MulOp op> <PowExpr right>`, Collector c) {
  collect(left, c);
  collect(right, c);
  c.calculate("binary mul", current, [left, right],
    AType(Solver s) {
      AType lt = s.getType(left);
      AType rt = s.getType(right);
      if (lt == floatType() || rt == floatType()) return floatType();
      return intType();
    }
  );
}

void collect(current: (PowExpr) `<UnaryExpr expr>`, Collector c) {
  collect(expr, c);
  c.fact(current, expr);
}

void collect(current: (PowExpr) `<UnaryExpr left> ** <PowExpr right>`, Collector c) {
  collect(left, c);
  collect(right, c);
  c.fact(current, intType());
}

void collect(current: (UnaryExpr) `<Postfix postfixExpr>`, Collector c) {
  collect(postfixExpr, c);
  c.fact(current, postfixExpr);
}

void collect(current: (UnaryExpr) `neg <UnaryExpr operand>`, Collector c) {
  collect(operand, c);
  c.requireEqual(boolType(), operand, error(operand, "Operand must be Bool"));
  c.fact(current, boolType());
}

void collect(current: (UnaryExpr) `- <UnaryExpr operand>`, Collector c) {
  collect(operand, c);
  c.calculate("unary minus", current, [operand],
    AType(Solver s) { return s.getType(operand); }
  );
}

void collect(current: (Postfix) `<Primary primaryExpr>`, Collector c) {
  collect(primaryExpr, c);
  c.fact(current, primaryExpr);
}

void collect(current: (Primary) `( <Expression expr> )`, Collector c) {
  collect(expr, c);
  c.fact(current, expr);
}

void collect(current: (Primary) `<Literal lit>`, Collector c) {
  collect(lit, c);
  c.fact(current, lit);
}

void collect(current: (Primary) `<Id name>`, Collector c) {
  c.use(name, {variableId()});
}

void collect(current: (Primary) `<ConstructorCall ctor>`, Collector c) {
  collect(ctor, c);
}

void collect(current: (Primary) `<Invocation inv>`, Collector c) {
  // No validation
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

// =======================
// Main entry point
// =======================
public TModel typeCheck(Program p) {
  return collectAndSolve(p, myConfig());
}

public TModel collectAndSolve(Tree pt) {
  if (pt has top) pt = pt.top;
  return collectAndSolve(pt, myConfig());
}