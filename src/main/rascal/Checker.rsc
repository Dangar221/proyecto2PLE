module Checker
import AST;
import Syntax;
extend analysis::typepal::TypePal;

// =======================
// Type converter
// =======================
AType astTypeToAType(Type t) {
  switch(t) {
    case tInt():    return intType();
    case tBool():   return boolType();
    case tChar():   return charType();
    case tString(): return stringType();
    case tFloat():  return floatType();
    case tUser(n):  return customType(n);
  }
  return unknownType();
}

// =======================
// Config
// =======================
TypePalConfig cfg() = tconfig(
  verbose = false,
  logTModel = false
);

// =======================
// Entry point
// =======================
public TModel typeCheck(Program p) {
  Tree dummy = emptyTree();
  Collector c = newCollector("ALU-checker", dummy, cfg());
  collectProgram(p, c);
  return newSolver(dummy, c.run()).run();
}

// ======================================================
// Program
// ======================================================
void collectProgram(Program p, Collector c) {
  switch(p) {
    case program(mods):
      for (m <- mods) collectModule(m, c);
  }
}

// ======================================================
// Module
// ======================================================
void collectModule(Module m, Collector c) {
  switch(m) {
    case funcDef(fd):  collectFunction(fd, c);
    case dataDecl(d):  collectDataDecl(d, c);
  }
}

// ======================================================
// Data declarations
// ======================================================
void collectDataDecl(DataDecl d, Collector c) {
  switch(d) {

    case dataCtorNoAssign(fields, cons, endName): {
      c.define(endName, dataTypeId(), d, defType(customType(endName)))
      defineFields(fields, c);
      collectConstructor(cons, endName, c);
    }

    case dataCtorWithAssign(assignName, fields, cons, endName): {
      c.define(assignName, dataTypeId(), d, defType(customType(endName)));
      c.define(endName,     dataTypeId(), d, defType(customType(endName)));
      defineFields(fields, c);
      collectConstructor(cons, endName, c);
    }
  }
}

// ------------------------------
// Helper: define fields
// ------------------------------
void defineFields(list[TypedId] fields, Collector c) {
  for(f <- fields) {
    switch(f) {
      case typedId(n, t):
        c.define(n, fieldId(), f, defType(astTypeToAType(t)));
      case untypedId(n):
        c.define(n, fieldId(), f, defType(unknownType()));
    }
  }
}

// ------------------------------
// Constructor
// ------------------------------
void collectConstructor(ConstructorDef cons, str parentType, Collector c) {
  switch(cons) {
    case constructorDef(name, usedFields): {

      c.define(name, structId(), cons, defType(customType(parentType)));

      // Validación: cada campo usado debe haber sido declarado
      set[str] declared = 
        { n | u <- usedFields, u := typedId(n, _) }
        + { n | u <- usedFields, u := untypedId(n) };

      for (u <- usedFields) {
        switch(u) {
          case typedId(n, _):
            if (n notin declared)
              c.report(error(cons, "Field <n> not declared in <parentType>"));
          case untypedId(n):
            if (n notin declared)
              c.report(error(cons, "Field <n> not declared in <parentType>"));
        }
      }
    }
  }
}

// ======================================================
// FunctionDef
// ======================================================
void collectFunction(FunctionDef f, Collector c) {
  switch(f) {
    case functionDef(name, params, body, endName): {

      c.define(name, functionId(), f, defType(unknownType()));
      c.enterScope(f);

      for(p <- params)
        c.define(p, paramId(), p, defType(unknownType()));

      for(s <- body)
        collectStatement(s, c);

      c.leaveScope(f);

      if (name != endName)
        c.report(error(f, "Function end name mismatch (<name> != <endName>)"));
    }
  }
}

// ======================================================
// Statements
// ======================================================
void collectStatement(Statement s, Collector c) {
  switch(s) {

    case assignStmt(tid, expr):
      collectAssign(tid, expr, c);

    case funcCallStmt(fc):
      collectFuncCall(fc, c);

    case conditionalStmt(cs):
      collectConditional(cs, c);

    case loopStmt(l):
      collectLoop(l, c);

    case invokeStmt(inv):
      collectInvocation(inv, c);

    //case iteratorStmt(_, _, _):
     //;

    case rangeStmtWithVar(_, fromP, toP):
      collectExpression(fromP, c)
      collectExpression(toP, c);

    case rangeStmtBare(fromP, toP):
      collectExpression(fromP, c)
      collectExpression(toP, c);
  }
}

// ======================================================
// Assignments
// ======================================================
void collectAssign(TypedId tid, Expression expr, Collector c) {

  collectExpression(expr, c);

  switch(tid) {

    case typedId(n, t): {
      AType at = astTypeToAType(t);
      c.define(n, variableId(), tid, defType(at));
      c.requireEqual(at, expr, error(expr, "Assigned type mismatch"));
    }

    case untypedId(n): {
      c.define(n, variableId(), tid, defType(unknownType()));
    }
  }
}

// ======================================================
// FunctionCall
// ======================================================
void collectFuncCall(FunctionCall fc, Collector c) {
  for(e <- fc.args) collectExpression(e, c);
  c.use(fc.name, {functionId()});
}

// ======================================================
// Invocation
// ======================================================
void collectInvocation(Invocation inv, Collector c) {
  switch (inv) {

    case dollarInvoke(name, vars): {
      c.use(name, {functionId()});
    }

    case methodInvoke(recv, method, vars): {
      c.use(recv,   {variableId()});
      c.use(method, {functionId()});
    }
  }
}


// ======================================================
// Conditionals
// ======================================================
void collectConditional(ConditionalStmt cs, Collector c) {
  switch(cs) {

    case ifStmt(i): {
      collectIf(i, c);
    }

    case condStmt(cond): {
      collectExpression(cond.cond, c);
      for (cl <- cond.clauses) {
        collectCondClause(cl, c);
      }
    }
  }
}


// ---------------------
// If
// ---------------------
void collectIf(IfStmt i, Collector c) {
  collectExpression(i.cond, c);
  c.requireEqual(boolType(), i.cond, error(i.cond, "Condition must be boolean"));

  for(s <- i.thenBlock) collectStatement(s, c);

  for(<e, blk> <- i.elseifBlocks) {
    collectExpression(e, c);
    c.requireEqual(boolType(), e, error(e, "Condition must be boolean"));
    for(s <- blk) collectStatement(s, c);
  }

  for(s <- i.elseBlock) collectStatement(s, c);
}

// ---------------------
// CondClause
// ---------------------
void collectCondClause(CondClause cl, Collector c) {
  collectExpression(cl.cond, c);
  c.requireEqual(boolType(), cl.cond, error(cl.cond, "Condition must be boolean"));
  for(s <- cl.body) collectStatement(s, c);
}

// ======================================================
// Loops
// ======================================================
void collectLoop(LoopStmt l, Collector c) {
  switch(l) {

    case forRange(v, fromE, toE, body): {
      collectExpression(fromE, c);
      collectExpression(toE, c);

      c.requireEqual(intType(), fromE, error(fromE, "Range start must be int"));
      c.requireEqual(intType(), toE,   error(toE,   "Range end must be int"));

      c.enterScope(l);
      c.define(v, variableId(), l, defType(intType()));
      for (s <- body) {
        collectStatement(s, c);
      }
      c.leaveScope(l);
    }

    case forIn(v, e, body): {
      collectExpression(e, c);
      c.enterScope(l);
      c.define(v, variableId(), l, defType(unknownType()));
      for (s <- body) {
        collectStatement(s, c);
      }
      c.leaveScope(l);
    }
  }
}


// ======================================================
// Expressions
// ======================================================
void collectExpression(Expression e, Collector c) {
  switch(e) {
    case orExpr(oe): collectOr(oe, c);
  }
}

// ======================================================
// OR
// ======================================================
void collectOr(OrExpr e, Collector c) {
  switch(e) {

    case binaryOr(left, right): {
      collectOr(left, c);
      collectAnd(right, c);
      c.requireEqual(boolType(), e, error(e, "OR must be boolean"));
    }

    case andExpr(a): {
      collectAnd(a, c);
    }
  }
}


// ======================================================
// AND
// ======================================================
void collectAnd(AndExpr e, Collector c) {
  switch(e) {

    case binaryAnd(left, right): {
      collectAnd(left, c);
      collectCmp(right, c);
      c.requireEqual(boolType(), e, error(e, "AND must be boolean"));
    }

    case cmpExpr(ce):
      collectCmp(ce, c);
  }
}

// ======================================================
// Comparison
// ======================================================
void collectCmp(CmpExpr e, Collector c) {
  switch(e) {

    case binaryExpr(left, op, right): {
      collectAdd(left, c);
      collectAdd(right, c);
      c.requireEqual(boolType(), e, warning(e, "Comparison result assumed boolean"));
    }

    case addExpr(a):
      collectAdd(a, c);
  }
}

// ======================================================
// Add
// ======================================================
void collectAdd(AddExpr e, Collector c) {
  switch(e) {

    case binaryAdd(left, op, right): {
      collectAdd(left, c);
      collectMul(right, c);
      c.requireEqual(floatType(), e, warning(e, "Add assumed numeric"));
    }

    case mulExpr(m):
      collectMul(m, c);
  }
}

// ======================================================
// Mul
// ======================================================
void collectMul(MulExpr e, Collector c) {
  switch(e) {

    case binaryMul(left, op, right): {
      collectMul(left, c);
      collectPow(right, c);
      c.requireEqual(floatType(), e, warning(e, "Mul assumed numeric"));
    }

    case powExpr(p):
      collectPow(p, c);
  }
}

// ======================================================
// Pow
// ======================================================
void collectPow(PowExpr e, Collector c) {
  switch(e) {

    case binaryPow(left, right): {
      collectUnary(left, c);
      collectPow(right, c);
      c.requireEqual(intType(), e, warning(e, "Pow assumed integer"));
    }

    case unaryExpr(u):
      collectUnary(u, c);
  }
}

// ======================================================
// Unary
// ======================================================
void collectUnary(UnaryExpr e, Collector c) {
  switch(e) {

    case unaryNeg(op): {
      collectUnary(op, c);
      c.requireEqual(boolType(), e, error(e, "neg must be boolean"));
    }

    case unaryMinus(op): {
      collectUnary(op, c);
      c.requireEqual(floatType(), e, warning(e, "Unary minus assumed numeric"));
    }

    case postfix(p):
      collectPostfix(p, c);
  }
}

// ======================================================
// Postfix
// ======================================================
void collectPostfix(Postfix p, Collector c) {
  switch(p) {

    case postfixCall(callee, args): {
      collectPostfix(callee, c);
      for(a <- args) collectExpression(a, c);
    }

    case primary(pr):
      collectPrimary(pr, c);
  }
}

// ======================================================
// Primary
// ======================================================
void collectPrimary(Primary pr, Collector c) {
  switch(pr) {

    case literalExpr(l):
      collectLiteral(l, c);

    case varExpr(n):
      c.use(n, {variableId()});

    case groupExpr(e):
      collectExpression(e, c);

    case ctorExpr(ctor):
      collectCtorCall(ctor, c);

    case invExpr(inv):
      collectInvocation(inv, c);
  }
}

// ======================================================
// ConstructorCall
// ======================================================
void collectCtorCall(ConstructorCall ctor, Collector c) {
  for(a <- ctor.args)
    collectExpression(a, c);
}

// ======================================================
// Literals
// ======================================================
void collectLiteral(Literal lit, Collector c) {
  switch(lit) {
    case intLit(_):    c.fact(lit, intType());
    case floatLit(_):  c.fact(lit, floatType());
    case boolLit(_):   c.fact(lit, boolType());
    case charLit(_):   c.fact(lit, charType());
    case stringLit(_): c.fact(lit, stringType());
  }
}


