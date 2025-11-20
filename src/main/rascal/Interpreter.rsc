module Interpreter

import AST; 
import IO;
import Map;
import util::Math;

alias Env = map[str, value];

// ========================================
// FUNCIÓN PRINCIPAL - evalProgram
// ========================================
public void evalProgram(Program p) {
  Env env = ();
  for (m <- p.modules) {
    evalModule(m, env);
  }
}

public void evalModule(Module m, Env env) {
  switch (m) {
    case dataDecl(_): println("Data declaration parsed (no runtime yet).");
    case funcDef(f): evalFunction(f, env);
  }
}

public void evalFunction(FunctionDef f, Env env) {
  println("Executing function: <f.name>");
  Env localEnv = env; 
  for (s <- f.body) {
    localEnv = evalStatement(s, localEnv);
  }
  println("End of function: <f.name>");
}

// ========================================
// Evaluación de Sentencias
// ========================================
public Env evalStatement(Statement s, Env env) {
  switch (s) {
    // assignStmt(str lhs, Expression val)
    case assignStmt(lhs, val): { 
      value v = evalExpression(val, env);
      env += (lhs: v);
      println("  <lhs> = <v>");
      return env;
    }
    
    // typedAssignStmt(Type typeAnn, str varId, Expression val)
    case typedAssignStmt(typeAnn, varId, val): {
      value v = evalExpression(val, env);
      env += (varId: v);
      println("  <varId> : <typeAnn> = <v>");
      return env;
    }
    
    // colonTypedAssignStmt(str varId, Type typeAnn, Expression val)
    case colonTypedAssignStmt(varId, typeAnn, val): {
      value v = evalExpression(val, env);
      env += (varId: v);
      println("  <varId> : <typeAnn> = <v>");
      return env;
    }
    
    case conditionalStmt(i): return evalConditional(i, env);
    case loopStmt(l): return evalLoop(l, env);
    
    case funcCallStmt(call): { 
      evalFunctionCall(call, env);
      return env;
    }
    
    case invokeStmt(inv): {
      println("Invocation not executed yet: <inv>");
      return env;
    }
    
    // iteratorStmt(str iterVar, list[str] inVars, list[str] outVars)
    case iteratorStmt(iterVar, inVars, outVars): {
      env += (iterVar: [inVars, outVars]);
      println("  iterator <iterVar>(<inVars>) yielding (<outVars>)");
      return env;
    }
    
    // rangeStmtWithVar(str rangeVar, Expression fromP, Expression toP)
    case rangeStmtWithVar(rangeVar, fromP, toP): {
      value fromV = evalExpression(fromP, env);
      value toV = evalExpression(toP, env);
      env += (rangeVar: [fromV, toV]);
      println("  range <rangeVar> = from <fromV> to <toV>");
      return env;
    }
    
    case rangeStmtBare(fromP, toP): {
      value fromV = evalExpression(fromP, env);
      value toV = evalExpression(toP, env);
      println("  range from <fromV> to <toV>");
      return env;
    }
    
    default: {
      println("Statement not implemented: <s>");
      return env;
    }
  }
}

// ========================================
// Evaluación de Condicionales
// ========================================
public Env evalConditional(ConditionalStmt c, Env env) {
  switch (c) {
    case ifStmt(i): return evalIf(i, env);
    case condStmt(cs): {
      Env localEnv = env;
      for (CondClause clause <- cs.clauses) {
        if (toBool(evalExpression(clause.cond, env))) {
          localEnv = evalBlock(clause.body, localEnv); 
          return localEnv; 
        }
      }
      return env; 
    }
  }
  return env; 
}

public Env evalIf(IfStmt i, Env env) {
  if (toBool(evalExpression(i.cond, env))) {
    return evalBlock(i.thenBlock, env);
  }
  
  for (<Expression cond, list[Statement] block> <- i.elseifBlocks) {
    if (toBool(evalExpression(cond, env))) {
      return evalBlock(block, env);
    }
  }
  
  return evalBlock(i.elseBlock, env);
}

// ========================================
// Evaluación de Bucles
// ========================================
public Env evalLoop(LoopStmt l, Env env) {
  switch (l) {
    case forRange(v, fromExpr, toExpr, body): {
      int startVal = asInt(evalExpression(fromExpr, env));
      int endVal = asInt(evalExpression(toExpr, env));
      Env loopEnv = env; 
      
      for (i <- [startVal .. endVal + 1]) {
        Env iterationEnv = loopEnv + (v: i); 
        loopEnv = evalBlock(body, iterationEnv);
      }
      return loopEnv; 
    }
    
    case forIn(v, e, body): { 
      value seq = evalExpression(e, env);
      Env loopEnv = env; 
      
      if (list[value] lst := seq) {
        for (item <- lst) {
          Env iterationEnv = loopEnv + (v: item); 
          loopEnv = evalBlock(body, iterationEnv);
        }
      }
      return loopEnv;
    }
  }
  
  throw "Unknown loop type";
}

public Env evalBlock(list[Statement] body, Env env) {
  for (s <- body) {
    env = evalStatement(s, env);
  }
  return env;
}

// ========================================
// Evaluación de Expresiones
// ========================================
public value evalExpression(Expression e, Env env) {
  switch (e) {
    case orExpr(oe): return evalOrExpr(oe, env);
  }
  return 0;
}

public value evalOrExpr(OrExpr e, Env env) {
  switch (e) {
    case binaryOr(left, right): 
      return toBool(evalOrExpr(left, env)) || toBool(evalAndExpr(right, env));
    case andExpr(ae):
      return evalAndExpr(ae, env);
  }
  return 0;
}

public value evalAndExpr(AndExpr e, Env env) {
  switch (e) {
    case binaryAnd(left, right):
      return toBool(evalAndExpr(left, env)) && toBool(evalCmpExpr(right, env));
    case cmpExpr(ce):
      return evalCmpExpr(ce, env);
  }
  return 0;
}

public value evalCmpExpr(CmpExpr e, Env env) {
  switch (e) {
    case binaryExpr(left, op, right): {
      value lv = evalAddExpr(left, env);
      value rv = evalAddExpr(right, env);
      switch (op) {
        case "\<": return toNum(lv) < toNum(rv);
        case "\>": return toNum(lv) > toNum(rv);
        case "\<=": return toNum(lv) <= toNum(rv);
        case "\>=": return toNum(lv) >= toNum(rv);
        case "=": return lv == rv;
        case "\<\>": return lv != rv;
        default: return false;
      }
    }
    case addExpr(ae):
      return evalAddExpr(ae, env);
  }
  return 0;
}

public value evalAddExpr(AddExpr e, Env env) {
  switch (e) {
    case binaryAdd(left, op, right): {
      value lv = evalAddExpr(left, env);
      value rv = evalMulExpr(right, env);
      switch (op) {
        case "+": return toNum(lv) + toNum(rv);
        case "-": return toNum(lv) - toNum(rv);
        default: return 0;
      }
    }
    case mulExpr(me):
      return evalMulExpr(me, env);
  }
  return 0;
}

public value evalMulExpr(MulExpr e, Env env) {
  switch (e) {
    case binaryMul(left, op, right): {
      value lv = evalMulExpr(left, env);
      value rv = evalPowExpr(right, env);
      switch (op) {
        case "*": return toNum(lv) * toNum(rv);
        case "/": return toNum(lv) / toNum(rv);
        case "%": return asInt(lv) % asInt(rv);
        default: return 0;
      }
    }
    case powExpr(pe):
      return evalPowExpr(pe, env);
  }
  return 0;
}

public value evalPowExpr(PowExpr e, Env env) {
  switch (e) {
    case binaryPow(left, right): {
      int base = asInt(evalUnaryExpr(left, env));
      int exp = asInt(evalPowExpr(right, env));
      return pow(base, exp);
    }
    case unaryExpr(ue):
      return evalUnaryExpr(ue, env);
  }
  return 0;
}

public value evalUnaryExpr(UnaryExpr e, Env env) {
  switch (e) {
    case unaryNeg(operand):
      return !toBool(evalUnaryExpr(operand, env));
    case unaryMinus(operand):
      return -toNum(evalUnaryExpr(operand, env));
    case postfix(postfixExpr):
      return evalPostfix(postfixExpr, env);
  }
  return 0;
}

public value evalPostfix(Postfix e, Env env) {
  switch (e) {
    case postfixCall(primary(varExpr(name)), args): {
      if (name == "tuple" || name == "struct" || name == "sequence") {
        list[value] out = [];
        for (a <- args) out += evalExpression(a, env);
        return out;
      }
      println("Function call not yet implemented: <name>");
      return 0;
    }
    case primary(primaryExpr):
      return evalPrimary(primaryExpr, env);
  }
  return 0;
}

public value evalPrimary(Primary e, Env env) {
  switch (e) {
    case literalExpr(lit):
      return evalLiteral(lit);
    case varExpr(name):
      return (name in env) ? env[name] : 0;
    case groupExpr(expr):
      return evalExpression(expr, env);
    case ctorExpr(ctor): {
      return evalConstructorCall(ctor, env);
    }
    case invExpr(inv): {
      println("Invocation in expression: <inv>");
      return 0;
    }
  }
  return 0;
}

public value evalConstructorCall(ConstructorCall ctor, Env env) {
  list[value] resultList = [];
  for (argExpr <- ctor.args) {
    resultList += evalExpression(argExpr, env);
  }
  return resultList;
}

public value evalFunctionCall(FunctionCall call, Env env) {
  println("Function call not implemented: <call.name>()");
  return 0;
}

// ========================================
// Evaluación de Literales
// ========================================
public value evalLiteral(Literal l) {
  switch (l) {
    case intLit(intValue): return intValue;
    case floatLit(realValue): return realValue;
    case boolLit(boolValue): return boolValue;
    case charLit(charValue): return charValue;
    case stringLit(strValue): return strValue;
  }
  throw "Unknown literal type";
}

// ========================================
// Utilidades de Conversión de Tipos
// ========================================
public bool toBool(value v) {
  if (bool b := v) return b;
  if (int i := v) return i != 0;
  if (real r := v) return r != 0.0;
  return false;
}

public num toNum(value v) {
  if (int i := v) return i;
  if (real r := v) return r;
  if (bool b := v) return b ? 1 : 0;
  return 0;
}

public int asInt(value v) {
  if (int i := v) return i;
  if (real r := v) return toInt(r);
  if (bool b := v) return b ? 1 : 0;
  return 0;
}

public int toInt(real r) {
  return floor(r); 
}