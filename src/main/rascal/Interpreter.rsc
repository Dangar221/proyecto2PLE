module Interpreter

import AST; 
import IO;
import Map;
import util::Math;

alias Env = map[str, value];

public void evalProgram(Program p) {
  Env env = ();
  for (m <- p.modules) {
    evalModule(m, env);
  }
}

public void evalModule(Module m, Env env) {
  switch (m) {
    case funcDef(f): evalFunction(f, env);
    case dataDecl(_): println("Data declaration parsed (no runtime yet).");
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

// =======================
// Helper: extraer nombre de TypedId
// =======================
str getTypedIdName(TypedId tid) {
  switch(tid) {
    case typedId(n, _): return n;
    case untypedId(n): return n;
  }
  return "";
}

//Evaluación de Sentencias
public Env evalStatement(Statement s, Env env) {
  switch (s) {
    case assignStmt(varName, val): { 
      str name = getTypedIdName(varName);  // Extraer el nombre
      value v = evalExpression(val, env);
      env += (name: v);
      println("  <name> = <v>");
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
    
    case iteratorStmt(varName, inVars, outVars): {
      str name = getTypedIdName(varName);  // Extraer el nombre
      env += (name: [inVars, outVars]);
      println("  iterator <name>(<inVars>) yielding (<outVars>)");
      return env;
    }
    
    case rangeStmtWithVar(varName, fromP, toP): {
      str name = getTypedIdName(varName);  // Extraer el nombre
      value fromV = evalExpression(fromP, env);  // Cambiar a Expression
      value toV = evalExpression(toP, env);      // Cambiar a Expression
      env += (name: [fromV, toV]);
      println("  range <name> = from <fromV> to <toV>");
      return env;
    }
    
    case rangeStmtBare(fromP, toP): {
      value fromV = evalExpression(fromP, env);  // Cambiar a Expression
      value toV = evalExpression(toP, env);      // Cambiar a Expression
      println("  range from <fromV> to <toV>");
      return env;
    }
    
    default: {
      println("Statement not implemented: <s>");
      return env;
    }
  }
}

// ELIMINAR esta función - ya no se usa Principal
// public value principalValue(Principal p, Env env) { ... }

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

public Env evalLoop(LoopStmt l, Env env) {
  switch (l) {
    case forRange(v, fromExpr, toExpr, body): {
      int startVal = asInt(evalExpression(fromExpr, env));
      int endVal = asInt(evalExpression(toExpr, env));
      Env loopEnv = env; 
      
      for (i <- [startVal .. endVal]) {
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

//Evaluación de Expresiones
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
      if (name == "tuple" || name == "struct") {
        list[value] out = [];
        for (a <- args) out += evalExpression(a, env);
        return out;
      }
      println("Function call not yet implemented");
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
    case ctorExpr(ctor):
      return evalConstructorCall(ctor, env);
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

public value evalFunctionCall(FunctionCall _call, Env _env) {
  println("Function call not implemented");
  return 0;
}

//Evaluación de Literales
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

//Utilidades de Conversión de Tipos
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
