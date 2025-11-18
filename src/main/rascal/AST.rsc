module AST

// Program
data Program = program(list[Module] modules);

// Módulo: funciones o declaraciones de datos
data Module
  = funcDef(FunctionDef func)
  | dataDecl(DataDecl decl)
  ;

// Tipos del lenguaje (para anotaciones explícitas)
data Type
  = tInt()
  | tBool()
  | tChar()
  | tString()
  | tFloat()
  | tUser(str name)    // tipo definido por el usuario (nombre)
  ;

// Identificadores con/ sin tipo
data TypedId
  = typedId(str name, Type typeAnn)
  | untypedId(str name)
  ;

// Data declarations - VERSIÓN ACTUALIZADA
data DataDecl
  = dataNoAssign(list[TypedId] fields, DataBody body, str endName)
  | dataWithAssign(str assignName, list[TypedId] fields, DataBody body, str endName)
  ;

data DataBody
  = consBody(ConstructorDef cons)
  | funcBody(FunctionDef func)
  ;

data ConstructorDef = constructorDef(str name, list[TypedId] fields);

// Definición de función
data FunctionDef = functionDef(
  str name,
  list[str] params,
  list[Statement] body,
  str endName
);

// Literales
data Literal
  = intLit(int intValue)
  | floatLit(real realValue)
  | boolLit(bool boolValue)
  | charLit(str charValue)
  | stringLit(str strValue)
  ;

// Expresiones (jerarquía)
data Expression = orExpr(OrExpr expr);

data OrExpr
  = binaryOr(OrExpr left, AndExpr right)
  | andExpr(AndExpr expr);

data AndExpr
  = binaryAnd(AndExpr left, CmpExpr right)
  | cmpExpr(CmpExpr expr);

data CmpExpr
  = binaryExpr(AddExpr left, str op, AddExpr right)
  | addExpr(AddExpr expr);

data AddExpr
  = binaryAdd(AddExpr left, str op, MulExpr right)
  | mulExpr(MulExpr expr);

data MulExpr
  = binaryMul(MulExpr left, str op, PowExpr right)
  | powExpr(PowExpr expr);

data PowExpr
  = binaryPow(UnaryExpr left, PowExpr right)
  | unaryExpr(UnaryExpr expr);

data UnaryExpr
  = unaryNeg(UnaryExpr operand)
  | unaryMinus(UnaryExpr operand)
  | postfix(Postfix postfixExpr);

data Postfix
  = postfixCall(Postfix callee, list[Expression] args)
  | primary(Primary primaryExpr);

data Primary
  = literalExpr(Literal lit)
  | varExpr(str name)
  | groupExpr(Expression expr)
  | ctorExpr(ConstructorCall ctor)
  | invExpr(Invocation inv)
  ;

// Invocaciones y llamadas
data FunctionCall = funcCall(str name, list[Expression] args);

data Invocation
  = dollarInvoke(str name, list[str] vars)
  | methodInvoke(str recv, str method, list[str] vars)
  ;

// Construcciones (sequence/tuple/struct) -> representadas como ConstructorCall en AST
data ConstructorCall
  = ctorCall(list[Expression] args)
  ;

// Named arg
data NamedArg = namedArg(str name, Expression expr);

// Sentencias / statements
data ConditionalStmt
  = ifStmt(IfStmt ifs)
  | condStmt(CondStmt cond)
  ;

data IfStmt = ifStmt(
  Expression cond,
  list[Statement] thenBlock,
  list[tuple[Expression, list[Statement]]] elseifBlocks,
  list[Statement] elseBlock
);

data CondStmt = condStmt(
  Expression cond,
  list[CondClause] clauses
);

data CondClause = condClause(
  Expression cond,
  list[Statement] body
);

data LoopStmt
  = forRange(str var, Expression fromExpr, Expression toExpr, list[Statement] body)
  | forIn(str var, Expression expr, list[Statement] body)
  ;

data Statement
  = assignStmt(TypedId varName, Expression val)
  | funcCallStmt(FunctionCall call)
  | conditionalStmt(ConditionalStmt ifs)
  | loopStmt(LoopStmt loop)
  | invokeStmt(Invocation inv)
  | iteratorStmt(TypedId varName, list[str] inVars, list[str] outVars)  
  | rangeStmtWithVar(TypedId varName, Expression fromP, Expression toP) 
  | rangeStmtBare(Expression fromP, Expression toP)
  ;
