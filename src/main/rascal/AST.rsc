module AST

// Program
data Program = program(list[Module] modules);

// Módulo
data Module
  = funcDef(FunctionDef func)
  | dataDecl(DataDecl decl)
  ;

// Tipos
data Type
  = tInt()
  | tBool()
  | tChar()
  | tString()
  | tFloat()
  | tUser(str name)
  ;

// TypedId ya no se usa en Statement pero sí en Data
data TypedId
  = typedId(str name, Type typeAnn)
  | typedIdPrefix(Type typeAnn, str name)
  | untypedId(str name)
  ;

// Data declarations CON TIPO
data DataDecl
  = dataNoAssign(Type dataType, str name, list[TypedId] fields, DataBody body, str endName)
  | dataWithAssign(str assignName, Type dataType, list[TypedId] fields, DataBody body, str endName)
  ;

data DataBody
  = consBody(ConstructorDef cons)
  | funcBody(FunctionDef func)
  ;

data ConstructorDef = constructorDef(str name, list[TypedId] fields);

// Función
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

// Expresiones
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

data FunctionCall = funcCall(str name, list[Expression] args);

data Invocation
  = dollarInvoke(str name, list[str] vars)
  | methodInvoke(str recv, str method, list[str] vars)
  ;

data ConstructorCall = ctorCall(list[Expression] args);

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

// Statements ACTUALIZADOS
data Statement
  = assignStmt(str lhs, Expression val)                           // x = 5
  | typedAssignStmt(Type typeAnn, str varId, Expression val)      // Int x = 5
  | colonTypedAssignStmt(str varId, Type typeAnn, Expression val) // x: Int = 5
  | funcCallStmt(FunctionCall call)
  | conditionalStmt(ConditionalStmt ifs)
  | loopStmt(LoopStmt loop)
  | invokeStmt(Invocation inv)
  | iteratorStmt(str iterVar, list[str] inVars, list[str] outVars)
  | rangeStmtWithVar(str rangeVar, Expression fromP, Expression toP)
  | rangeStmtBare(Expression fromP, Expression toP)
  ;