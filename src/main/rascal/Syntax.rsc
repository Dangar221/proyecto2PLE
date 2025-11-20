module Syntax

// Programa y módulos
start syntax Program = program: Module+ modules ; 

syntax Module
  = funcDef: FunctionDef
  | dataDecl: Data
  ; 

syntax Type
  = tInt: "Int"
  | tBool: "Bool"
  | tChar: "Char"
  | tString: "String"
  | tFloat: "Float"
  ;

syntax TypedId
  = typedId: Id name ":" Type typeAnn
  | typedIdPrefix: Type typeAnn Id name
  | untypedId: Id name                    
  ;

// Data declarations - CON TIPO EXPLÍCITO
syntax Data 
  = dataWithAssign: Id assignName ":" Type dataType "=" "data" "with" {TypedId ","}+ vars DataBody body "end" Id endName
  | dataNoAssign: "data" Type dataType Id name "with" {TypedId ","}+ vars DataBody body "end" Id endName 
  ;

syntax DataBody 
  = consBody: Constructor 
  | funcBody: FunctionDef
  ;

syntax Constructor 
  = constructorDef: Id name "=" "struct" "(" {TypedId ","}+ vars ")" 
  ;

// Funciones
syntax FunctionDef =
  functionDef: "function" Id name "(" {Id ","}* params ")"
  "do" Statement* body
  "end" Id endName ; 

// Sentencias - SIN AMBIGÜEDAD
syntax Statement
  = assignStmt: Id lhs "=" Expression val                              // x = 5
  | typedAssignStmt: Type typeAnn Id varId "=" Expression val          // Int x = 5
  | colonTypedAssignStmt: Id varId ":" Type typeAnn "=" Expression val // x: Int = 5
  | conditionalStmt: ConditionalStmt ifs 
  | loopStmt: LoopStmt loop 
  | invokeStmt: Invocation inv
  | iteratorStmt: Id iterVar "=" "iterator" "(" {Id ","}* inVars ")" "yielding" "(" {Id ","}* outVars ")"
  | rangeStmtWithVar: Id rangeVar "=" "from" Expression fromP "to" Expression toP  
  | rangeStmtBare: "from" Expression fromP "to" Expression toP                          
  ;

// Invocation forms
syntax Invocation
  = dollarInvoke: Id name "$" "(" {Id ","}* vars ")"
  | methodInvoke: Id recv "." Id method "(" {Id ","}* vars ")"
  ; 

// Constructores
syntax ConstructorCall =
  ctorCall: "sequence" "[" {Expression ","}* items "]"
  | ctorCall: "tuple" "(" {Expression ","}* elements ")"
  | ctorCall: "struct" "(" {Expression ","}* args ")" ; 

// Condicionales
syntax ConditionalStmt
  = ifStmt: IfStmt
  | condStmt: CondStmt
  ; 

syntax IfStmt =
  ifStmt: "if" Expression cond "then" Statement* thenBlock
  ("elseif" Expression "then" Statement*)* elseifBlocks
  ("else" Statement* elseBlock)?
  "end" ; 

syntax CondStmt =
  condStmt: "cond" Expression cond "do" CondClause+ clauses "end" ; 

syntax CondClause = condClause: Expression cond "-\>" Statement+ body ;

// Bucles
syntax LoopStmt =
  forRange: "for" Id var "from" Expression fromExpr "to" Expression toExpr "do" Statement* body "end" 
  | forIn: "for" Id var "in" Expression expr "do" Statement* body "end"
  ; 

// Jerarquía de expresiones 
syntax Expression = orExpr: OrExpr expr ; 

syntax OrExpr
  = andExpr: AndExpr expr
  | left binaryOr: OrExpr left "or" AndExpr right
  ; 

syntax AndExpr
  = cmpExpr: CmpExpr expr
  | left binaryAnd: AndExpr left "and" CmpExpr right
  ; 

syntax CmpExpr
  = addExpr: AddExpr expr
  | non-assoc binaryExpr: AddExpr left CmpOp op AddExpr right
  ; 

lexical CmpOp = "\<" | "\>" | "\<=" | "\>=" | "\<\>" | "=" ; 

syntax AddExpr
  = mulExpr: MulExpr expr
  | left binaryAdd: AddExpr left AddOp op MulExpr right
  ; 

lexical AddOp = "+" | "-" ;

syntax MulExpr
  = powExpr: PowExpr expr
  | left binaryMul: MulExpr left MulOp op PowExpr right
  ; 

lexical MulOp = "*" | "/" | "%" ;

syntax PowExpr
  = unaryExpr: UnaryExpr expr
  | right binaryPow: UnaryExpr left "**" PowExpr right
  ; 

syntax UnaryExpr
  = postfix: Postfix postfixExpr
  | unaryNeg: "neg" UnaryExpr operand
  | unaryMinus: "-" UnaryExpr operand
  ; 

syntax Postfix
  = primary: Primary primaryExpr
  ; 

syntax Primary
  = bracket groupExpr: "(" Expression expr ")" 
  > literalExpr: Literal lit 
  > varExpr: Id name 
  | ctorExpr: ConstructorCall ctor 
  | invExpr: Invocation inv
  ; 

syntax Literal
  = boolLit: BooleanLit boolValue     
  | floatLit: Float realValue
  | intLit: Integer intValue
  | charLit: Char charValue
  | stringLit: String strValue
  ;

// Tokens Léxicos
lexical BooleanLit = "true" | "false" ;

lexical Id = ([a-zA-Z_][a-zA-Z0-9_\-]* !>> [a-zA-Z0-9_\-]) \ Reserved ;

lexical Float = [0-9]+ "." [0-9]+ ;
lexical Integer = [0-9]+ ;

lexical Char = [\'] CharContent [\'] ; 
lexical CharContent
  = "\\\\"
  | [\'][\\][\']
  | "\\n"
  | "\\t"
  | "\\r"
  | ![\'\\]
  ; 

lexical String = [\"] StringContent* [\"] ; 
lexical StringContent
  = "\\\\"
  | [\"][\\][\"]
  | "\\n"
  | "\\t"
  | "\\r"
  | ![\"\\]
  ; 

keyword Reserved =
  "data" | "with" | "rep" | "struct" | "function" | "do" | "end"
  | "if" | "then" | "elseif" | "else" | "cond" | "for" | "from" | "to" | "in"
  | "iterator" | "yielding" | "sequence" | "tuple"
  | "and" | "or" | "neg" 
  | "true" | "false"  
  | "Int" | "Bool" | "Char" | "String" | "Float" ;

layout Layout = WhitespaceOrComment* !>> [\ \t\n\r#];

lexical WhitespaceOrComment
  = [\ \t\n\r]
  | Comment
  ; 

lexical Comment = "#" ![\n\r]* ;