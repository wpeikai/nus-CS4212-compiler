%{
    open Jlite
    open Printf

    let second_item (a, b) = b
    let first_item (a, b) = a
%}


%token DOT
%token ASSSIGN
%token PLUS MINUS MULTIPLY DIVIDE CARET
%token <string> RELATIVE_OPERATOR
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET
%token EXCLAMATION_POINT QUESTION_POINT EQ SEMICOLON COMMA
%token AND_OPERATOR OR_OPERATOR
%token VOID_KEYWORD MAIN_KEYWORD IF_KEYWORD ELSE_KEYWORD
%token IF_KEYWORD RETURN_KEYWORD WHILE_KEYWORD THIS_KEYWORD 
%token NEW_KEYWORD CLASS_KEYWORD READLN_KEYWORD PRINTLN_KEYWORD NULL_KEYWORD
%token <bool> BOOLEAN_LITERAL
%token <string> IDENTIFIER
%token <string> CLASSNAME
%token INT_KEYWORD BOOL_KEYWORD STRING_KEYWORD
%token <string> STRING_LITERAL

%token UMINUS
%token END_OF_FILE

%token <float> NUM
%token <int> INTLIT
%token <string> VARID


%left OR_OPERATOR
%left AND_OPERATOR

%left PLUS MINUS
%left MULTIPLY DIVIDE
%right EXCLAMATION_POINT /* negation operator */
%right CARET /* exponentiation */
%right ASSSIGN /* assignment */



%start program
%type <Jlite.jlite_program> program
%type <Jlite.class_decl> classdecl
%type <Jlite.class_main> mainclass
%type <Jlite.md_decl> mthd
%type <Jlite.jlite_stmt list> stmtpositive
%type <Jlite.var_decl list * Jlite.jlite_stmt list> mdbody
%%

program: mainclass classdeclkleene END_OF_FILE { ($1, $2) }

classdeclkleene:    { [] }
        |       classdeclkleene classdecl   { $2 :: $1 }
;

mainclass:      CLASS_KEYWORD CLASSNAME LBRACKET VOID_KEYWORD
                    MAIN_KEYWORD LPAREN fmllist RPAREN mdbody RBRACKET  { ($2, {jliteid=(SimpleVarId "main"); rettype=VoidT; params=$7; localvars=(first_item $9); stmts=(second_item $9); ir3id=(SimpleVarId $2)}) }

classdecl:      CLASS_KEYWORD CLASSNAME LBRACKET classbody RBRACKET   { ($2, (first_item $4), (second_item $4)) }
;

classbody: { ([], []) }
        |   mddecllist { ([], $1) }
        |   vardeclist mddecllist { ($1, $2) }
;

vardeclist : vardecl { $1 :: [] }
        | vardeclist vardecl { $2 :: $1 }
;

mddecllist:  { [] }
        | mthd mddecllist { $1 :: $2 }
;

mthd: typee IDENTIFIER LPAREN fmllist RPAREN mdbody {
    {jliteid=(SimpleVarId $2); rettype=$1; params=$4; localvars=(first_item $6); stmts=(second_item $6); ir3id=(SimpleVarId $2)} }

vardecl: typee IDENTIFIER SEMICOLON   { ($1, (SimpleVarId $2)) }
;

fmllist:    { [] }
        |   fmllists { $1 }
;

fmllists:   typee IDENTIFIER { ($1, (SimpleVarId $2)) :: [] }
        |   fmllists COMMA typee IDENTIFIER { ($3, (SimpleVarId $4)) :: $1 }
;

typee:          INT_KEYWORD { IntT }
        |       BOOL_KEYWORD    { BoolT }
        |       STRING_KEYWORD    { StringT }
        |       VOID_KEYWORD    { VoidT }
        |       CLASSNAME   { ObjectT $1 }
;

mdbody:         LBRACKET vardeclkeene stmtpositive RBRACKET { ($2, $3) }
;

vardeclkeene:   { [] }
        |       vardeclkeene vardecl { $2 :: $1 }
;

stmtkleene:     { [] }
        |       stmtlist    { $2 :: $1 }
;


stmtlist:   stmtlist stmt  { $2 :: $1 }


stmtpositive:   stmtpositive stmt  { $2 :: $1 }
;

stmt:           IF_KEYWORD LPAREN exp RPAREN LBRACKET stmtpositive RBRACKET ELSE_KEYWORD  
                    LBRACKET stmtpositive RBRACKET  { IfStmt ($3, $6, $10) }
        |       WHILE_KEYWORD LPAREN exp RPAREN LBRACKET stmtkleene RBRACKET    { WhileStmt ($3, $6) }
        |       READLN_KEYWORD LPAREN IDENTIFIER RPAREN SEMICOLON   { ReadStmt (SimpleVarId $3) }
        |       PRINTLN_KEYWORD LPAREN exp RPAREN SEMICOLON     { PrintStmt $3 }
        |       IDENTIFIER ASSSIGN exp SEMICOLON  { AssignStmt ((SimpleVarId $1), $3) }
        |       atom DOT IDENTIFIER ASSSIGN exp SEMICOLON   { AssignFieldStmt (FieldAccess ($1, (SimpleVarId $3)), $5) }
        |       atom LPAREN explist RPAREN SEMICOLON { MdCallStmt (MdCall ($1, $3)) } /* Error in */
        |       RETURN_KEYWORD exp SEMICOLON    { ReturnStmt $2 }
        |       RETURN_KEYWORD SEMICOLON    { ReturnVoidStmt }
;

exp:            bexp    { $1 }
        |       aexp    { $1 }
        |       sexp    { $1 }
;

bexp:       rexp { $1 }
        |   rexp AND_OPERATOR rexp { BinaryExp (BooleanOp "&&", $1, $3) }
        |   rexp OR_OPERATOR rexp { BinaryExp (BooleanOp "||", $1, $3) }


rexp:       aexp bop aexp { BinaryExp ($2, $1, $3) }
        |   bgrd { $1 }
;

bop:        RELATIVE_OPERATOR { RelationalOp $1}
        |   EXCLAMATION_POINT ASSSIGN { BooleanOp "!="}
        |   ASSSIGN ASSSIGN { BooleanOp "=="}
;

bgrd:       EXCLAMATION_POINT bgrd { UnaryExp (UnaryOp "!", $2) }
        |   BOOLEAN_LITERAL { BoolLiteral $1}
        |   atom { $1 }
;

aexp:       ftr { $1 }
        |   aexp PLUS aexp { BinaryExp (AritmeticOp "+", $1, $3) }
        |   aexp MINUS aexp { BinaryExp (AritmeticOp "-", $1, $3) }
        |   aexp MULTIPLY aexp { BinaryExp (AritmeticOp "*", $1, $3) }
        |   aexp DIVIDE aexp { BinaryExp (AritmeticOp "/", $1, $3) }
;

ftr:        INTLIT { IntLiteral $1 }
        |   MINUS %prec ftr { UnaryExp (UnaryOp "-", $2) }
        |   atom { $1 }
;

sexp:       STRING_LITERAL { StringLiteral $1 }
        |   atom { $1 }
;

atom:       atom DOT IDENTIFIER { FieldAccess ($1, (SimpleVarId $3)) }
        |   atom LPAREN explist RPAREN { MdCall ($1, $3) }
        |   THIS_KEYWORD { ThisWord }
        |   IDENTIFIER { Var (SimpleVarId $1) }
        |   NEW_KEYWORD CLASSNAME LPAREN RPAREN { ObjectCreate $2 }
        |   LPAREN exp RPAREN { $2 }
        |   NULL_KEYWORD { NullWord }
;

explist:    { [] }
        |   exprlists { $1 }
;

exprlists:  exp { $1 :: [] }
        |   exprlists COMMA exp { $3 :: $1 }
;