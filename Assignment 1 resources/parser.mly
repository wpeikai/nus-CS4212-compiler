%{
    open Jlite
    open Printf
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
%token BOOLEAN_LITERAL
%token <string> IDENTIFIER
%token <string> CLASSNAME
%token INT_KEYWORD BOOL_KEYWORD STRING_KEYWORD
%token <string> STRING_LITERAL

%token UMINUS

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
%type <unit> program
%%

program: mainclass classdeclkleene  {print_string "mainprogram\n"}
;

classdeclkleene:    {print_string "class declkleene emmmmmmpty\n"}
        |       classdeclkleene classdecl   {print_string "classdeclkleene classdecl\n"}
;

mainclass:      CLASS_KEYWORD CLASSNAME LBRACKET VOID_KEYWORD
                    MAIN_KEYWORD LPAREN fmllist RPAREN mdbody RBRACKET  {printf "%s clllllllassssss\n" $2}
;

classdecl:      CLASS_KEYWORD CLASSNAME LBRACKET classbody RBRACKET   { printf "%s classdecl\n" $2}
;

classbody: {}
        |   aaaaa {}
;

aaaaa: typee IDENTIFIER ffff {}
;

ffff: SEMICOLON aaaaa {}
    | mddecl  {}
;

vardecl: typee IDENTIFIER SEMICOLON   {print_string "typpee\n"}
;

mddecl:  LPAREN fmllist RPAREN mdbody    {}
;

vardeclkeene:   {}
        |       vardeclkeene vardecl {}
;


fmllist:    {}
        |       typee IDENTIFIER fmlrestkleene   {printf "list of decl var %s %s\n" (string_of_jlite_type $1) ($2)}
;

fmlrestkleene:  {}
        |       fmlrestkleene fmlrest   {}
;

fmlrest:        COMMA typee IDENTIFIER  {}
;

typee:          INT_KEYWORD { IntT }
        |       BOOL_KEYWORD    { BoolT }
        |       STRING_KEYWORD    { StringT }
        |       VOID_KEYWORD    { VoidT }
        |       CLASSNAME   { ObjectT $1 }
;

mdbody:         LBRACKET vardeclkeene stmtpositive RBRACKET    {print_string "mdbody\n"}
;

stmtkleene:     {}
        |       stmtkleene stmt    {}
;

stmtpositive:   stmtkleene stmt  {}
;

stmt:           IF_KEYWORD LPAREN exp RPAREN LBRACKET stmtpositive RBRACKET ELSE_KEYWORD  
                    LBRACKET stmtpositive RBRACKET  {}
        |       WHILE_KEYWORD LPAREN exp RPAREN LBRACKET stmtkleene RBRACKET    {}
        |       READLN_KEYWORD LPAREN IDENTIFIER RPAREN SEMICOLON   {}
        |       PRINTLN_KEYWORD LPAREN exp RPAREN SEMICOLON     {}
        |       IDENTIFIER ASSSIGN exp SEMICOLON  {}
        |       atom DOT IDENTIFIER ASSSIGN exp SEMICOLON   {}
        |       atom LPAREN explist RPAREN SEMICOLON {}
        |       RETURN_KEYWORD exp SEMICOLON    {}
        |       RETURN_KEYWORD SEMICOLON    {}
;

exp:            bexp    {}
        |       aexp    {}
        |       sexp    {}
;

bexp:        rexp AND_OPERATOR rexp {}
        |    rexp OR_OPERATOR rexp {}


rexp:       aexp bop aexp {}
        |   bgrd {}
;

bop:        RELATIVE_OPERATOR { RelationalOp $1}
        |   EXCLAMATION_POINT ASSSIGN { BooleanOp "!="}
;

bgrd:       EXCLAMATION_POINT bgrd {}
        |   BOOLEAN_LITERAL {}
        |   atom {}
;

aexp:       ftr {}
        |   aexp PLUS aexp {}
        |   aexp MINUS aexp {}
        |   aexp MULTIPLY aexp {}
        |   aexp DIVIDE aexp {}
;

ftr:        INTLIT {}
        |   MINUS ftr {}
        |   atom {}
;

sexp:       STRING_LITERAL {}
        |   atom {}
;

atom:       atom DOT IDENTIFIER {}
        |   atom LPAREN explist RPAREN {}
        |   THIS_KEYWORD {}
        |   IDENTIFIER {}
        |   NEW_KEYWORD CLASSNAME LPAREN RPAREN {}
        |   LPAREN exp RPAREN {}
        |   NULL_KEYWORD {}
;

explist:    {}
        |   exp exprestkeene{}
;

exprestkeene: {}
        |   exprestkeene exprest {}
;

exprest:    COMMA exp {}
;
