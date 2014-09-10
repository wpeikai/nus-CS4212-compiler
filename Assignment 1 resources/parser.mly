%{
    open Jlite
%}


%token DOT
%token ASSSIGN
%token PLUS MINUS MULTIPLY DIVIDE CARET
%token RELATIVE_OPERATOR
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET
%token EXCLAMATION_POINT QUESTION_POINT EQ SEMICOLON COMMA
%token AND_OPERATOR OR_OPERATOR
%token VOID_KEYWORD MAIN_KEYWORD IF_KEYWORD ELSE_KEYWORD
%token IF_KEYWORD RETURN_KEYWORD WHILE_KEYWORD THIS_KEYWORD 
%token NEW_KEYWORD CLASS_KEYWORD READLN_KEYWORD PRINTLN_KEYWORD NULL_KEYWORD
%token BOOLEAN_LITERAL
%token IDENTIFIER
%token <string> CLASSNAME
%token INT_KEYWORD BOOL_KEYWORD STRING_KEYWORD
%token STRING_LITERAL

%token <float> NUM
%token <int> INTLIT
%token <string> VARID

%left PLUS MINUS
%left MULTIPLY DIVIDE
%right EXCLAMATION_POINT /* negation operator */
%right CARET /* exponentiation */
%right ASSSIGN /* assignment */
%right MINUS /* Negative operator */

%start program
%type <unit> program
%%

program: mainclass classdeclkleene  {print_string "mainprogram\n"}
;

classdeclkleene:    {print_string "classdeclkleeneepty\n"}
        |       classdeclkleene classdecl   {print_string "classdeclkleene classdecl\n"}
;

mainclass:      CLASS_KEYWORD CLASSNAME LBRACKET VOID_KEYWORD
                    MAIN_KEYWORD LPAREN fmllist RPAREN mdbody RBRACKET  {print_string "mainclass\n"}
;

classdecl:      CLASS_KEYWORD CLASSNAME LBRACKET vardeclkleene
                    mddeclkleene RBRACKET   {}
;

vardeclkleene: {}
        |       vardeclkleene vardecl   {}
;

mddeclkleene:  {}
        |       mddeclkleene mddecl     {}
;

vardecl:        typee IDENTIFIER SEMICOLON   {}
;

mddecl:         typee IDENTIFIER LPAREN fmllist RPAREN mdbody    {}
;

fmllist:    {}
        |       typee IDENTIFIER fmlrestkleene   {}
;

fmlrestkleene:  {}
        |       fmlrestkleene fmlrest   {}
;

fmlrest:        COMMA typee IDENTIFIER  {}
;

typee:          INT_KEYWORD {}
        |       BOOL_KEYWORD    {}
        |       VOID_KEYWORD    {}
        |       CLASSNAME   {}
;

mdbody:         LBRACKET vardeclkleene stmtpositive RBRACKET    {}
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
        |       IDENTIFIER ASSSIGN exp SEMICOLON    {}
        |       atom DOT IDENTIFIER ASSSIGN exp SEMICOLON   {}
        |       atom LPAREN explist RPAREN  {}
        |       RETURN_KEYWORD exp SEMICOLON    {}
        |       RETURN_KEYWORD SEMICOLON    {}
;

exp:            bexp    {}
        |       aexp    {}
        |       sexp    {}
;

bexp:       bexp OR_OPERATOR conj {}
        |   conj {}
;

conj:       conj AND_OPERATOR rexp {}
        |   rexp {}
;

rexp:       aexp bop aexp {}
        |   bgrd {}
;

bop:        RELATIVE_OPERATOR {}
        |   EXCLAMATION_POINT ASSSIGN {}
;

bgrd:       EXCLAMATION_POINT bgrd {}
        |   BOOLEAN_LITERAL {}
        |   atom {}
;

aexp:       aexp PLUS term {}
        |   aexp MINUS term {}
        |   term {}
;

term:       term MULTIPLY ftr {}
        |   aexp DIVIDE ftr {}
        |   ftr {}
;

ftr:        INTLIT {}
        |   MINUS ftr {}
        |   ftr {}
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

/*
program: 
    varrule ASSSIGN exp {print_string "\n reduce statement; ";}
;


varrule: 
    VARID {print_string (" shift varid:" ^ $1);} 
;

intrule :
    INTLIT { print_string " shift int "}
;

exp: exp PLUS term {print_string "\n reduce addition exp to exp; "}
    | term      { print_string "\n reduce term to exp; "}
;

term : term MULT atom {print_string " reduce multiplication to term; "}
    | atom      { print_string " reduce atom to term; "}
;

atom : INTLIT { print_string " reduce to atom:"; print_int $1; print_string "; ";}
        | VARID {print_string " reduce to atom:"; print_string ($1 ^ "; ");}
;

/*BNF pdf*/

/*program: mainclass classdecl*

mainclass: CLASS_KEYWORD CNAME OPEN_BRACKET VOID_KEYWORD MAIN_KEYWORD OPEN_PARENT fmllist CLOSE_PARENT mdbody CLOSE_BRACKET { print_string " reduce to atom:" }
    

*/	