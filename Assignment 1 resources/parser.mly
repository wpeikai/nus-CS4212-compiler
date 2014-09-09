%{

%}


%token <float> NUM
%token <int> INTLIT
%token <string> VARID


%token NUM
%token INTLIT
%token VARID
%token ASSSIGN
%token BOOLEAN_LITERAL
%token IDENTIFIER
%token CLASSNAME
%token EQ
%token NEWLINE
%token LPAREN RPAREN
%token LBRACE RBRACE
%token OPEN_BRACKET CLOSE_BRACKET
%token CNAME
%token PLUS MINUS MULTIPLY DIVIDE CARET
%token CLASS_KEYWORD
%token VOID_KEYWORD
%token MAIN_KEYWORD
%token EXCLAMATION_POINT
%token QUESTION_POINT
%token SEMICOLON
%token DOT
%token NULL
%token LOWER_OP
%token LOWER_EQ_OP
%token HIGHER_OP
%token HIGHER_EQ_OP
%left NEG /* negation -- unary minus */
%left PLUS MINUS
%left MULT DIV
%right CARET /* exponentiation */
%right EQ /* assignment */

%start stmt
%type <unit> stmt
%%

stmt: 
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