%{

%}


%token DOT
%token ASSSIGN
%token PLUS MINUS MULTIPLY DIVIDE CARET
%token RELATIVE_OPERATOR
%token LPAREN RPAREN
%token LBRACE RBRACE
%token OPEN_BRACKET CLOSE_BRACKET
%token EXCLAMATION_POINT QUESTION_POINT EQ SEMICOLON COLON
%token AND_OPERATOR OR_OPERATOR
%token VOID_KEYWORD MAIN_KEYWORD IF_KEYWORD ELSE_KEYWORD
%token IF_KEYWORD RETURN_KEYWORD WHILE_KEYWORD THIS_KEYWORD 
%token NEW_KEYWORD CLASS_KEYWORD READLN_KEYWORD PRINTLN_KEYWORD NULL_KEYWORD
%token BOOLEAN_LITERAL
%token IDENTIFIER
%token CLASSNAME


%token <float> NUM
%token <int> INTLIT
%token <string> VARID

%left PLUS MINUS
%left MULTIPLY DIVIDE
%right EXCLAMATION_POINT /* negation operator */
%right CARET /* exponentiation */
%right ASSSIGN /* assignment */
%right MINUS /* Negative operator */

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