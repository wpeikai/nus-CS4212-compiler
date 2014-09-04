%{

%}

%token PLUS
%token MULT
%token ASSSIGN
%token <int> INTLIT
%token <string> VARID

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
	| term  	{ print_string "\n reduce term to exp; "}
;

term : term MULT atom {print_string " reduce multiplication to term; "}
	| atom  	{ print_string " reduce atom to term; "}
;

atom : INTLIT { print_string " reduce to atom:"; print_int $1; print_string "; ";}
		| VARID {print_string " reduce to atom:"; print_string ($1 ^ "; ");}
;


	