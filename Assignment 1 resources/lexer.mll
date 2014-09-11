{
    open Parser
    open Printf

}

let lower_alpha = ['a'-'z']
let upper_alpha = ['A'-'Z']
let digits = ['0'-'9']
let white = [' ' '\t' '\n']+
let alphanumunderscore = (lower_alpha|upper_alpha|digits|'_')
let identifier = lower_alpha(alphanumunderscore)*
let classname = upper_alpha(alphanumunderscore)*
let intexp = ['1'-'9'](digits)*
let varidexp = lower_alpha+
let string_literal = "\"" ['a'-'z']* "\""

rule token = parse
| "." { DOT }
| "=" { ASSSIGN }
| "+" { PLUS }
| "-" { MINUS }
| "*" { MULTIPLY }
| "/" { DIVIDE }
| "^" { CARET }
| "<"|"<="|">"|">=" as relative_op { RELATIVE_OPERATOR relative_op}
| "(" { LPAREN }
| ")" { RPAREN }
| "[" { LBRACE }
| "]" { RBRACE }
| "{" { LBRACKET }
| "}" { RBRACKET }
| "//" { print_endline "comments start"; commentsoneline lexbuf }
| "/*" { print_endline "comments start"; commentsmultiline lexbuf }
| "!" { EXCLAMATION_POINT }
| "?" { QUESTION_POINT }
| "=" { EQ }
| ";" { SEMICOLON }
| "," { COMMA }
| "&&" { AND_OPERATOR }
| "||" { OR_OPERATOR }
| "void" { VOID_KEYWORD }
| "main" { MAIN_KEYWORD }
| "if" { IF_KEYWORD }
| "else" { ELSE_KEYWORD }
| "return" { RETURN_KEYWORD }
| "while" { WHILE_KEYWORD }
| "this" { THIS_KEYWORD }
| "new" { NEW_KEYWORD }
| "Int" { INT_KEYWORD }
| "Bool" { BOOL_KEYWORD }
| "String" { STRING_KEYWORD }
| "class" { CLASS_KEYWORD }
| "readln" { READLN_KEYWORD }
| "println" { PRINTLN_KEYWORD }
| "null" { NULL_KEYWORD }
| "true"|"false" { BOOLEAN_LITERAL }
| string_literal as string_l { STRING_LITERAL string_l}
| identifier as id { IDENTIFIER id}
| classname as cname { CLASSNAME cname}
| intexp as s { INTLIT (int_of_string s) }
| varidexp as v { VARID v}
| white { token lexbuf } (* ignore white space *)
| eof { exit 0}

and commentsoneline = parse
  | "\n" { print_endline "commentsoneline close"; token lexbuf }
  | _ { commentsoneline lexbuf }
  | eof   { print_endline "commentsoneline are not closed";
        raise End_of_file}

and commentsmultiline = parse
  | "*/" { print_endline "commentsmultiline close"; token lexbuf }
  | _ { commentsmultiline lexbuf }
  | eof   { print_endline "commentsmultiline are not closed";
        raise End_of_file}
