{
    open Parser
    open Printf
    exception Eof
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

rule token = parse
| "." { printf "DOT \n"; DOT }
| "=" { printf "ASSSIGN\n"; ASSSIGN }
| "+" { printf "PLUS\n"; PLUS }
| "-" { printf "MINUS\n"; MINUS }
| "*" { printf "MULTIPLY\n"; MULTIPLY }
| "/" { printf "DIVIDE\n"; DIVIDE }
| "^" { printf "CARET\n"; CARET }
| "<" | "<=" | ">" | ">=" { printf "ARITH_OPERATOR\n"; ARITH_OPERATOR }
| "(" { printf "LPAREN\n"; LPAREN }
| ")" { printf "RPAREN\n"; RPAREN }
| "[" { printf "LBRACE\n"; LBRACE }
| "]" { printf "RBRACE\n"; RBRACE }
| "{" { printf "OPEN_BRACKET\n"; OPEN_BRACKET }
| "}" { printf "CLOSE_BRACKET\n"; CLOSE_BRACKET }
| "//" { print_endline "commentsoneline start"; commentsoneline lexbuf }
| "/*" { print_endline "commentsmultiline start"; commentsmultiline lexbuf }
| "!" { printf "EXCLAMATION_POINT \n"; EXCLAMATION_POINT }
| "?" { printf "QUESTION_POINT \n"; QUESTION_POINT }
| "=" { printf "EQ \n"; EQ }
| ";" { printf "SEMICOLON \n"; SEMICOLON }
| ";" { printf "COLON \n"; COLON }
| "&&" { printf "AND_OPERATOR \n"; AND_OPERATOR }
| "||" { printf "OR_OPERATOR \n"; OR_OPERATOR }
| "void" { printf "VOID_KEYWORD\n"; VOID_KEYWORD }
| "main" { printf "MAIN_KEYWORD\n"; MAIN_KEYWORD }
| "if" { printf "IF_KEYWORD\n"; IF_KEYWORD }
| "else" { printf "ELSE_KEYWORD\n"; ELSE_KEYWORD }
| "return" { printf "RETURN_KEYWORD\n"; RETURN_KEYWORD }
| "while" { printf "WHILE_KEYWORD\n"; WHILE_KEYWORD }
| "this" { printf "THIS_KEYWORD\n"; THIS_KEYWORD }
| "new" { printf "NEW_KEYWORD\n"; NEW_KEYWORD }
| "class" { printf "CLASS_KEYWORD \n"; CLASS_KEYWORD }
| "readln" { printf "READLN_KEYWORD \n"; READLN_KEYWORD }
| "println" { printf "PRINTLN_KEYWORD \n"; PRINTLN_KEYWORD }
| "null" { printf "NULL_KEYWORD\n"; NULL_KEYWORD }
| "true"|"false" { printf "BOOLEAN_LITERAL\n"; BOOLEAN_LITERAL }
| identifier as id { printf "IDENTIFIER %s\n" id; IDENTIFIER }
| classname as cname { printf "CLASSNAME %s\n" cname; CLASSNAME }
| intexp as s { printf "INTLIT\n"; INTLIT (int_of_string s) }
| varidexp as v { printf "VARID %s \n" v; VARID v}
| white { token lexbuf } (* ignore white space *)
| eof { printf "RAAAAIIIISSEE\n"; exit 0}

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
    
{
  let rec parse lexbuf =
      let zz = token lexbuf in
      (*print_newline zz;*) 
      (* do nothing in this example *)
      parse lexbuf
  let mai () =
    let lexbuf = Lexing.from_channel (open_in "test0") in
    while true do
      token lexbuf;
    done
  let _ = Printexc.print mai ()
}
