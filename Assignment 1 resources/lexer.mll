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
let boolean_literal = "true"|"false"
let varidexp = lower_alpha+

rule token = parse
| "." { printf "DOT \n"; DOT }
| "=" { printf "ASSSIGN\n"; ASSSIGN }
| "+" { printf "PLUS\n"; PLUS }
| "-" { printf "MINUS\n"; MINUS }
| "*" { printf "MULTIPLY\n"; MULTIPLY }
| "<" { printf "LOWER_OP\n"; LOWER_OP }
| "<=" { printf "LOWER_EQ_OP\n"; LOWER_EQ_OP }
| ">" { printf "HIGHER_OP\n"; HIGHER_OP }
| ">=" { printf "HIGHER_EQ_OP\n"; HIGHER_EQ_OP }
| "/" { printf "DIVIDE\n"; DIVIDE }
| "^" { printf "CARET\n"; CARET }
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
| ';' { printf "SEMICOLON \n"; SEMICOLON }
| "void" { printf "VOID_KEYWORD\n"; VOID_KEYWORD }
| "main" { printf "MAIN_KEYWORD\n"; MAIN_KEYWORD }
| "class" { printf "CLASS_KEYWORD \n"; CLASS_KEYWORD }
| identifier as id { printf "IDENTIFIER %s\n" id; IDENTIFIER }
| classname as cname { printf "CLASSNAME %s\n" cname; CLASSNAME }
| intexp as s { printf "INTLIT\n"; INTLIT (int_of_string s) }
| boolean_literal { printf "BOOLEAN_LITERAL\n"; BOOLEAN_LITERAL }
| "NULL" { printf "NULL\n"; NULL }
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
