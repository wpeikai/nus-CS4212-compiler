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
let string_literal_authorized_chars = ['\x20' '\x21' '\x23' '\x24' '\x25' '\x26' '\x27' '\x28' '\x29' '\x2A' '\x2B' '\x2C' '\x2D' '\x2E' '\x2F' '\x30'
'\x31' '\x32' '\x33' '\x34' '\x35' '\x36' '\x37' '\x38' '\x39' '\x3A' '\x3B' '\x3C' '\x3D' '\x3E' '\x3F' '\x40' '\x41' '\x42' '\x43' '\x44' '\x45' '\x46'
'\x47' '\x48' '\x49' '\x4A' '\x4B' '\x4C' '\x4D' '\x4E' '\x4F' '\x50' '\x51' '\x52' '\x53' '\x54' '\x55' '\x56' '\x57' '\x58' '\x59' '\x5A' '\x5B' '\x5D'
'\x5E' '\x5F' '\x60' '\x61' '\x62' '\x63' '\x64' '\x65' '\x66' '\x67' '\x68' '\x69' '\x6A' '\x6B' '\x6C' '\x6D' '\x6E' '\x6F' '\x70' '\x71' '\x72' '\x73'
'\x74' '\x75' '\x76' '\x77' '\x78' '\x79' '\x7A' '\x7B' '\x7C' '\x7D' '\x7E' '\x7F']

let string_literal = "\"" string_literal_authorized_chars* "\""

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
