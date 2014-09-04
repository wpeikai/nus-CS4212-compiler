{
	open Parser
}

let intexp = ['1'-'9']['0'-'9']*
let varidexp = ['a'-'z']+
let white = [' ' '\t']

rule token = parse
| "+" 			{ PLUS }
| "=" 			{ ASSSIGN }
| "*" 			{ MULT }
| intexp as s 	{ INTLIT (int_of_string s)}
| varidexp as v { VARID v}
| white 		{ token lexbuf}
| eof { raise End_of_file}

