open Parser
open Lexer

let parse =
	let channelf = open_in (Sys.argv.(1)) in
	let lexbuf = Lexing.from_channel channelf in
		try
			let _ = Parser.stmt (Lexer.token) lexbuf in
			close_in channelf
		with
			End_of_file -> exit 0