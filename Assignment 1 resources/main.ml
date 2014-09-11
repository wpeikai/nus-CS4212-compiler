open Parser
open Lexer
(* 
let parse =
  let channelf = open_in (Sys.argv.(1)) in
  let lexbuf = Lexing.from_channel channelf in
    try
      let _ = Parser.stmt (Lexer.token) lexbuf in
      close_in channelf
    with
      End_of_file -> exit 0 *)

let parse =
  print_string "begiiiin\n";
	let channelf = open_in ("test3") in
	let lexbuf = Lexing.from_channel channelf in
		try
			let _ = Parser.program (Lexer.token) lexbuf in
			close_in channelf
		with
			End_of_file -> exit 0



(* let parse_file filename =
  let l = Lexing.from_channel (open_in filename) in *)

(* 
let _ =
      try
        let lexbuf = Lexing.from_channel stdin in
        while true do
          let _ = Parser.stmt Lexer.token lexbuf in
            print_newline(); flush stdout
        done
      with Lexer.Eof ->
        exit 0 *)