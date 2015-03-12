
(* ===================================================== *)
(* ============== CS4212 Compiler Design ============== *)
(*   Transformation to intermediary representation IR3   *)
(* ===================================================== *)

open Arm_structs
open Ir3_structs
open Jlite_structs
open Liveness

let labelcount = ref 0 
let fresh_label () = 
	(labelcount:=!labelcount+1; "L" ^ (string_of_int !labelcount))

let varcount = ref 0 
let fresh_var () = 
	(varcount:=!varcount+1; (string_of_int !varcount))

let argcount = ref 0 
let fresh_arg () = 
	(argcount:=!argcount+1; (string_of_int !argcount))

let ifcount = ref 0 
let fresh_if () = 
	(ifcount:=!ifcount+1; (string_of_int !ifcount))

(* Label exist of a metthod *)
let label_exit_methd (md:md_decl3): string=
	"." ^ md.id3 ^ "Exit"

(* Returns the type of a variable given its name *)
let rec var_type (var:id3) (var_list: var_decl3 list): ir3_type =
	match var_list with
	| (head_type,head_id3)::tail ->
		if (are_equal_id3 head_id3 var)
		then head_type
		else var_type var tail
	| _ -> 
		failwith ("#259 Unknown variable " ^  var)

(* returns the variables of a class given the class type *)
let rec class_var_list (class_type:ir3_type) ((class_list,_,_):ir3_program):var_decl3 list =
	let rec helper (class_list:cdata3 list) (class_type:ir3_type) =
		match class_type with
		| ObjectT cn ->
			begin
			match class_list with
			| (cname_head, var_list_head)::tail -> 
				if ((String.compare cname_head cn) == 0)
				then var_list_head
				else helper tail class_type
			| [] ->
				failwith "#260 Unknown class"
			end
		| _ ->
			failwith "#261 Looks like it's trying to access the field of NOT a class"
	in helper class_list class_type
	
(* Find the index of the variable in a list of variable *)
let index_id3_in_var_decl3_list (var:id3) (var_list:var_decl3 list): int=
	let rec helper(i:int) (var:id3) (var_list:var_decl3 list): int = 
		match var_list with
		| (_, head_id3)::tail ->
			if (are_equal_id3 head_id3 var)
			then i
			else helper (i+1) var tail
		| _ -> failwith ("#47 var unknown: " ^ var)
	in helper 0 var var_list

(* Return the offset between fp and the local variables based on the variable declarations list *)
let get_offset (var:id3) (md:md_decl3): int =
	let index_var = index_id3_in_var_decl3_list var (md.localvars3 @ md.params3)
	in 24 + 4 * index_var

(* Returns offset of variable within a class structure *)
let get_field_offset (class_id3:id3) (field_id3:id3) (md:md_decl3) (program_ir3:ir3_program):int=
	(* Find the class name in the local variables or in the params *)
	let vtype = var_type class_id3 (md.localvars3 @ md.params3) in
		let var_list = class_var_list vtype program_ir3 in
			4 * (index_id3_in_var_decl3_list field_id3 var_list)
	
(* Only works on simple types at the moment *)
let get_stack_space (md:md_decl3): int =	
	24 + 4 * List.length(md.localvars3 @ md.params3)

(* Only works on simple types at the moment *)
let get_stack_local_vars_space (md:md_decl3): int =	
	24 + 4 * List.length(md.localvars3)

(* Convert an integer i to #i *)
let number_op (i:int): operand2_type=
	ImmedOp ("#" ^ (string_of_int i))

let color_to_register (color:int): string =
	let letter = if color >= 0 then "v" else "a" in (letter ^ (string_of_int color))	
let convert_id3_var id3_0 color_table md  preferred_reg: arm_program * arm_program * reg =
	let reg1 = Hashtbl.find color_table id3_0 in
	if reg1 > 0 
	then [], [], color_to_register reg1
	else
		[LDR ("", "", preferred_reg, (RegPreIndexed ("fp", - get_offset id3_0 md , false)))],
		[STR ("", "", preferred_reg, (RegPreIndexed ("fp", - get_offset id3_0 md , false)))],
		preferred_reg

(* From a idc3 and the register, it gives all the instructions *)
let convert_operand (var:idc3) color_table (md:md_decl3) preferred_reg : arm_program * arm_program * operand2_type =
	match var with
		| IntLiteral3 i -> 
			[], [],  (number_op i)
		| Var3 var_id3 -> 
			let bef, aft, var_used = convert_id3_var  var_id3  color_table md  preferred_reg in
			bef, aft, (RegOp var_used)
		| BoolLiteral3 bool_id3 -> 
			begin
				match bool_id3 with
				(* true = 1 *)
				| true -> [], [], (number_op 1)
				(* false = 0 *)
				| false -> [], [], (number_op 0)
			end
		| _ -> failwith "#55"

(* From a idc3 and the register, it gives all the instructions *)
let convert_idc3 (var:idc3) color_table (md:md_decl3) preferred_reg : arm_program * arm_program * reg =
	match var with
	| IntLiteral3 i ->
		[MOV ("", false, preferred_reg, (number_op i))], [], preferred_reg
	| Var3 var_id3 -> 
		let bef, aft, var_used = convert_id3_var  var_id3  color_table md  preferred_reg in
		bef, aft, var_used
	| BoolLiteral3 bool_id3 -> 
		begin
			match bool_id3 with
			(* true = 1 *)
			| true -> [MOV ("", false, preferred_reg, (number_op 1))], [], preferred_reg
			(* false = 0 *)
			| false -> [MOV ("", false, preferred_reg, (number_op 0))], [], preferred_reg
		end
	| _ -> failwith "#55"


(* Push all idc3 variables on idc3_list in the stack in the registers a1, a2, a3, a4 *)
let rec push_arguments_on_stack (n:int) (idc3_list: idc3 list) (md:md_decl3) color_table: arm_program=
	
	if n < 4
	(* 4 arguments only *)
	then match idc3_list with 
		| head :: tail -> 
			let reg_to_store = ("a" ^ (string_of_int (n+1))) in
		let bef0, aft0, var_used0 = convert_idc3 head color_table md reg_to_store in
			let arm_instr =
				if var_used0 == reg_to_store
				then []
				else [MOV ("", false, reg_to_store, (RegOp var_used0))]
			in 
			bef0 @ arm_instr @ (push_arguments_on_stack (n+1) tail md color_table)
		| [] -> []
	else 
		let sub_sp_instr = 
			if n == 4
			then [SUB ("", false, "sp", "sp", (number_op ((List.length idc3_list) * 4)))]
			else []
		in

		(* If there are more than 4 arguments *)
		match idc3_list with 
			| head :: tail -> 
				let bef0, aft0, var_used0 = convert_idc3 head color_table md  "v1" in
				sub_sp_instr @
				bef0 @
				STR ("", "", var_used0, (RegPreIndexed ("sp", (4 * (n-4)), false))) :: 
				aft0 @
				push_arguments_on_stack (n+1) tail md color_table
			| [] -> []
	

let rec helper_2 color_set current_color =
	let is_same_color (c:int):bool = (c == current_color) in
	if ColorSet.exists is_same_color color_set
	then helper_2 color_set (current_color+1)
	else current_color

let convert_def_use_t_color_set node color_table:color_set =
	let def_use = Id3Set.union node.def node.use in
	let f elt init = ColorSet.add (Hashtbl.find color_table elt) init in
	Id3Set.fold f def_use ColorSet.empty


let find_new_color node color_table =
	let color_used = convert_def_use_t_color_set node color_table in
	helper_2 color_used 1

(* Convert an ir3 statement to arm instructions *)
let convert_ir3_stmt_node (stmt_node:stmt_node) color_table (md:md_decl3) (program_ir3:ir3_program):arm_program * arm_program = 
	match stmt_node.stmt with
	| AssignStmt3 (id3_0, ir3_exp_0) ->
	
		begin
		match ir3_exp_0 with
			| BinaryExp3 (ir3_op_1, idc3_1, idc3_2) ->
				let bef0, aft0, var_used0 = convert_id3_var id3_0 color_table md  "a1" in
				let bef1, aft1, var_used1 = convert_idc3 idc3_1 color_table md "a1" in
				let bef2, aft2, var_used2reg = convert_operand idc3_2 color_table md "a2" in

				begin
					match ir3_op_1 with 
					| AritmeticOp op ->
						begin
							match op with
							| "+" -> 
								[],
									bef0 @
									bef1 @
									bef2 @
									[ADD ("", false, var_used0, var_used1, var_used2reg)] @
									aft0 @
									aft1 @
									aft2	
							| "-" ->
								[],
									bef0 @
									bef1 @
									bef2 @
								[SUB ("", false, var_used0, var_used1, var_used2reg)] @
									aft0 @
									aft1 @
									aft2	
							| "*" -> 	
								(* Improve multiplication *)
								begin 
								match var_used2reg with 
								|  RegOp reg -> 
									[],
										bef0 @
										bef1 @
										bef2 @
										[MUL ("", false, var_used0, var_used1, reg)] 	@
										aft0 @
										aft1 @
										aft2		
								| ImmedOp s -> 
									let bef3, aft3, var_used3 = convert_idc3 idc3_2 color_table md "a2" in
									[], 
										bef0 @
										bef1 @
										bef3 @
										[MUL ("", false, var_used0, var_used1, var_used3)] 	@
										aft0 @
										aft1 @
										aft3
								end
							| _ -> failwith "#57"
						end
					
					| BooleanOp op -> 
						let bef0, aft0, var_used0 = convert_id3_var id3_0 color_table md  "a1" in
						let bef1, aft1, var_used1 = convert_idc3 idc3_1 color_table md "a1" in
						let bef2, aft2, var_used2reg = convert_operand idc3_2 color_table md "a2" in

						let arm_op_instructions =
							begin
								match op with
								| "&&" -> 
									[AND ("", false, var_used0, var_used1, var_used2reg)]
								| "||" -> 
									[ORR ("", false, var_used0, var_used1, var_used2reg)]
								| _ -> failwith "#61"
							end in
						[],
							bef0 @
							bef1 @
							bef2 @
							arm_op_instructions 	@
							aft0 @
							aft1 @
							aft2
					| RelationalOp op -> 
						let bef0, aft0, var_used0 = convert_id3_var id3_0 color_table md  "a1" in
						let bef1, aft1, var_used1 = convert_idc3 idc3_1 color_table md "a1" in
						let bef2, aft2, var_used2op = convert_operand idc3_2 color_table md "a2" in
						let arm_op_instructions =
							begin
								match op with
								| "==" -> 	
									MOV ("eq", false, var_used0, (number_op 1)) :: 
									MOV ("ne", false, var_used0, (number_op 0)) :: []
								| "!=" -> 	
									MOV ("eq", false, var_used0, (number_op 0)) :: 
									MOV ("ne", false, var_used0, (number_op 1)) :: []
								| ">" -> 	
									MOV ("gt", false, var_used0, (number_op 1)) :: 
									MOV ("lt", false, var_used0, (number_op 0)) :: []
								| ">=" -> 
									MOV ("ge", false, var_used0, (number_op 1)) :: 
									MOV ("le", false, var_used0, (number_op 0)) :: []
								| "<" -> 
									MOV ("gt", false, var_used0, (number_op 0)) :: 
									MOV ("lt", false, var_used0, (number_op 1)) :: []
								| "<=" -> 
									MOV ("ge", false, var_used0, (number_op 0)) :: 
									MOV ("le", false, var_used0, (number_op 1)) :: []
								| _ -> failwith "#59"
							end in
						[],
							bef0 @
							bef1 @
							bef2 @
							[CMP ("", var_used1, var_used2op)] @
							arm_op_instructions @
							aft0 @
							aft1 @
							aft2
					| _ -> failwith "#54: Unknown binary operator"
				end
			| UnaryExp3 (ir3_op_1, idc3_0) ->
				let bef0, aft0, var_used0 = convert_id3_var id3_0 color_table md  "a1" in
				let bef2, aft2, var_used2reg = convert_operand idc3_0 color_table md "a2" in

				begin
					match ir3_op_1 with 
					| UnaryOp op ->
						let arm_op_instructions =
							begin
								match op with
								| "-" -> 
									MOV ("", false, "a3", (number_op 0)) ::
									SUB ("", false, var_used0, "a3", var_used2reg) :: []
								| "!" ->
									MVN ("", false, var_used0, var_used2reg) :: []
								| _ -> failwith "#67"
							end in
						[],
							bef0 @
							bef2 @
							arm_op_instructions 	@
							aft0 @
							aft2
					| _ -> failwith "#544: Unknown unary operator"
				end
			| Idc3Expr idc3_0 ->
				let bef0, aft0, var_used0 = convert_id3_var id3_0 color_table md  "a1" in
				let bef2, aft2, var_used2reg = convert_operand idc3_0 color_table md "a2" in
				let arm_op_instructions = 
					[MOV ("", false, var_used0, var_used2reg)]
				in 		[], 
							bef0 @
							bef2 @
							arm_op_instructions 	@
							aft0 @
							aft2
			| FieldAccess3 (id3_1, id3_2) ->
				let bef0, aft0, var_used0 = convert_id3_var id3_0 color_table md  "a1" in

				let bef1, aft1, var_used1 = convert_id3_var id3_1 color_table md  "a2" in

				let field_offset = (get_field_offset id3_1 id3_2 md program_ir3)

				in [],
					bef0 @ 
					bef1 @
					LDR ("", "", var_used0, (RegPreIndexed (var_used1, field_offset, false))) ::
					aft0 @
					aft1
			| ObjectCreate3 class_name ->
				let bef0, aft0, var_used0 = convert_id3_var id3_0 color_table md  "a1" in

				let var_list = class_var_list (ObjectT class_name) program_ir3
				in let alloc_size = 4 * List.length var_list in
				let arm_op_instructions = 
					MOV ("", false, "a1", (number_op alloc_size)) ::
					BL ("", "_Znwj(PLT)") ::
					MOV ("", false, var_used0, (RegOp "a1")) :: []
				in [],
					bef0 @ 
					arm_op_instructions @
					aft0
			| MdCall3 (id3_0_md, idc3_list) -> 
				let bef0, aft0, var_used0 = convert_id3_var id3_0 color_table md  "a1" in

				let idc3_list_length = (List.length idc3_list) in

				let arm_op_instructions = 
				if idc3_list_length > 4
				then [ADD ("", false, "sp", "sp", number_op ((idc3_list_length-4)*4))] @ [MOV ("", false, var_used0, (RegOp "a1"))]
				else [MOV ("", false, var_used0, (RegOp "a1"))]
				in [],
					bef0 @ 
					(push_arguments_on_stack 0 idc3_list md color_table) @ 
					[BL ("", id3_0_md ^ "(PLT)")] @
					arm_op_instructions @
					aft0
		end

	| PrintStmt3 idc3_0 ->
		begin
			(* Label fresh gives a new label *)
			let label_string = fresh_label() in
			match idc3_0 with 
			| StringLiteral3 str ->
				(* Add a newline at the end of the string *)
				Label label_string :: 
				[PseudoInstr (".asciz \"" ^ str ^ "\\n\"")],
				LDR ("", "", "a1", (LabelAddr ("=" ^ label_string))) :: 
				[BL ("", "printf(PLT)")]
			| IntLiteral3 i -> 
				(* Add a newline at the end of the string *)
				Label label_string :: 
				[PseudoInstr (".asciz \"" ^ (string_of_int i) ^"\\n\"")],
				LDR ("", "", "a1", (LabelAddr ("=" ^ label_string))) :: 
				[BL ("", "printf(PLT)")]
			| Var3 var_id3 -> 
				let bef1, aft1, var_used1 = convert_id3_var var_id3 color_table md "a1" in

				Label label_string :: 
				[PseudoInstr (".asciz \"%i\\n\"")],
				LDR ("", "", "a1", (LabelAddr ("=" ^ label_string))) ::	
				MOV ("", false,  "a2", (RegOp var_used1)) ::
				[BL ("", "printf(PLT)")]
			| _ -> failwith "#69"
		end

	| AssignFieldStmt3 (ir3_exp_1, ir3_exp_2) ->
		let (id3_1, id3_2) = 
			begin match ir3_exp_1 with
				| FieldAccess3 (a, b) ->
					(a, b)
				| _ -> 
					failwith "Left Hand Side should be a field access"
			end in
		let id3_3 =
		begin 
			match ir3_exp_2 with
				| Idc3Expr iwwww -> 
					begin
						match iwwww with
						| Var3 i -> i
						| _ -> failwith "444"
					end
				| _ -> failwith "444"
		end 
		in let field_offset = (get_field_offset id3_1 id3_2 md program_ir3) in

		let bef1, aft1, var_used1 = convert_id3_var id3_3 color_table md "a2" in
		let bef2, aft2, var_used2 = convert_id3_var id3_1 color_table md "a1"

		in [],
			bef1 @ 
			bef2 @
			STR ("", "", var_used1, (RegPreIndexed (var_used2, field_offset, false))) ::
			aft1 @
			aft2

	| IfStmt3 (ir3_expr_0, label_0) -> (*TODO*)
		let a = 
			begin 
				match ir3_expr_0 with
					| Idc3Expr idc3_0 ->
						let bef2, aft2, var_used2 = convert_idc3 idc3_0 color_table md "a1" in
						let arm_op_instructions =
							[CMP ("", var_used2, (number_op 1))] @ [B ("eq", "." ^ (string_of_int label_0))]
						in
							bef2 @
							arm_op_instructions 	@
							aft2
					| BinaryExp3 (ir3_op_1, idc3_1, idc3_2) -> 
						let arm_op_instructions =
							begin 
							match ir3_op_1 with
								| RelationalOp op -> 
									begin
										match op with
										| "==" -> 	
											MOV ("eq", false, "a2", (number_op 1)) :: 
											MOV ("ne", false, "a2", (number_op 0)) :: []
										| "!=" -> 	
											MOV ("eq", false, "a2", (number_op 0)) :: 
											MOV ("ne", false, "a2", (number_op 1)) :: []
										| _ -> failwith "4445"
									end
								| _ -> failwith "999"
							end
						in

						let bef_instr1, after_instr1, var_used1 = convert_idc3 idc3_1 color_table md "a1" in
						let bef_instr2, after_instr2, var_used2 = convert_operand idc3_2 color_table md "a2" 
						in bef_instr1 @ bef_instr2 @ arm_op_instructions @ [CMP ("", var_used1, var_used2)]@  after_instr1 @ after_instr2 @ [B ("eq", "." ^ (string_of_int label_0))]
					| UnaryExp3 (ir3_op_1, idc3_0) ->
						let bef2, aft2, var_used2 = convert_operand idc3_0 color_table md "a1" in
						let arm_op_instructions =
							[MOV ("", false, "a2", (number_op 0))] @
							[CMP ("", "a2", var_used2)] @ [B ("eq", "." ^ (string_of_int label_0))]
						in
							bef2 @
							arm_op_instructions 	@
							aft2
			| _ -> failwith ("666   " ^ (string_of_int label_0))
			end
		in [], a
		
	| Label3 label_0 -> 
		[], [PseudoInstr ("\n." ^ (string_of_int label_0) ^ ":")]
	| GoTo3 label_0 -> 
		[], [B ("", "." ^ (string_of_int label_0))]
	| ReturnStmt3 id3_1 ->

		let bef_instr, after_instr, var_used0 = convert_operand (Var3 id3_1) color_table md "a1" in
		[],
			bef_instr @
			MOV ("", false, "a1", var_used0) :: 
			[B ("", label_exit_methd md)]
	| ReturnVoidStmt3 ->
		[], MOV ("", false, "a1", number_op 0) :: 
		[B ("", label_exit_methd md)]
	| LoadStmt3 i ->
		let color_i = Hashtbl.find color_table i in
		if color_i > 0
		then [],
				[LDR ("", "", (color_to_register color_i), (RegPreIndexed ("fp", - get_offset i md , false)))]
		else [], []
	| StrStmt3 i ->
		let color_i = Hashtbl.find color_table i in
		if color_i > 0
		then [], [STR ("", "", (color_to_register color_i), (RegPreIndexed ("fp", - get_offset i md , false)))]
		else [], []

	| _ ->
		failwith "#51: Statement not yet implemented"


let rec convert_ir3_stmt_node_list (stmts: stmt_node list) color_table (md:md_decl3) (program_ir3:ir3_program) : arm_program * arm_program =
		match stmts with
		| head::tail -> 
			let data_instr_list, text_instr_list = convert_ir3_stmt_node head color_table md program_ir3 in
			let data_instr_tail_list, text_instr_tail_list = convert_ir3_stmt_node_list tail color_table md program_ir3 in
			data_instr_list @ data_instr_tail_list, text_instr_list @ text_instr_tail_list
		| [] -> [], []

(* Store all parameters in the stack when a function is called *)
let rec store_params_instr (n:int) (params_list: var_decl3 list) (md:md_decl3) : arm_program=
	let stack_local_var_size = get_stack_local_vars_space md 
	in if n < 4
	(* 4 arguments only *)
	then match params_list with 
		| head :: tail -> 
			STR ("", "", "a" ^ (string_of_int (n+1)), (RegPreIndexed ("fp", -(stack_local_var_size + 4 * n), false))) ::
			store_params_instr (n+1) tail md
		| [] -> []
	else 
	(* If there are more than 4 arguments *)
	match params_list with 
		| head :: tail -> 
			LDR ("", "", "v1", (RegPreIndexed ("fp", (4 * (n-3)), false))) ::
			STR ("", "", "v1", (RegPreIndexed ("fp", -(stack_local_var_size + 4 * n), false))) ::
			store_params_instr (n+1) tail md
		| [] -> []


(* Alive params should be loaded at the beginnning of the function *)
let load_alive_params md_struct :arm_program =
	let color_table = md_struct.colored_tab in
	let first_stmt_node = List.hd md_struct.stmt_node_list in
	let all_var_alive = first_stmt_node.live_in in
	let all_params = md_struct.md.params3 in
	let f (init:id3 list) (elt:var_decl3)  : (id3 list)  =
					let _, res = elt in
					 if (Id3Set.exists (are_equal_id3 res) all_var_alive)
					 then res :: init
					 else init in
	let all_params_to_load =
		List.fold_left f [] all_params
	in let g  (init:arm_program) (elt:id3) : arm_program= 
		let color = Hashtbl.find color_table elt in
		(LDR ("", "", (color_to_register color), (RegPreIndexed ("fp", -(get_offset elt md_struct.md), false)))) :: init
	in List.fold_left g [] all_params_to_load


let convert_ir3_md_decl color_table (md_struct:md_struct) (program_ir3:ir3_program): arm_program * arm_program=
	let data_instr_list, text_instr_list = convert_ir3_stmt_node_list md_struct.stmt_node_list color_table md_struct.md program_ir3 in
	let store_params = store_params_instr 0 md_struct.md.params3 md_struct.md in
	let load_params_instr = load_alive_params md_struct in
	data_instr_list,
		(*Label with function name*)
		PseudoInstr ("\n" ^ md_struct.md.id3 ^ ":") ::
		(*Store registers on the stack*)
		STMFD ("fp" :: "lr" :: "v1" :: "v2" :: "v3" :: "v4" :: "v5" :: []) ::
		(* sp = fp - 24 *)
		ADD ("", false, "fp", "sp", number_op 24) ::
		(* allocate local variables*)
		SUB ("", false, "sp", "fp", number_op (get_stack_space md_struct.md)) ::
		store_params @
		load_params_instr @
		text_instr_list @
		(* Put a L#exit label here *)
		PseudoInstr ("\n" ^ label_exit_methd md_struct.md ^ ":") ::
		SUB ("", false, "sp", "fp", ImmedOp "#24") ::
		[LDMFD ("fp" :: "pc" :: "v1" :: "v2" :: "v3" :: "v4" :: "v5" :: [])]

(* Convert a list of md_decl3 *)
let rec convert_md_decl3_list (mds:md_struct list) (program_ir3:ir3_program):arm_program *arm_program = 
	match mds with
		| head::tail -> 
			let color_table = head.colored_tab in 
			let data_instr_list, text_instr_list = convert_ir3_md_decl color_table head program_ir3 in
			let data_instr_tail_list, text_instr_tail_list = convert_md_decl3_list tail program_ir3 in
			data_instr_list @ data_instr_tail_list, text_instr_list @ text_instr_tail_list
		| [] -> [], []

(* Convert a ir3 program to arm program *)
let ir3_program_to_arm (program_ir3:ir3_program):arm_program =
	let method_hash_table = create_md_table program_ir3 in
	(* print_md_table method_hash_table; *)
	let f k elt init = elt :: init in
	let md_struct_list = Hashtbl.fold f method_hash_table [] in
	let data_instr_list, text_instr_list = convert_md_decl3_list (md_struct_list) program_ir3 in
	PseudoInstr (".data") ::
	PseudoInstr ("") ::
	data_instr_list @
	PseudoInstr ("\n.text") ::
	PseudoInstr ("\n.global main") ::
   	text_instr_list @
	[PseudoInstr ("\n")] (* Add a newline at the end *)
