
(* ===================================================== *)
(* ============== CS4212 Compiler Design ============== *)
(*   Transformation to intermediary representation IR3   *)
(* ===================================================== *)

open Arm_structs
open Ir3_structs
open Jlite_structs

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

let compare_id3 (i1:id3) (i2:id3): bool =
	((String.compare i1 i2) == 0)

(* Label exist of a metthod *)
let label_exit_methd (md:md_decl3): string=
	"." ^ md.id3 ^ "Exit"

(* Returns the type of a variable given its name *)
let rec var_type (var:id3) (var_list: var_decl3 list): ir3_type =
	match var_list with
	| (head_type,head_id3)::tail ->
		if (compare_id3 head_id3 var)
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
			if (compare_id3 head_id3 var)
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

(* From a idc3 and the register, it gives all the instructions *)
let convert_idc3 (var:idc3) (register:reg) (md:md_decl3): arm_program =
	match var with
	| IntLiteral3 i -> 
		[MOV ("", false, register, (number_op i))]
	| Var3 var_id3 -> 
		[LDR ("", "", register, (RegPreIndexed ("fp", - get_offset var_id3 md , false)))]
	| BoolLiteral3 bool_id3 -> 
		begin
			match bool_id3 with
			(* true = 1 *)
			| true -> [MOV ("", false, register, (number_op 1))]
			(* false = 0 *)
			| false -> [MOV ("", false, register, (number_op 0))]
		end
	| _ -> failwith "#55"

(* 
(* Return the n first elements of l *)
let rec filter_first_n (n:int) (l: idc3 list) : idc3 list=
	match l with
	| head::tail ->	
		if n > 0
		then head :: filter_first_n (n-1) tail
		else []
	| [] -> []

Return all elements execpt the n first elements of l
(* Return empty list if less than n *)
let rec filter_last_n (n:int) (l: idc3 list) : idc3 list=
	if n == 0
	then l
	else match l with
		| head::tail ->	
			filter_last_n (n-1) tail
		| [] -> [] *)
	
(* Push all idc3 variables on idc3_list in the stack in the registers a1, a2, a3, a4 *)
let rec push_arguments_on_stack (n:int) (idc3_list: idc3 list) (md:md_decl3) : arm_program=
	
	if n < 4
	(* 4 arguments only *)
	then match idc3_list with 
		| head :: tail -> 
			(convert_idc3 head ("a" ^ (string_of_int (n+1))) md) @ (push_arguments_on_stack (n+1) tail md)
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
			sub_sp_instr @
			convert_idc3 head "v1" md @
			STR ("", "", "v1", (RegPreIndexed ("sp", (4 * (n-4)), false))) :: 
			push_arguments_on_stack (n+1) tail md
		| [] -> []
	

(* Convert an ir3 expr to arm instructions *)
let convert_ir3_expr (exp:ir3_exp) (md:md_decl3) (program_ir3:ir3_program): arm_program=
	match exp with
	| BinaryExp3 (ir3_op_1, idc3_1, idc3_2) ->
		let instructions1 = convert_idc3 idc3_1 "a1" md
		in let instructions2 = convert_idc3 idc3_2 "a2" md
		in 
		begin
			match ir3_op_1 with 
			| AritmeticOp op ->
				let arm_op_instructions =
					begin
						match op with
						| "+" -> 
							[ADD ("", false, "v1", "a1", (RegOp "a2"))]
						| "-" ->
							[SUB ("", false, "v1", "a1", (RegOp "a2"))]
						| "*" -> 	
							(* Improve multiplication *)
							[MUL ("", false, "v1", "a1", "a2")] 						
						| _ -> failwith "#57"
					end
				in instructions1 @ instructions2 @ arm_op_instructions
			| RelationalOp op -> 
				let comparative_inst = [CMP ("", "a1", (RegOp "a2"))]
				in let arm_op_instructions =
					begin
						match op with
						| "==" -> 	
							MOV ("eq", false, "v1", (number_op 1)) :: 
							MOV ("ne", false, "v1", (number_op 0)) :: []
						| "!=" -> 	
							MOV ("eq", false, "v1", (number_op 0)) :: 
							MOV ("ne", false, "v1", (number_op 1)) :: []
						| ">" -> 	
							MOV ("gt", false, "v1", (number_op 1)) :: 
							MOV ("lt", false, "v1", (number_op 0)) :: []
						| ">=" -> 
							MOV ("ge", false, "v1", (number_op 1)) :: 
							MOV ("le", false, "v1", (number_op 0)) :: []
						| "<" -> 
							MOV ("gt", false, "v1", (number_op 0)) :: 
							MOV ("lt", false, "v1", (number_op 1)) :: []
						| "<=" -> 
							MOV ("ge", false, "v1", (number_op 0)) :: 
							MOV ("le", false, "v1", (number_op 1)) :: []
						| _ -> failwith "#59"
					end
				in instructions1 @ instructions2 @ comparative_inst @ arm_op_instructions
			| BooleanOp op -> 
				let arm_op_instructions =
					begin
						match op with
						| "&&" -> 
							[AND ("", false, "v1", "a1", (RegOp "a2"))]
						| "||" -> 
							[ORR ("", false, "v1", "a1", (RegOp "a2"))]
						| _ -> failwith "#61"
					end
				in let comparative_inst = CMP ("", "v1", (number_op 0)) :: MOV ("eq", false, "v1", (number_op 0)) :: MOV ("ne", false, "v1", (number_op 1)) :: []
				in instructions1 @ instructions2 @ arm_op_instructions @ comparative_inst
			| _ -> failwith "#54: Unknown binary operator"
		end
	| UnaryExp3 (ir3_op_1, idc3_0) ->
		let instructions1 = convert_idc3 idc3_0 "a1" md
		in 
		begin
			match ir3_op_1 with 
			| UnaryOp op ->
				let arm_op_instructions =
					begin
						match op with
						| "-" -> 
							MOV ("", false, "a2", (number_op 0)) ::
							SUB ("", false, "v1", "a2", (RegOp "a1")) :: []
						| "!" -> 
							CMP ("", "a1", (number_op 1)) :: 
							MOV ("eq", false, "v1", (number_op 0)) :: 
							MOV ("ne", false, "v1", (number_op 1)) :: []
						| _ -> failwith "#67"
					end
				in instructions1 @ arm_op_instructions
			| _ -> failwith "#544: Unknown unary operator"
		end
	| Idc3Expr idc3_0 ->
		convert_idc3 idc3_0 "v1" md
	| FieldAccess3 (id3_1, id3_2) ->
		LDR ("", "", "v2", (RegPreIndexed ("fp", - get_offset id3_1 md, false))) ::
		LDR ("", "", "v1", (RegPreIndexed ("v2", get_field_offset id3_1 id3_2 md program_ir3,false))) :: 
		[]
	| ObjectCreate3 class_name ->
		let var_list = class_var_list (ObjectT class_name) program_ir3
		in let alloc_size = 4 * List.length var_list
		in MOV ("", false, "a1", (number_op alloc_size)) ::
		BL ("", "_Znwj(PLT)") ::
		MOV ("", false, "v1", (RegOp "a1")) :: []	
	| MdCall3 (id3_0, idc3_list) -> 
		let idc3_list_length = (List.length idc3_list) in
		push_arguments_on_stack 0 idc3_list md @ 
		BL ("", id3_0 ^ "(PLT)") ::
		if idc3_list_length > 4
		then [ADD ("", false, "sp", "sp", number_op ((idc3_list_length-4)*4))] @ [MOV ("", false, "v1", (RegOp "a1"))]
		else [MOV ("", false, "v1", (RegOp "a1"))]
	| _ ->
		failwith "#50: Expression not yet implemented"

(* Convert an ir3 statement to arm instructions *)
let convert_ir3_stmt (stmt:ir3_stmt) (md:md_decl3) (program_ir3:ir3_program):arm_program * arm_program = 
	match stmt with
	| AssignStmt3 (id3_0, ir3_exp_0) ->
		[], (convert_ir3_expr ir3_exp_0 md program_ir3) @ [STR ("", "", "v1", (RegPreIndexed ("fp", - get_offset id3_0 md, false)))]
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
				[PseudoInstr (".asciz \"%i\\n\"")],
				LDR ("", "", "a1", (LabelAddr ("=" ^ label_string))) :: 
				MOV ("", false, "a2", (number_op i)) :: 
				[BL ("", "printf(PLT)")]
			| Var3 var_id3 -> 
				(* Add a newline at the end of the string *)
				Label label_string :: 
				[PseudoInstr (".asciz \"%i\\n\"")],
				LDR ("", "", "a1", (LabelAddr ("=" ^ label_string))) ::
				LDR ("", "", "a2", (RegPreIndexed ("fp", - get_offset var_id3 md , false))) ::
				[BL ("", "printf(PLT)")]
			| _ -> failwith "#69"
		end
	| AssignFieldStmt3 (ir3_exp_1, ir3_exp_2) ->
		let (id3_1, id3_2) = begin match ir3_exp_1 with
			| FieldAccess3 (a, b) ->
				(a, b)
			| _ -> 
				failwith "Left Hand Side should be a field access"
			end
		in let idc3_1 = begin match ir3_exp_2 with
			| Idc3Expr a ->
				begin
				match a with
				| Var3 b ->
					b
				| _ ->
					failwith "#270"
				end
			| _ -> 
				failwith "Right hand side should be a variable"
			end
		in let var_offset_r = (get_offset idc3_1 md)
		in let var_offset_l = (get_offset id3_1 md)
		in let field_offset = (get_field_offset id3_1 id3_2 md program_ir3)
		in [],
		LDR ("", "", "v1", (RegPreIndexed ("fp", -var_offset_r , false))) ::
		LDR ("", "", "v2", (RegPreIndexed ("fp", -var_offset_l , false))) ::
		STR ("", "", "v1", (RegPreIndexed ("v2", field_offset, false))) :: []	
	| IfStmt3 (ir3_expr_0, label_0) -> 
		[],
		convert_ir3_expr ir3_expr_0 md program_ir3 @
		CMP ("", "v1", (number_op 1)) ::
		[B ("eq", "." ^ (string_of_int label_0))]
	| Label3 label_0 -> 
		[], [PseudoInstr ("\n." ^ (string_of_int label_0) ^ ":")]
	| GoTo3 label_0 -> 
		[], [B ("", "." ^ (string_of_int label_0))]
	| ReturnStmt3 id3_1 ->
		let var_offset = (get_offset id3_1 md)
		in [],
		LDR ("", "", "a1", (RegPreIndexed ("fp", -var_offset , false))) :: 
		[B ("", label_exit_methd md)]
	| ReturnVoidStmt3 ->
		[], MOV ("", false, "a1", number_op 0) :: 
		[B ("", label_exit_methd md)]
	| _ ->
		failwith "#51: Statement not yet implemented"

let rec convert_ir3_stmt_list (stmts: ir3_stmt list) (md:md_decl3) (program_ir3:ir3_program): arm_program * arm_program =
		match stmts with
		| head::tail -> 
			let data_instr_list, text_instr_list = convert_ir3_stmt head md program_ir3 in
			let data_instr_tail_list, text_instr_tail_list = convert_ir3_stmt_list tail md program_ir3 in
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


let convert_ir3_md_decl (md:md_decl3) (program_ir3:ir3_program): arm_program * arm_program=
	let data_instr_list, text_instr_list = convert_ir3_stmt_list md.ir3stmts md program_ir3 in
	let store_params = store_params_instr 0 md.params3 md in
	data_instr_list,
		(*Label with function name*)
		PseudoInstr ("\n" ^ md.id3 ^ ":") ::
		(*Store registers on the stack*)
		STMFD ("fp" :: "lr" :: "v1" :: "v2" :: "v3" :: "v4" :: "v5" :: []) ::
		(* sp = fp - 24 *)
		ADD ("", false, "fp", "sp", number_op 24) ::
		(* allocate local variables*)
		SUB ("", false, "sp", "fp", number_op (get_stack_space md)) ::
		store_params @
		text_instr_list @
		(* Put a L#exit label here *)
		PseudoInstr ("\n" ^ label_exit_methd md ^ ":") ::
		SUB ("", false, "sp", "fp", ImmedOp "#24") ::
		[LDMFD ("fp" :: "pc" :: "v1" :: "v2" :: "v3" :: "v4" :: "v5" :: [])]

(* Convert a list of md_decl3 *)
let rec convert_md_decl3_list (mds:md_decl3 list) (program_ir3:ir3_program):arm_program *arm_program = 
	match mds with
		| head::tail -> 
			let data_instr_list, text_instr_list = convert_ir3_md_decl head program_ir3 in
			let data_instr_tail_list, text_instr_tail_list = convert_md_decl3_list tail program_ir3 in
			data_instr_list @ data_instr_tail_list, text_instr_list @ text_instr_tail_list
		| [] -> [], []

(* Convert a ir3 program to arm program *)
let ir3_program_to_arm (program_ir3:ir3_program):arm_program =
	let (cdata3_list, main_md_decl3, md_decl3_list) = program_ir3 in
	let data_instr_list, text_instr_list = convert_md_decl3_list (main_md_decl3 :: md_decl3_list) program_ir3 in

	PseudoInstr (".data") ::
	PseudoInstr ("") ::
	data_instr_list @
	PseudoInstr ("\n.text") ::
	PseudoInstr ("\n.global main") ::
   	text_instr_list @
	[PseudoInstr ("\n")] (* Add a newline at the end *)

(* 	
let iR3Expr_get_idc3 (exp:ir3_exp) =
	match exp with
	| Idc3Expr e -> e
	| _ -> failwith " Error in getidc "
		
let iR3Expr_get_id3 (exp:ir3_exp) =
	match exp with
	| Idc3Expr e ->  
		begin
		match e with 
		| Var3 id -> id
		| _ -> failwith " Error in getid "
		end
	| _ -> failwith " Error in getid"

let iR3Expr_to_id3 (exp:ir3_exp) (typ:jlite_type) 
	vars stmts (toidc3:bool) 
	: (ir3_exp * var_decl3 list * ir3_stmt list)  =
	if (toidc3 == false) then (exp,vars,stmts)
	else
	let new_varname = "_t" ^ fresh_var() in
	let new_vardecl = (typ, new_varname) in 
	let new_stmt = AssignStmt3 (new_varname, exp) in
	(Idc3Expr (Var3 new_varname), 
		List.append vars [new_vardecl], 
		List.append stmts [new_stmt])
	
(* Transform a var_id to IR3 by looking at the scope of the variable *)	
let jlitevarid_to_IR3Expr
	(classid: class_name) 
	(v:var_id) (toid3:bool)
	:(ir3_exp * var_decl3 list * ir3_stmt list) =
	match v with
	| SimpleVarId id -> (Idc3Expr (Var3 id),[],[])
	| TypedVarId (id,t,s) -> 
		if (s == 1) (* class scope *)
		then 
			let thisExpr = 
			 FieldAccess3 ("this",id) in 
			 (iR3Expr_to_id3 thisExpr t [] [] toid3)
		else
			let newExpr = Idc3Expr (Var3 id) in
			(newExpr,[], [])
			
(* Transform an expression to IR3 *)
let rec jliteexpr_to_IR3Expr
	(classid: class_name) 
	(jexp:jlite_exp) (toidc3:bool) (toid3:bool)
	:(ir3_exp * var_decl3 list * ir3_stmt list) =
	let rec helper 
		(je:jlite_exp) (toidc3:bool) (toid3:bool)=
		match je with
		| BoolLiteral v -> 
			let newExpr = Idc3Expr (BoolLiteral3 v) in 
			(iR3Expr_to_id3 newExpr BoolT [] [] toid3)
		| IntLiteral v -> 
			let newExpr = Idc3Expr (IntLiteral3 v) in
			(iR3Expr_to_id3 newExpr IntT [] [] toid3)
		| StringLiteral v -> 
			let newExpr = Idc3Expr (StringLiteral3 v) in 
			(iR3Expr_to_id3 newExpr StringT [] [] toid3)
		| TypedExp (te,t) -> 
			begin 
			match te with
			| Var v -> 
				(jlitevarid_to_IR3Expr classid v toidc3)
			| ThisWord -> (Idc3Expr (Var3 "this"),[],[])
			| NullWord -> 
				let newExpr = (Idc3Expr (IntLiteral3 0)) in 
				(iR3Expr_to_id3 newExpr t [] [] toid3)
			| UnaryExp (op,arg) -> 
				let (argIR3,vars,stmts) = (helper arg true false) in
				let argIdc3 = (iR3Expr_get_idc3 argIR3) in 
				let newExpr = UnaryExp3 (op,argIdc3) in 
				(iR3Expr_to_id3 newExpr t vars stmts toidc3)
			| BinaryExp (op,arg1,arg2) -> 
				let (arg1IR3,vars1,stmts1) = (helper arg1 true false) in
				let (arg2IR3,vars2,stmts2) = (helper arg2 true false) in
				let arg1Idc3 = (iR3Expr_get_idc3 arg1IR3) in 
				let arg2Idc3 = (iR3Expr_get_idc3 arg2IR3) in 
				let newExpr = BinaryExp3 (op, arg1Idc3, arg2Idc3) in 
				(iR3Expr_to_id3 newExpr t 
					(vars1@vars2) 
					(stmts1@stmts2) toidc3)
			| FieldAccess (arg,id) -> 
				let (argIR3,vars,stmts) = (helper arg true true) in
				let argId3 = (iR3Expr_get_id3 argIR3) in 
				let newExpr = FieldAccess3 (argId3, string_of_var_id id) in
				(iR3Expr_to_id3 newExpr t vars stmts toidc3)
			| ObjectCreate c -> 
				let newExpr = ObjectCreate3 c in
				(iR3Expr_to_id3 newExpr t [] [] toidc3)
			| MdCall (e,args) -> 
				let (newExpr,vars,stmts) = 
					(jlitemdcall_to_IR3Expr classid (e,args) toidc3) in
				(iR3Expr_to_id3 newExpr t vars stmts toidc3)
			| _ -> failwith "Error: Untyped expression"
			end
		| _ -> failwith "Error: Untyped expression"
	  in helper jexp toidc3 toid3

	  
(* Transform a function application to IR3 *) 	  
and jlitemdcall_to_IR3Expr
	(classid: class_name) 
	(exp,args) (toidc3:bool) = 
	let (calleeid, caller, expVars, expStmts) =
	match exp with
		| Var v -> (v, "this", [], [])
		| FieldAccess (e,id) -> 
			let (expIR3, vars, stmts) = 
				(jliteexpr_to_IR3Expr classid e true true) in
			(id, (iR3Expr_get_id3 expIR3), vars, stmts) 
		| _ -> failwith "Error in transforming method call" 
	in
	let rec helper explst =
		match explst with
		| [] -> []
		| arg::tail_lst -> 
			let (argIR3,vars,stmts) = 
				(jliteexpr_to_IR3Expr classid arg true false) in
			let argIdc3 = (iR3Expr_get_idc3 argIR3) in
			 (argIdc3,(vars,stmts)) ::  helper tail_lst
	in let res = ( helper args) in 
	let (paramsIR3, varsstmts) = List.split res in
	let (paramsNewVars, paramsNewStmts) = List.split varsstmts in
		(MdCall3 (string_of_var_id calleeid, (Var3 caller)::paramsIR3),
		 expVars@(List.flatten paramsNewVars),
		 expStmts@(List.flatten paramsNewStmts))
	
let jlitevar_decl_lst_to_ID3 
	(vlst:var_decl list) 
	:(var_decl3 list) =
	List.map (fun (t,id) -> (t, string_of_var_id id)) vlst

let negate_relational_op op =
	match op with 
	| "<" -> ">="
	| "<=" -> ">"
	| ">" -> "<="
	| ">=" -> "<"
	| "==" -> "!="
	| "!=" -> "=="
	| _ -> op
	
let negate_relational_exp 
	e =
	match e with 
	| Idc3Expr ie ->
		begin
		match ie with
		| BoolLiteral3 true -> Idc3Expr (BoolLiteral3 false)
		| BoolLiteral3 false -> Idc3Expr (BoolLiteral3 true)
		| Var3 v  -> (BinaryExp3 (RelationalOp"==", Var3 v,BoolLiteral3 false))
		| _ -> failwith "Eror in IR3 expression generation. Should be boolean." 
		end
	| BinaryExp3 (op,idc1,idc2) -> 		
		begin
			match op with 
			| RelationalOp opid -> 
				let negop =  RelationalOp (negate_relational_op opid) in
				BinaryExp3 (negop,idc1,idc2)
			| _ -> failwith "Eror in IR3 expression generation. Should be relational operator." 
		end
	| _ -> failwith "Eror in IR3 expression generation." 
	
		
(* Transform a list of statements to IR3 *) 
let rec jlitestmts_to_IR3Stmts 
	(classid: class_name) 
	(mthd: md_decl) 
	(stmtlst:jlite_stmt list)
	: (var_decl3 list * ir3_stmt list) =
	match stmtlst with
		| [] -> ([],[])
		| s::tail_lst -> 
			let rec helper s 
			:( var_decl3 list * ir3_stmt list) =
			match s with
			| IfStmt (e, stmts1, stmts2) -> 
				let (expr3,exprvars,exprstmts) = 
					(jliteexpr_to_IR3Expr classid e true true) in
				let negatedExp = (negate_relational_exp expr3) in 
				let (thenvars,thenstmst) = 
					(jlitestmts_to_IR3Stmts classid mthd stmts1) in 
				let (elsevars,elsestmts) = 
					(jlitestmts_to_IR3Stmts classid mthd stmts2) in
				let gotolabel = fresh_label() in
				let endlabel =  fresh_label() in
				let ifIR3 = (IfStmt3 (negatedExp, gotolabel)) in
				let gotoEndIR3 = (GoTo3 endlabel) in 
				(exprvars@thenvars@elsevars,
					exprstmts@(ifIR3::thenstmst)
					@(gotoEndIR3::((Label3 gotolabel)::elsestmts))
					@[Label3 endlabel]
				 )
			| WhileStmt (e, stmts) -> 
				let negatedexp = 
					TypedExp (UnaryExp (UnaryOp "!",e), BoolT) in
				let (expr3,exprvars,exprstmts) = 
					(jliteexpr_to_IR3Expr classid negatedexp false false) in
				let (vars,stmst) = 
					(jlitestmts_to_IR3Stmts classid mthd stmts) in 
				let looplabel = fresh_label() in
				let endlabel =  fresh_label() in
				let ifIR3 = (IfStmt3 (expr3, endlabel)) in
				let gotoLoopIR3 = (GoTo3 looplabel) in 
				(exprvars@vars,
					(Label3 looplabel::exprstmts)@(ifIR3::stmst)
					@[gotoLoopIR3]@[(Label3 endlabel)]
				 )
			| ReturnStmt e ->  
				let (expr3,exprvars,exprstmts) = 
					(jliteexpr_to_IR3Expr classid e true true) in 
				let retIR3 = (ReturnStmt3 (iR3Expr_get_id3 expr3)) in 
				(exprvars,exprstmts@[retIR3])
			| ReturnVoidStmt ->  
				([], [ReturnVoidStmt3])
			| AssignStmt (id,e) ->  
				let (expr3,exprvars,exprstmts) = 
					(jliteexpr_to_IR3Expr classid e false false) in 
				begin
				let assignIR3 = match id with
				| TypedVarId (id1,t,1) -> 
					AssignFieldStmt3 (FieldAccess3 ("this",id1), expr3)
				| TypedVarId (id1,_,2) | SimpleVarId id1 -> 
					(AssignStmt3 (id1, expr3))
				| _ -> failwith "Eror in IR3 expression generation." 
				in (exprvars, exprstmts@[assignIR3])	
				end
			| AssignFieldStmt (id,e) ->  
				let (idIR3,idvars,idstmts) = 
					(jliteexpr_to_IR3Expr classid id false false) in 
				let (expr3,exprvars,exprstmts) = 
					(jliteexpr_to_IR3Expr classid e false false) in 
				let assignIR3 = (AssignFieldStmt3 (idIR3, expr3)) in 
				(idvars@exprvars, idstmts@exprstmts@[assignIR3])	
			| ReadStmt id -> 
				let (idir3,idvars,idstmts) = 
					(jlitevarid_to_IR3Expr classid id true) in 	
				let readIR3 = (ReadStmt3 (iR3Expr_get_id3 idir3)) in 
				(idvars,idstmts@[readIR3])
			| PrintStmt e ->  
				let (expr3,exprvars,exprstmts) = 
					(jliteexpr_to_IR3Expr classid e true false) in 
				let printIR3 = (PrintStmt3 (iR3Expr_get_idc3 expr3)) in 
				(exprvars,exprstmts@[printIR3])
			| MdCallStmt e ->  
				let (expr3,exprvars,exprstmts) = 
					(jliteexpr_to_IR3Expr classid e false false) in 
				let printIR3 = (MdCallStmt3 expr3) in 
				(exprvars,exprstmts@[printIR3])
			
		  in let (vars,stmts) = (helper s) in
		  let (tailvars,tailstmts) = 
			(jlitestmts_to_IR3Stmts classid mthd tail_lst) in
		 (vars@tailvars,stmts@tailstmts)
  
(* Transform a method to IR3 *) 
let jlite_mddecl_to_IR3 cname m  = 
	let (newvars,newstmts) = 
		(jlitestmts_to_IR3Stmts cname m m.stmts)
	in { id3= string_of_var_id m.ir3id;
		 rettype3=m.rettype; 
		 params3=(ObjectT cname, "this")::
			(jlitevar_decl_lst_to_ID3 m.params);
		 localvars3=
			(jlitevar_decl_lst_to_ID3 m.localvars)@newvars; 
		 ir3stmts=newstmts;
		}

(* Transform a JLite program to IR3 *) 
let jlite_program_to_IR3 (p:jlite_program):ir3_program=
	let jlite_class_main_to_IR3 
		((cname,mmthd):class_main ) =
		 ((cname,[]),
			(jlite_mddecl_to_IR3 cname mmthd )) in
	let rec jlite_class_decl_to_IR3 
		((cname,cvars,cmthds):class_decl) =
		let rec helper mthdlst =
			match mthdlst with 
			| [] -> []
			| m::tail_rest -> 
				(jlite_mddecl_to_IR3 cname m)::
					( helper tail_rest)
		in ((cname,
			(jlitevar_decl_lst_to_ID3 cvars)),
			(helper cmthds))
	in 
	begin
		let (mainclass, classes) = p in 
		let (newmainir3, newmainmdir3) =
			(jlite_class_main_to_IR3 mainclass) in
		let newir3classesLst = 
			(List.map jlite_class_decl_to_IR3 classes) in
		let (newclasses,newmethods) = 
			(List.split newir3classesLst) in 
		(newmainir3::newclasses,newmainmdir3,
			(List.flatten newmethods))
	end
	
  *)
