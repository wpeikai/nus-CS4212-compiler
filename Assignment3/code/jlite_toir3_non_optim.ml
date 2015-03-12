
(* ===================================================== *)
(* ============== CS4212 Compiler Design ============== *)
(*     Translate a typed of Jlite tree to a IR tree *)
(* ===================================================== *)

open Jlite_structs
open Jlite_annotatedtyping
open Ir3_structs


(* Convert jlite_type to ir3_type *)	
let convert_jlite_type (jlite_type_: jlite_type) : ir3_type =
	jlite_type_

(* Convert jlite_op to ir3_op *)	
let convert_jlite_op (op: jlite_op) : ir3_op =
	op

(* Convert var_id to ir3 *)	
let convert_jlite_var_id (vid: var_id) : id3 =
	match vid with
	| SimpleVarId v ->  v
	| TypedVarId (s, jtype, i)->  s

(* Convert var_id to ir3 *)	
let convert_jlite_typed_var_id (vid: var_id) : id3 * jlite_type =
	match vid with
	| TypedVarId (s, jtype, i)->  s, jtype
	| _ -> failwith "6"

(* Convert var_decl to var_decl3 *)	
let convert_jlite_var_decl (vd: var_decl) : var_decl3 =
	let (jtype, vid) = vd in
	jtype, (convert_jlite_var_id vid)

(* Convert var_decl list to var_decl3 list*)	
let rec convert_var_decl_list (var_decl_list: var_decl list) : (var_decl3 list) = 
	match var_decl_list with
	| [] -> []
	| head :: tail -> (convert_jlite_var_decl head) :: (convert_var_decl_list tail)

(* Create a temporary variable. Keep track of the count. *)
let create_temp_var (counter) : string =
	counter := !counter + 1;
	"_zz" ^ (string_of_int (!counter - 1))

(* Create a label integer. Keep track of the count. *)
let create_label (counter) : label3 =
	counter := !counter + 1;
	!counter-1

(* Create a temporary variable of type id3 *)
let create_temp_id3 (expr_ir3: ir3_exp) (expr_type: ir3_type) (counter_var:int ref) :(ir3_stmt list) * id3 * (var_decl3 list) = 
	match expr_ir3 with
				| Idc3Expr (Var3 s) -> [], s, []
				| _ ->
					let id3_var : id3 = create_temp_var counter_var in
					let stmt = AssignStmt3 (id3_var, expr_ir3) in
					[stmt], id3_var, [(expr_type, id3_var)]
				
(* Create a temporary variable of type idc3 *)
let create_temp_idc3 (expr_ir3: ir3_exp) (expr_type: ir3_type) (counter_var:int ref) :(ir3_stmt list) * idc3 * (var_decl3 list) = 
	let stmt_list, id3_var, local_vars = create_temp_id3 expr_ir3 expr_type counter_var in
	stmt_list, Var3 id3_var, local_vars

(* Convert a jlite expression to an ir3 expression. Return the type of the expression and all the statements needed to do the conversion. *)
let rec convert_jlite_expr (exp_jlite: jlite_exp) (counter_var:int ref) (counter_label:int ref) (p:jlite_program) (decl_md:md_decl): ir3_type * ir3_exp * (ir3_stmt list) *  (var_decl3 list)=
	match exp_jlite with 
	| TypedExp (typed_exp_jlite_exp, typed_exp_jlite_type) ->
		let type_ir3 = convert_jlite_type typed_exp_jlite_type in
		begin match typed_exp_jlite_exp with
			| UnaryExp (operator_jlite, exp_unary) ->
				begin 
				let expr_ir3_type, expr_ir3, ir3_stmt_list, localvars_0 = convert_jlite_expr exp_unary counter_var counter_label p decl_md in
				let stmt_list, idc3_var, localvars_1 = create_temp_idc3 expr_ir3 expr_ir3_type counter_var in
				type_ir3, UnaryExp3 ((convert_jlite_op operator_jlite), idc3_var), ir3_stmt_list @ stmt_list, localvars_0 @ localvars_1
				end
			| BinaryExp (operator_jlite, exp_binary_1, exp_binary_2) ->

 				let type_binary_1, new_exp_binary_1, stmt_list_1, localvars_0 = convert_jlite_expr exp_binary_1 counter_var counter_label p decl_md in
				let type_binary_2, new_exp_binary_2, stmt_list_2, localvars_1 = convert_jlite_expr exp_binary_2 counter_var counter_label p decl_md in
				
				(* Boolean short-circuiting *)
				begin
				match operator_jlite with
				| BooleanOp "&&"  ->
					(* Create the labels *)
					let label_int_if = create_label counter_label in
					let label_if = Label3 label_int_if in
					let ifstmt = IfStmt3 (new_exp_binary_1, label_int_if) in

					let label_int_next = create_label counter_label  in
					let label_next  = Label3 label_int_next in

					let stmt_list_create_temp_false, id3_expr, localvars_2 = create_temp_id3 (Idc3Expr (BoolLiteral3 false)) BoolT counter_var in 

					let stmt_list_create_temp_true = AssignStmt3 (id3_expr, new_exp_binary_2) in

					let if_else_stmt = ifstmt :: stmt_list_create_temp_false @ (GoTo3 label_int_next) :: label_if :: stmt_list_create_temp_true :: [label_next] in

					type_ir3, Idc3Expr (Var3 id3_expr), stmt_list_1 @ stmt_list_2 @ if_else_stmt, localvars_0 @ localvars_1 @ localvars_2
				| BooleanOp "||" -> 
					(* Create the labels *)
					let label_int_if = create_label counter_label in
					let label_if = Label3 label_int_if in
					let ifstmt = IfStmt3 (new_exp_binary_1, label_int_if) in

					let label_int_next = create_label counter_label  in
					let label_next  = Label3 label_int_next in

					let stmt_list_create_temp_true, id3_expr, localvars_2 = create_temp_id3 (Idc3Expr (BoolLiteral3 true)) BoolT counter_var in 

					let stmt_list_create_temp_false = AssignStmt3 (id3_expr, new_exp_binary_2) in

					let if_else_stmt = ifstmt :: stmt_list_create_temp_false :: (GoTo3 label_int_next) :: label_if :: stmt_list_create_temp_true @ [label_next] in
					type_ir3, Idc3Expr (Var3 id3_expr), stmt_list_1 @ stmt_list_2 @ if_else_stmt, localvars_0 @ localvars_1 @ localvars_2
				| _ -> 
					(* Normal case *)
					let stmt_list_create_temp_1, idc3_var1, localvars_2 = create_temp_idc3 new_exp_binary_1 type_binary_1 counter_var in
					let stmt_list_create_temp_2, idc3_var2, localvars_3 = create_temp_idc3 new_exp_binary_2 type_binary_2 counter_var in

					type_ir3, BinaryExp3 ((convert_jlite_op operator_jlite), idc3_var1, idc3_var2), stmt_list_1 @ stmt_list_2 @ stmt_list_create_temp_1 @ stmt_list_create_temp_2, localvars_0 @ localvars_1 @ localvars_2 @ localvars_3
				end
			| FieldAccess (jexp, vid) ->
				(* print_string "\n\n"; *)
				(* print_string (string_of_jlite_expr jexp); *)
				(* print_string "\n"; *)
				(* print_string (string_of_var_id vid); *)
				let ir3_type_exp, id3_expr_, ir3_stmt_list, localvars_0 = convert_jlite_expr jexp counter_var counter_label p decl_md in
				let stmt_list, id3_var, localvars_3 = create_temp_id3 id3_expr_ ir3_type_exp counter_var in
				type_ir3, FieldAccess3 (id3_var, convert_jlite_var_id vid), ir3_stmt_list @ stmt_list, localvars_0 @ localvars_3

			| ObjectCreate cname -> type_ir3, ObjectCreate3 cname, [], []
			| MdCall (exp_mdcall, exp_list_mdcall) ->
			(* type of jexp is the class with the method*)
			    begin
				(* Get the params type of the method call *)
				let get_jlite_type x = match x with
		                | TypedExp (_, t) -> t
		                | _ -> failwith  "6"
				in let type_jlite_param_list = List.map get_jlite_type exp_list_mdcall in
				let id3_method_name, return_method_type, exp_class_method_jlite = match exp_mdcall with
					| TypedExp (exp_mdcall_exp, exp_mdcall_type) ->
						begin 
						let cname = match exp_mdcall_type with
							| ObjectT c -> c
							| _ -> failwith "Cannot convert " ^ (string_of_jlite_expr exp_jlite)
						in 
						
						(* We can only have field access to match : class.method(params) *)
						match exp_mdcall_exp with
							| FieldAccess (exp_field_access, method_name) ->
							(*  Look for the method decl in the program p *)
								let method_declaration_jlite = find_md_ids_params_in_p p cname method_name type_jlite_param_list in
								(* Return the method name, the return type and the field_access expression *)
								convert_jlite_var_id method_declaration_jlite.ir3id, convert_jlite_type method_declaration_jlite.rettype, exp_field_access
							| _ -> failwith ("Cannot convert " ^ (string_of_jlite_expr exp_jlite))
						end
					| _ -> failwith ("Cannot convert " ^ (string_of_jlite_expr exp_jlite))
				in
				(* Convert the class.method part if needed *)
				let exp_class_method_jlite, class_method_var, stmt_convert_list_0, localvars_0 = convert_jlite_expr exp_class_method_jlite counter_var counter_label p decl_md in
				let stmt_convert_list_1, class_method_var, localvars_1 = create_temp_idc3 class_method_var exp_class_method_jlite counter_var in
				(* Let s convert the params *)
				let convert_param_list param =
					let type_param, exp_param, stmt_list, localvars_2  = convert_jlite_expr param counter_var counter_label p decl_md in
					let stmt_list_temp, idc3_temp, localvars_3 = create_temp_idc3 exp_param type_param counter_var in
					idc3_temp, stmt_list @ stmt_list_temp, localvars_2 @ localvars_3
				in
				let param_list_converted = List.map convert_param_list exp_list_mdcall in

				(* Retrieve all the stmt from the param conversion *)
				let get_stmt_expr_list x =
					let _, s, _ = x  in
					s
				in
				let get_local_var_list x =
					let _, _, s = x  in
					s
				in

				let stmt_list_param = List.map get_stmt_expr_list param_list_converted in
				let localvars_4_list = List.map get_local_var_list param_list_converted in
				
				(* Flatten a list *)
				let rec flatten foo = match foo with
  					| [] -> []
  					| h::tail -> h @ (flatten tail) in
  				let all_stmts = flatten stmt_list_param in
  				let localvars_4 = flatten localvars_4_list in
  				(* Retrieve all the idc3 vars from the param conversion *)
				let get_idc3_expr_list x =
					let v,_, _  = x  in
					v
				in
				let idc3_list_param = List.map get_idc3_expr_list param_list_converted in
				return_method_type, MdCall3 (id3_method_name, class_method_var :: idc3_list_param), stmt_convert_list_0 @ stmt_convert_list_1 @ all_stmts, localvars_0 @ localvars_1 @ localvars_4
				end
			| BoolLiteral b ->
				type_ir3, Idc3Expr (BoolLiteral3 b), [], []
			| IntLiteral i ->
				type_ir3, Idc3Expr (IntLiteral3 i), [], []
			| StringLiteral s ->
				type_ir3, Idc3Expr (StringLiteral3 s), [], []
			| NullWord -> type_ir3, Idc3Expr (IntLiteral3 0), [], []
			| ThisWord -> type_ir3, Idc3Expr (Var3 "this"), [], []
			| Var vid ->
				(* If the variable is not in the local variables declarations or in the parameters, it must be in a class attribute *)
				let all_declared_vars : var_decl list = decl_md.localvars @ decl_md.params in
				if (exists_var_id all_declared_vars vid)
				then type_ir3, Idc3Expr (Var3 (convert_jlite_var_id vid)), [], []
				else 
				type_ir3, FieldAccess3 ("this", (convert_jlite_var_id vid)), [], []
				
			| _ -> failwith ("10:Cannot convert " ^ (string_of_jlite_expr exp_jlite))
		end
	| _ -> failwith ("11:Cannot convert " ^ (string_of_jlite_expr exp_jlite))

(* Convert a list of statements *)
let rec convert_stmts_list (stmts_list: jlite_stmt list) (counter_var:int ref) (counter_label:int ref) (p:jlite_program) (md_decl_:md_decl): (ir3_stmt list) *  (var_decl3 list)=
	match stmts_list with
	| head::tail -> let head_stmt_list, localvars_head = 
		begin
		match head with
			| IfStmt (bool_exp_if, stmt_jlite_if, stmt_jlite_else) -> 
				(* Convert the boolean expression *)
				let _, bool_exp_if_ir3, stmt_list_if, localvars_0 = convert_jlite_expr bool_exp_if counter_var counter_label p md_decl_ in

				(* Create the labels *)
				let label_int_if = create_label counter_label in
				let label_if = Label3 label_int_if in
				let ifstmt = IfStmt3 (bool_exp_if_ir3, label_int_if) in

				let label_int_next = create_label counter_label  in
				let label_next  = Label3 label_int_next in

				(* Convert both statements*)
				let stmt_if_list_ir3, localvars_1 = convert_stmts_list stmt_jlite_if counter_var counter_label p md_decl_ in
				let stmt_else_list_ir3, localvars_2 = convert_stmts_list stmt_jlite_else counter_var counter_label p md_decl_ in

				stmt_list_if @  ifstmt :: stmt_else_list_ir3 @ (GoTo3 label_int_next) :: label_if :: stmt_if_list_ir3 @ [label_next], localvars_0 @ localvars_1 @ localvars_2
			| WhileStmt (bool_while_exp, stmt_jlite_list) ->
				(* Convert the boolean expression *)
				let _, bool_while_exp_ir3, stmt_list_boolean_exp, localvars_0 = convert_jlite_expr bool_while_exp counter_var counter_label p md_decl_ in

				(* Create the labels *)
				let if_loop = create_label counter_label  in
				let label_if_loop = Label3 if_loop in
				let end_loop = create_label counter_label  in
				let label_end_loop = Label3 end_loop in

				(* Create the if statements *)
				(* When loop is entered *)
				let if_stmt_start = IfStmt3 (bool_while_exp_ir3, if_loop) in
				(* At then end of the loop *)
				let if_stmt_end = IfStmt3 (bool_while_exp_ir3, if_loop) in

				(* Convert the statements*)
				let stmt_ir3_list, localvars_1 = convert_stmts_list stmt_jlite_list counter_var counter_label p md_decl_ in

				stmt_list_boolean_exp @  if_stmt_start :: (GoTo3 end_loop) :: label_if_loop :: stmt_ir3_list @ if_stmt_end :: [label_end_loop], localvars_0 @ localvars_1
			| ReadStmt var_read ->
				let _, t = convert_jlite_typed_var_id var_read in
				let type_read_stmt, exp_read_stmt_ir3, ir3_stmt_list, localvars_0 = convert_jlite_expr (TypedExp(Var var_read, t)) counter_var counter_label p md_decl_ in
				begin
				match exp_read_stmt_ir3 with
				| Idc3Expr idc3 -> 
					begin
					match idc3 with
					| Var3 id3_readstmt -> ir3_stmt_list @ [ReadStmt3 id3_readstmt], localvars_0
					| _ -> 
						let id3_create_temp_list, id3_readstmt, localvars_1 = create_temp_id3 exp_read_stmt_ir3 type_read_stmt counter_var in
						ir3_stmt_list @ id3_create_temp_list @ [ReadStmt3 id3_readstmt], localvars_0 @ localvars_1
					end
				| FieldAccess3 (_, _) -> 
						let id3_create_temp_list, id3_readstmt, localvars_1 = create_temp_id3 exp_read_stmt_ir3 type_read_stmt counter_var in
						ir3_stmt_list @ id3_create_temp_list @ [ReadStmt3 id3_readstmt], localvars_0 @ localvars_1
				| _ -> failwith ("Unrecognized value " ^ (string_of_var_id var_read))
				end
			| PrintStmt exp_print_stmt_jlite->
				let type_print_stmt, exp_print_stmt_ir3, ir3_stmt_list, localvars_0 = convert_jlite_expr exp_print_stmt_jlite counter_var counter_label p md_decl_ in
				begin
				match exp_print_stmt_ir3 with
				| Idc3Expr idc3_printstmt -> ir3_stmt_list @ [PrintStmt3 idc3_printstmt], localvars_0
				| _ -> 
					let idc3_create_temp_list, idc3_printstmt, localvars_1 = create_temp_idc3 exp_print_stmt_ir3 type_print_stmt counter_var in
					ir3_stmt_list @ idc3_create_temp_list @ [PrintStmt3 idc3_printstmt], localvars_0 @ localvars_1
			 	end
			| AssignStmt (var_assig_stmt_jlite, exp_assign_stmt_jlite) ->
				(* Be careful if the var id is an attribute of the class *)
				(* let var_assig_stmt_ir3 = convert_jlite_var_id var_assig_stmt_jlite in *)
				let _, t = convert_jlite_typed_var_id var_assig_stmt_jlite in
				let var_assign_stmt_ir3_type, var_assign_stmt_ir3, stmt_list_var, localvars_0 = convert_jlite_expr (TypedExp(Var var_assig_stmt_jlite, t)) counter_var counter_label p md_decl_ in
				let id3_create_temp_list, id3_printstmt, localvars_1 = create_temp_id3 var_assign_stmt_ir3 var_assign_stmt_ir3_type counter_var in

				let _, exp_assign_stmt_ir3, stmt_list_exp, localvars_2 = convert_jlite_expr exp_assign_stmt_jlite counter_var counter_label p md_decl_ in
				stmt_list_var @ id3_create_temp_list @ stmt_list_exp @ [AssignStmt3 (id3_printstmt, exp_assign_stmt_ir3)], localvars_0 @ localvars_1 @ localvars_2
			| AssignFieldStmt (var_assig_field_jlite_1, var_assign_field_jlite_2) ->
				let _, var_assig_field_ir3_1, stmt_list_1, localvars_0 = convert_jlite_expr var_assig_field_jlite_1 counter_var counter_label p md_decl_ in
				
				let var_assig_field_ir3_2_type, var_assig_field_ir3_2, stmt_list_2, localvars_1 = convert_jlite_expr var_assign_field_jlite_2 counter_var counter_label p md_decl_ in
				
				let id3_create_temp_list, idc3_temp, localvars_2 = create_temp_idc3 var_assig_field_ir3_2 var_assig_field_ir3_2_type counter_var in

				stmt_list_1 @ stmt_list_2 @ id3_create_temp_list @ [AssignFieldStmt3 (var_assig_field_ir3_1, Idc3Expr idc3_temp)], localvars_0 @ localvars_1 @ localvars_2
			| MdCallStmt exp_md_call_jlite ->
				let _, exp_md_call_ir3, stmt_list, localvars_0 = convert_jlite_expr exp_md_call_jlite counter_var counter_label p md_decl_ in
				stmt_list @ [MdCallStmt3 exp_md_call_ir3], localvars_0
			| ReturnStmt exp_return_jlite ->
				let type_return_jlite, exp_return_ir3, stmt_list, localvars_0 = convert_jlite_expr exp_return_jlite counter_var counter_label p md_decl_ in
				let stmt_list_create_tmp, id3_var_temp, localvars_1 = create_temp_id3 exp_return_ir3 type_return_jlite counter_var in
				stmt_list @ stmt_list_create_tmp @ [ReturnStmt3 id3_var_temp], localvars_0 @ localvars_1
			| ReturnVoidStmt -> [ReturnVoidStmt3], []
		end
		in let tail_stmt_list, localvars_tail = convert_stmts_list tail counter_var counter_label p md_decl_ 
		in head_stmt_list @ tail_stmt_list, localvars_head @ localvars_tail
	| [] -> [], []

(* Convert a method declaration *)
let convert_md_decl (md: md_decl) (counter_var:int ref) (counter_label: int ref) (cname: class_name) (p:jlite_program): md_decl3 = 
	let smtmt_list, localvars_list = convert_stmts_list md.stmts counter_var counter_label p md
	in 
	{ rettype3= convert_jlite_type md.rettype; 
	  id3 = (convert_jlite_var_id md.ir3id);
	  params3= (ObjectT cname, "this") :: convert_var_decl_list md.params;				
	  localvars3 = (convert_var_decl_list md.localvars) @ localvars_list;				
	  ir3stmts = smtmt_list;}

(* Convert a method declaration list *)
let rec convert_md_decl_list (md_decl_list: md_decl list) (counter_var:int ref) (counter_label:int ref) (cname: class_name) (p:jlite_program): (md_decl3 list) = 
	match md_decl_list with
	| head::tail -> (convert_md_decl head counter_var counter_label cname p) :: (convert_md_decl_list tail counter_var counter_label cname p)
	| [] -> []

(* Convert a class declaration *)
let convert_class_decl (class_decl: class_decl) (counter_var:int ref) (counter_label:int ref) (p:jlite_program): cdata3 * (md_decl3 list) =
	let c_name, var_decl_l, md_decl_l = class_decl
	in let var_decl3_list = convert_var_decl_list var_decl_l
	in let md_decl3_list = convert_md_decl_list md_decl_l counter_var counter_label c_name p
	in ((c_name, var_decl3_list), md_decl3_list)

(* Convert a jlite program *)
let jlite_program_to_IR3 (p: jlite_program) : ir3_program =
	(* Track the temporary var count *)
    let counter_var : int ref = ref 0 in
	(* Track the temporary label count *)
    let counter_label : int ref = ref 0 in

    (* Convert the main class *)
	let class_main_, class_decl_list = p in
	let convert_class_main (class_main_:class_main) (counter_var:int ref) (p:jlite_program):  (cdata3 * md_decl3) =
		let c_name, _md_decl_main = class_main_ in
		((c_name, []), convert_md_decl _md_decl_main counter_var counter_label c_name p)
    
    (* Convert all other classes *)
	in let rec convert_class_decl_list (class_decl_list: class_decl list) (counter_var:int ref) (counter_label: int ref) (p:jlite_program): ((cdata3 list) * (md_decl3 list)) =
		begin
			match class_decl_list with 
			| [] -> [], []
			| head :: tail -> 
				let cdata_list, md_decl3_ = convert_class_decl_list tail counter_var counter_label p in
				let cdata, md_decl3_list = convert_class_decl head counter_var counter_label p in
				(cdata:: cdata_list, md_decl3_ @ md_decl3_list)
		end
	in
	let c_main, main_md = convert_class_main class_main_ counter_var p in
	let cdata3_list, md_decl3_list = convert_class_decl_list class_decl_list counter_var counter_label p in
	((c_main :: cdata3_list), main_md, md_decl3_list)