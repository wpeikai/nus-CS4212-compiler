
(* ===================================================== *)
(* ============== CS4212 Compiler Design ============== *)
(*     Translate a typed of Jlite tree to a IR tree *)
(* ===================================================== *)

open Jlite_structs
open Jlite_simple_annotatedtyping
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
let create_temp_id3 (expr_ir3: ir3_exp) (expr_type: ir3_type) (counter_var:int ref) :(ir3_stmt list) * id3 = 
	match expr_ir3 with
				| Idc3Expr (Var3 s) -> [], s
				| _ ->
					let id3_var : id3 = create_temp_var counter_var in
					let stmt = AssignDeclStmt3 (expr_type, id3_var, expr_ir3) in
					[stmt], id3_var
				
(* Create a temporary variable of type idc3 *)
let create_temp_idc3 (expr_ir3: ir3_exp) (expr_type: ir3_type) (counter_var:int ref) :(ir3_stmt list) * idc3 = 
	let stmt_list, id3_var = create_temp_id3 expr_ir3 expr_type counter_var in
	stmt_list, Var3 id3_var

(* Convert a jlite expression to an ir3 expression. Return the type of the expression and all the statements needed to do the conversion. *)
let rec convert_jlite_expr (exp_jlite: jlite_exp) (counter_var:int ref) (counter_label:int ref) (p:jlite_program) (decl_md:md_decl): ir3_type * ir3_exp * (ir3_stmt list)=
	match exp_jlite with 
	| TypedExp (typed_exp_jlite_exp, typed_exp_jlite_type) ->
		let type_ir3 = convert_jlite_type typed_exp_jlite_type in
		begin match typed_exp_jlite_exp with
			| UnaryExp (operator_jlite, exp_unary) ->
				begin 
				let expr_ir3_type, expr_ir3, ir3_stmt_list = convert_jlite_expr exp_unary counter_var counter_label p decl_md in
				let stmt_list, idc3_var = create_temp_idc3 expr_ir3 expr_ir3_type counter_var in
				type_ir3, UnaryExp3 ((convert_jlite_op operator_jlite), idc3_var), stmt_list @ ir3_stmt_list
				end
			| BinaryExp (operator_jlite, exp_binary_1, exp_binary_2) ->

 				let type_binary_1, new_exp_binary_1, stmt_list_1 = convert_jlite_expr exp_binary_1 counter_var counter_label p decl_md in
				let type_binary_2, new_exp_binary_2, stmt_list_2 = convert_jlite_expr exp_binary_2 counter_var counter_label p decl_md in
				
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

					let stmt_list_create_temp_false, id3_expr = create_temp_id3 (Idc3Expr (BoolLiteral3 false)) BoolT counter_var in 

					let stmt_list_create_temp_true = AssignDeclStmt3 (BoolT, id3_expr, new_exp_binary_2) in

					let if_else_stmt = ifstmt :: stmt_list_create_temp_false @ (GoTo3 label_int_next) :: label_if :: stmt_list_create_temp_true :: [label_next] in

					type_ir3, Idc3Expr (Var3 id3_expr), stmt_list_1 @ stmt_list_2 @ if_else_stmt
				| BooleanOp "||" -> 
					(* Create the labels *)
					let label_int_if = create_label counter_label in
					let label_if = Label3 label_int_if in
					let ifstmt = IfStmt3 (new_exp_binary_1, label_int_if) in

					let label_int_next = create_label counter_label  in
					let label_next  = Label3 label_int_next in

					let stmt_list_create_temp_true, id3_expr = create_temp_id3 (Idc3Expr (BoolLiteral3 true)) BoolT counter_var in 

					let stmt_list_create_temp_false = AssignDeclStmt3 (BoolT, id3_expr, new_exp_binary_2) in

					let if_else_stmt = ifstmt :: stmt_list_create_temp_true @ (GoTo3 label_int_next) :: label_if :: stmt_list_create_temp_false :: [label_next] in
					type_ir3, Idc3Expr (Var3 id3_expr), stmt_list_1 @ stmt_list_2 @ if_else_stmt
				| _ -> 
					(* Normal case *)
					let stmt_list_create_temp_1, idc3_var1 = create_temp_idc3 new_exp_binary_1 type_binary_1 counter_var in
					let stmt_list_create_temp_2, idc3_var2 = create_temp_idc3 new_exp_binary_2 type_binary_2 counter_var in

					type_ir3, BinaryExp3 ((convert_jlite_op operator_jlite), idc3_var1, idc3_var2), stmt_list_1 @ stmt_list_2 @ stmt_list_create_temp_1 @ stmt_list_create_temp_2
				end
			| FieldAccess (jexp, vid) ->
				let ir3_type_exp, id3_expr_, ir3_stmt_list = convert_jlite_expr jexp counter_var counter_label p decl_md in
				let stmt_list, id3_var = create_temp_id3 id3_expr_ ir3_type_exp counter_var in
				type_ir3, FieldAccess3 (id3_var, convert_jlite_var_id vid), ir3_stmt_list @ stmt_list
			| ObjectCreate cname -> type_ir3, ObjectCreate3 cname, []
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
				let exp_class_method_jlite, class_method_var, stmt_convert_list_0 = convert_jlite_expr exp_class_method_jlite counter_var counter_label p decl_md in
				let stmt_convert_list_1, class_method_var = create_temp_idc3 class_method_var exp_class_method_jlite counter_var in
				(* Let s convert the params *)
				let convert_param_list param =
					let type_param, exp_param, stmt_list  = convert_jlite_expr param counter_var counter_label p decl_md in
					let stmt_list_temp, idc3_temp = create_temp_idc3 exp_param type_param counter_var in
					idc3_temp, stmt_list_temp @ stmt_list
				in
				let param_list_converted = List.map convert_param_list exp_list_mdcall in

				(* Retrieve all the stmt from the param conversion *)
				let get_stmt_expr_list x =
					let _, s = x  in
					s
				in
				let stmt_list_param = List.map get_stmt_expr_list param_list_converted in
				
				(* Flatten a list *)
				let rec flatten foo = match foo with
  					| [] -> []
  					| h::tail -> h @ (flatten tail) in
  				let all_stmts = flatten stmt_list_param in
  				(* Retrieve all the idc3 vars from the param conversion *)
				let get_idc3_expr_list x =
					let v,_  = x  in
					v
				in
				let idc3_list_param = List.map get_idc3_expr_list param_list_converted in
				return_method_type, MdCall3 (id3_method_name, class_method_var :: idc3_list_param), stmt_convert_list_0 @ stmt_convert_list_1 @ all_stmts
				end
			| BoolLiteral b ->
				type_ir3, Idc3Expr (BoolLiteral3 b), []
			| IntLiteral i ->
				type_ir3, Idc3Expr (IntLiteral3 i), []
			| StringLiteral s ->
				type_ir3, Idc3Expr (StringLiteral3 s), []
			| NullWord -> type_ir3, Idc3Expr (Var3 "null"), []

			| ThisWord -> type_ir3, Idc3Expr (Var3 "this"), []
			| Var vid ->
				(* If the variable is not in the local variables declarations or in the parameters, it must be in a class attribute *)
				let all_declared_vars : var_decl list = decl_md.localvars @ decl_md.params in
				if (exists_var_id all_declared_vars vid)
				then type_ir3, Idc3Expr (Var3 (convert_jlite_var_id vid)), []
				else type_ir3, FieldAccess3 ("this", (convert_jlite_var_id vid)), []
			| _ -> failwith ("Cannot convert " ^ (string_of_jlite_expr exp_jlite))
		end
	| _ -> failwith ("Cannot convert " ^ (string_of_jlite_expr exp_jlite))

(* Convert a list of statements *)
let rec convert_stmts_list (stmts_list: jlite_stmt list) (counter_var:int ref) (counter_label:int ref) (p:jlite_program) (md_decl_:md_decl): (ir3_stmt list) =
	match stmts_list with
	| head::tail -> let head_stmt_list = 
		match head with
			| IfStmt (bool_exp_if, stmt_jlite_if, stmt_jlite_else) -> 
				(* Convert the boolean expression *)
				let _, bool_exp_if_ir3, stmt_list_if = convert_jlite_expr bool_exp_if counter_var counter_label p md_decl_ in

				(* Create the labels *)
				let label_int_if = create_label counter_label in
				let label_if = Label3 label_int_if in
				let ifstmt = IfStmt3 (bool_exp_if_ir3, label_int_if) in

				let label_int_next = create_label counter_label  in
				let label_next  = Label3 label_int_next in

				(* Convert both statements*)
				let stmt_if_list_ir3 = convert_stmts_list stmt_jlite_if counter_var counter_label p md_decl_ in
				let stmt_else_list_ir3 = convert_stmts_list stmt_jlite_else counter_var counter_label p md_decl_ in

				stmt_list_if @  ifstmt :: stmt_else_list_ir3 @ (GoTo3 label_int_next) :: label_if :: stmt_if_list_ir3 @ [label_next]
			| WhileStmt (bool_while_exp, stmt_jlite_list) ->
				(* Convert the boolean expression *)
				let _, bool_while_exp_ir3, stmt_list_boolean_exp = convert_jlite_expr bool_while_exp counter_var counter_label p md_decl_ in

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
				let stmt_ir3_list = convert_stmts_list stmt_jlite_list counter_var counter_label p md_decl_ in

				stmt_list_boolean_exp @  if_stmt_start :: (GoTo3 end_loop) :: label_if_loop :: stmt_ir3_list @ if_stmt_end :: [label_end_loop]
			| ReadStmt var_read ->
				[ReadStmt3 (convert_jlite_var_id var_read)]
			| PrintStmt exp_print_stmt_jlite->
				let type_print_stmt, exp_print_stmt_ir3, ir3_stmt_list = convert_jlite_expr exp_print_stmt_jlite counter_var counter_label p md_decl_ in
				ir3_stmt_list @ [PrintStmt3 idc3_printstmt]
			| AssignStmt (var_assig_stmt_jlite, exp_assign_stmt_jlite) ->
				let var_assig_stmt_ir3 = convert_jlite_var_id var_assig_stmt_jlite in
				let _, exp_assign_stmt_ir3, stmt_list = convert_jlite_expr exp_assign_stmt_jlite counter_var counter_label p md_decl_ in
				stmt_list @ [AssignStmt3 (var_assig_stmt_ir3, exp_assign_stmt_ir3)]
			| AssignFieldStmt (var_assig_field_jlite_1, var_assign_field_jlite_2) ->
				let _, var_assig_field_ir3_1, stmt_list_1 = convert_jlite_expr var_assig_field_jlite_1 counter_var counter_label p md_decl_ in
				let _, var_assig_field_ir3_2, stmt_list_2 = convert_jlite_expr var_assign_field_jlite_2 counter_var counter_label p md_decl_ in
				stmt_list_1 @ stmt_list_2 @ [AssignFieldStmt3 (var_assig_field_ir3_1, var_assig_field_ir3_2)]
			| MdCallStmt exp_md_call_jlite ->
				let _, exp_md_call_ir3, stmt_list = convert_jlite_expr exp_md_call_jlite counter_var counter_label p md_decl_ in
				stmt_list @ [MdCallStmt3 exp_md_call_ir3]
			| ReturnStmt exp_return_jlite ->
				let type_return_jlite, exp_return_ir3, stmt_list = convert_jlite_expr exp_return_jlite counter_var counter_label p md_decl_ in
				let stmt_list_create_tmp, id3_var_temp = create_temp_id3 exp_return_ir3 type_return_jlite counter_var in
				stmt_list @ stmt_list_create_tmp @ [ReturnStmt3 id3_var_temp]
			| ReturnVoidStmt -> [ReturnVoidStmt3]
		in head_stmt_list @ (convert_stmts_list tail counter_var counter_label p md_decl_)
	| [] -> []

(* Convert a method declaration *)
let convert_md_decl (md: md_decl) (counter_var:int ref) (counter_label: int ref) (cname: class_name) (p:jlite_program): md_decl3 = 
	{ rettype3= convert_jlite_type md.rettype; 
	  id3 = (convert_jlite_var_id md.ir3id);
	  params3= (ObjectT cname, "this") :: convert_var_decl_list md.params;				
	  localvars3 = convert_var_decl_list md.localvars;				
	  ir3stmts = convert_stmts_list md.stmts counter_var counter_label p md;}

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
let convert_jlite_typed_program (p: jlite_program) : ir3_program =
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