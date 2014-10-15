
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


let rec convert_var_decl_list (var_decl_list: var_decl list) : (var_decl3 list) = 
	match var_decl_list with
	| [] -> []
	| head :: tail -> (convert_jlite_var_decl head) :: (convert_var_decl_list tail)

let create_temp_var (counter) : string =
	counter := !counter + 1;
	"_zz" ^ (string_of_int (!counter - 1))

let create_label (counter) : label3 =
	counter := !counter + 1;
	!counter-1

let create_temp_idc3 (expr_ir3: ir3_exp) (expr_type: ir3_type) (counter_var:int ref) :(ir3_stmt list) * idc3 = 
	match expr_ir3 with
				| Idc3Expr s -> [], s
				| _ ->
					let id3_var : id3 = create_temp_var counter_var in
					let stmt = AssignDeclStmt3 (expr_type, id3_var, expr_ir3) in
					[stmt], Var3 id3_var

let create_temp_id3 (expr_ir3: ir3_exp) (expr_type: ir3_type) (counter_var:int ref) :(ir3_stmt list) * id3 = 
	match expr_ir3 with
				| Idc3Expr (Var3 s) -> [], s
				| _ ->
					let id3_var : id3 = create_temp_var counter_var in
					let stmt = AssignDeclStmt3 (expr_type, id3_var, expr_ir3) in
					[stmt], id3_var
				

let rec convert_jlite_expr (j_exp: jlite_exp) (counter_var:int ref) (p:jlite_program) (md_decl_:md_decl): ir3_type * ir3_exp * (ir3_stmt list)=
	match j_exp with 
	| TypedExp (e, jtype) ->
		let ir3_type_ = convert_jlite_type jtype in
		(* print_string ((string_of_jlite_type jtype) ^ "\n"); *)
		(* print_string ((string_of_jlite_expr e) ^ "   a\n"); *)
		begin match e with
			| UnaryExp (jop, jexp) ->
				begin 
				let expr_ir3_type, expr_ir3, ir3_stmt_list = convert_jlite_expr jexp counter_var p md_decl_ in
				let stmt_list, idc3_var = create_temp_idc3 expr_ir3 expr_ir3_type counter_var in
				ir3_type_, UnaryExp3 ((convert_jlite_op jop), idc3_var), stmt_list @ ir3_stmt_list
				end
			| BinaryExp (jop, jexp1, jexp2) ->
				(* print_string ((string_of_jlite_expr e) ^ "\n"); *)
				(* print_string ((string_of_jlite_expr jexp1) ^ "\n"); *)
				(* print_string ((string_of_jlite_expr jexp2) ^ "\n"); *)
				(* print_string "\n"; *)

				let ir3_type1, ir3_expr1, ir3_stmt_list1 = convert_jlite_expr jexp1 counter_var p md_decl_ in
				let ir3_type2, ir3_expr2, ir3_stmt_list2 = convert_jlite_expr jexp2 counter_var p md_decl_ in

				let stmt_list1_create_temp, idc3_var1 = create_temp_idc3 ir3_expr1 ir3_type1 counter_var in
				let stmt_list2_create_temp, idc3_var2 = create_temp_idc3 ir3_expr2 ir3_type2 counter_var in

				(* TODO Boolean short circuiting *)
				ir3_type_, BinaryExp3 ((convert_jlite_op jop), idc3_var1, idc3_var2), ir3_stmt_list1 @ ir3_stmt_list2 @ stmt_list1_create_temp @ stmt_list2_create_temp
			| FieldAccess (jexp, vid) ->
				let ir3_type_exp, id3_expr_, ir3_stmt_list = convert_jlite_expr jexp counter_var p md_decl_ in
				let stmt_list, id3_var = create_temp_id3 id3_expr_ ir3_type_exp counter_var in
				ir3_type_, FieldAccess3 (id3_var, convert_jlite_var_id vid), ir3_stmt_list @ stmt_list
			| ObjectCreate cname -> ir3_type_, ObjectCreate3 cname, []
			| MdCall (jexp, jexp_list) ->
			(* type of jexp is the class with the method*)
			    begin
				(* let class_type, _, _ = convert_jlite_expr jexp counter_var p md_decl_ in *)
				let get_jlite_type x = match x with
					| TypedExp (_, e) -> e
					| _ -> failwith  "6"
				in
				let id3_method_name, return_method_type, v_name = match jexp with
				| TypedExp (exp, class_type_) -> 
						print_string ((string_of_jlite_expr exp) ^ "\n");
						begin 
						let cname = match class_type_ with
						 | ObjectT c -> c
						 | _ -> failwith "3"
						in 
						 
						match exp with
						| FieldAccess (class_expr, method_name) ->
							let e_type_list = List.map get_jlite_type jexp_list in
							let method_decl = find_md_ids_params_in_p p cname method_name e_type_list in
							convert_jlite_var_id method_decl.ir3id, convert_jlite_type method_decl.rettype, class_expr
						| _ -> failwith "9"
						end
				| _ -> failwith "88"
				in
				let v_name_type, v_name_ir3, v_name_stmts = convert_jlite_expr v_name counter_var p md_decl_ in
				let v_name_idc3_stmt_list, v_name_idc3 = create_temp_idc3 v_name_ir3 v_name_type counter_var in
				let handles_expr x =
					let ir3_type_, id3_expr_, ir3_stmts_list  = convert_jlite_expr x counter_var p md_decl_  in
					let stmt_list_temp, idc3_var_temp = create_temp_idc3 id3_expr_ ir3_type_ counter_var in
					idc3_var_temp, stmt_list_temp @ ir3_stmts_list
				in
				let alllll = List.map handles_expr jexp_list in

				let get_stmt_expr_list x =
					let _, s = x  in
					s
				in
				let get_idc3_expr_list x =
					let a,_  = x  in
					a
				in
				let all_stmts_list = List.map get_stmt_expr_list alllll in
				let rec flatten a = match a with
  					| [] -> []
  					| h::tail -> h @ (flatten tail) in
  				let all_stmts = flatten all_stmts_list in
				let all_idc3 = List.map get_idc3_expr_list alllll in

				return_method_type, MdCall3 (id3_method_name, v_name_idc3 :: all_idc3), v_name_stmts @ v_name_idc3_stmt_list @ all_stmts
				end
			| BoolLiteral b ->
				ir3_type_, Idc3Expr (BoolLiteral3 b), []
			| IntLiteral i ->
				ir3_type_, Idc3Expr (IntLiteral3 i), []
			| StringLiteral s ->
				ir3_type_, Idc3Expr (StringLiteral3 s), []
			| NullWord -> ir3_type_, Idc3Expr (Var3 "NULL"), []

			| ThisWord -> ir3_type_, Idc3Expr (Var3 "this"), []
			| Var vid ->
				let all_declared_vars : var_decl list = md_decl_.localvars @ md_decl_.params in
				if (exists_var_id all_declared_vars vid)
				then ir3_type_, Idc3Expr (Var3 (convert_jlite_var_id vid)), []
				else ir3_type_, FieldAccess3 ("this", (convert_jlite_var_id vid)), []
			| _ -> failwith "99"
		end
	| _ -> failwith ("\nFail 511\n" ^ Jlite_structs.string_of_jlite_expr j_exp)


let rec convert_stmts_list (stmts_list: jlite_stmt list) (counter_var:int ref) (counter_label:int ref) (p:jlite_program) (md_decl_:md_decl): (ir3_stmt list) =

	(* Function begin here *)
	match stmts_list with
	| head::tail -> let head_stmt_list = 
		match head with
		| IfStmt (exp1, jsmtm1_list, jsmtm_list_else) -> 
			let _, exprir3, stmts_list = convert_jlite_expr exp1 counter_var p md_decl_ in
			let label_int_if = create_label counter_label in
			let label_if = Label3 label_int_if in
			let ifstmt = IfStmt3 (exprir3, label_int_if) in

			let label_int_next = create_label counter_label  in
			let label_next  = Label3 label_int_next in

			let stmt1_ir3_list = convert_stmts_list jsmtm1_list counter_var counter_label p md_decl_ in
			let stmt_ir3_list_else = convert_stmts_list jsmtm_list_else counter_var counter_label p md_decl_ in

			stmts_list @  ifstmt :: stmt_ir3_list_else @ (GoTo3 label_int_next) :: label_if :: stmt1_ir3_list @ [label_next]
		| WhileStmt (jexp, jstmt_list) ->
			let _, id3_expr, jexp_stmts = convert_jlite_expr jexp counter_var p md_decl_ in
			let if_loop = create_label counter_label  in
			let label_if_loop = Label3 if_loop in
			let end_loop = create_label counter_label  in
			let label_end_loop = Label3 end_loop in
			let if_stmt_start = IfStmt3 (id3_expr, if_loop) in
			let if_stmt_end = IfStmt3 (id3_expr, if_loop) in
			let stmt_ir3_list = convert_stmts_list jstmt_list counter_var counter_label p md_decl_ in
			jexp_stmts @  if_stmt_start :: (GoTo3 end_loop) :: label_if_loop :: stmt_ir3_list @ if_stmt_end :: [label_end_loop]
		| ReadStmt vid ->
			[ReadStmt3 (convert_jlite_var_id vid)]
		| PrintStmt jexp->
			let ir3_type_, ir3_expr_, ir3_stmt_list = convert_jlite_expr jexp counter_var p md_decl_ in
			let stmt_list_create_tmp, idc3_var = create_temp_idc3 ir3_expr_ ir3_type_ counter_var in
			stmt_list_create_tmp @ [PrintStmt3 idc3_var]
		| AssignStmt (vid, jexp) ->
			let id3_ = convert_jlite_var_id vid in
			let ir3_type_, ir3_expr_, ir3_stmt_list = convert_jlite_expr jexp counter_var p md_decl_ in
			ir3_stmt_list @ [AssignStmt3 (id3_, ir3_expr_)]
		| AssignFieldStmt (jexp1, jexp2) ->
			let _, ir3_expr1, ir3_stmt_list1 = convert_jlite_expr jexp1 counter_var p md_decl_ in
			let _, ir3_expr2, ir3_stmt_list2 = convert_jlite_expr jexp2 counter_var p md_decl_ in
			ir3_stmt_list1 @ ir3_stmt_list2 @ [AssignFieldStmt3 (ir3_expr1, ir3_expr2)]
		| MdCallStmt jexp ->
			let _, ir3_expr_, ir3_stmt_list = convert_jlite_expr jexp counter_var p md_decl_ in
			ir3_stmt_list @ [MdCallStmt3 ir3_expr_]
		| ReturnStmt jexp ->
			let ir3_type_, ir3_expr_, ir3_stmt_list = convert_jlite_expr jexp counter_var p md_decl_ in
			let stmt_list_create_tmp, id3_var = create_temp_id3 ir3_expr_ ir3_type_ counter_var in
			ir3_stmt_list @ stmt_list_create_tmp @ [ReturnStmt3 id3_var]
		| ReturnVoidStmt -> [ReturnVoidStmt3]
	in head_stmt_list @ (convert_stmts_list tail counter_var counter_label p md_decl_)
	| [] -> []

let convert_md_decl (md: md_decl) (counter_var:int ref) (counter_label: int ref) (cname: class_name) (p:jlite_program): md_decl3 = 
	{ rettype3= convert_jlite_type md.rettype; 
	  id3 = (convert_jlite_var_id md.ir3id);
	  params3= (ObjectT cname, "this") :: convert_var_decl_list md.params;				
	  localvars3 = convert_var_decl_list md.localvars;				
	  ir3stmts = convert_stmts_list md.stmts counter_var counter_label p md;}


let rec convert_md_decl_list (md_decl_list: md_decl list) (counter_var:int ref) (counter_label:int ref) (cname: class_name) (p:jlite_program): (md_decl3 list) = 
	match md_decl_list with
	| head::tail -> (convert_md_decl head counter_var counter_label cname p) :: (convert_md_decl_list tail counter_var counter_label cname p)
	| [] -> []

let convert_class_decl (class_decl: class_decl) (counter_var:int ref) (counter_label:int ref) (p:jlite_program): cdata3 * (md_decl3 list) =
	let c_name, var_decl_l, md_decl_l = class_decl
	in let var_decl3_list = convert_var_decl_list var_decl_l
	in let md_decl3_list = convert_md_decl_list md_decl_l counter_var counter_label c_name p
	in ((c_name, var_decl3_list), md_decl3_list)


let convert_jlite_typed_program (p: jlite_program) : ir3_program =
    let counter_var : int ref = ref 0 in
    let counter_label : int ref = ref 0 in

	let class_main_, class_decl_list = p in
	let convert_class_main (class_main_:class_main) (counter_var:int ref) (p:jlite_program):  (cdata3 * md_decl3) =
		let c_name, _md_decl_main = class_main_ in
		((c_name, []), convert_md_decl _md_decl_main counter_var counter_label c_name p)
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