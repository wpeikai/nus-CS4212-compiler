
(* ===================================================== *)
(* ============== CS4212 Compiler Design ============== *)
(*     Translate a typed of Jlite tree to a IR tree *)
(* ===================================================== *)

open Jlite_structs
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


let rec convert_jlite_expr (j_exp: jlite_exp) : ir3_type * ir3_exp * (ir3_stmt list)=
	print_string ((string_of_jlite_expr j_exp) ^ "\n");
	match j_exp with 
	| TypedExp (e, jtype) ->
		let ir3_type_ = convert_jlite_type jtype  in	
		begin match e with
			| UnaryExp (jop, jexp) ->
				let id3_var : id3 = "t1" in
				let idc3_var : idc3 = Var3 id3_var  in
				let ir3_op = convert_jlite_op jop  in
				let _, (expr_ir3:ir3_exp), (ir3_stmt_list : (ir3_stmt list)) = convert_jlite_expr jexp in
				let stmt = AssignDeclStmt3 ((convert_jlite_type jtype), id3_var, expr_ir3) in
				ir3_type_, UnaryExp3 (ir3_op, idc3_var), stmt :: ir3_stmt_list
			| BinaryExp (jop, jexp1, jexp2) ->
				let ir3_type1, ir3_expr1, ir3_stmt_list1 = convert_jlite_expr jexp1 in
				let ir3_type2, ir3_expr2, ir3_stmt_list2 = convert_jlite_expr jexp2 in
				let id31, id32 = "ee", "ff" in
				let id31c, id32c = Var3 id31, Var3 id32 in
				let ass1 = AssignDeclStmt3 (ir3_type1, id31, ir3_expr1) in
				let ass2 = AssignDeclStmt3 (ir3_type2, id32, ir3_expr2) in
				(* TODO Boolean short circuiting *)
				ir3_type_, BinaryExp3 ((convert_jlite_op jop), id31c, id32c), ir3_stmt_list1 @ ir3_stmt_list2 @ (ass1 :: [ass2])
			| FieldAccess (jexp, vid) ->
				let ir3_type_exp, id3_expr_, ir3_stmt_list = convert_jlite_expr jexp in
				let id3_var : id3 = "t1" in
				ir3_type_, FieldAccess3 (id3_var, convert_jlite_var_id vid), ir3_stmt_list @ [AssignDeclStmt3 (ir3_type_, id3_var, id3_expr_)]
			| ObjectCreate cname -> ir3_type_, ObjectCreate3 cname, []
			| MdCall (jexp, jexp_list) ->
			    begin
				let ir3_type_, ir3_expr_, ir3_stmt_list = convert_jlite_expr jexp in
				let id3_var : id3 = "t14" in
				let ass = AssignDeclStmt3 (ir3_type_, id3_var, ir3_expr_) in

				let handles_expr x =
					let ir3_type_, id3_expr_, ir3_stmts_list  = convert_jlite_expr x  in
					let id3_var_temp : id3 = "t1" in
					let idc3_var_temp : idc3 = Var3 id3_var_temp in
					idc3_var_temp, AssignDeclStmt3 (ir3_type_, id3_var_temp, id3_expr_) :: ir3_stmts_list
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

				ir3_type_, MdCall3 (id3_var, all_idc3), (ass :: (ir3_stmt_list @ all_stmts))
				end
			| BoolLiteral b ->
				ir3_type_, Idc3Expr (BoolLiteral3 b), []
			| IntLiteral i ->
				ir3_type_, Idc3Expr (IntLiteral3 i), []
			| StringLiteral s ->
				ir3_type_, Idc3Expr (StringLiteral3 s), []
			| NullWord -> ir3_type_, Idc3Expr (Var3 "NULL"), []

			| ThisWord -> ir3_type_, Idc3Expr (Var3 "THIS"), []
			| Var vid -> ir3_type_, Idc3Expr (Var3 (convert_jlite_var_id vid)), []
			| _ -> failwith ((string_of_jlite_expr e) ^ "\n")
		end
	| _ -> failwith ("\nFail 511\n" ^ Jlite_structs.string_of_jlite_expr j_exp)


let rec convert_stmts_list (stmts_list: jlite_stmt list) : (ir3_stmt list) =

	(* Function begin here *)
	match stmts_list with
	| head::tail -> let head_stmt_list = 
		match head with
		| IfStmt (exp1, jsmtm1_list, jsmtm_list_else) -> 
			let _, exprir3, stmts_list = convert_jlite_expr exp1 in
			let ifstmt = IfStmt3 (exprir3, 1) in
			let label1  = Label3 1 in
			let stmt1_ir3_list = convert_stmts_list jsmtm1_list in
			let stmt_ir3_list_else = convert_stmts_list jsmtm_list_else in
			let returnint = 2 in
			(* ifstmt :: stmt_ir3_list_else @ GoTo3 returnint :: label1 :: (stmt1_ir3_list @ [Label3 returnint]) *)
			stmts_list @  ifstmt :: stmt_ir3_list_else @ label1 :: stmt1_ir3_list
		| WhileStmt (jexp, jstmt_list) ->
			let _, id3_expr, jexp_stmts = convert_jlite_expr jexp in
			let start_loop = 3 in
			let end_loop = 4 in
			let label_start_loop = Label3 start_loop in
			let label_end_loop = Label3 end_loop in
			let if_stmt_start = IfStmt3 (id3_expr, end_loop) in
			let if_stmt_end = IfStmt3 (id3_expr, start_loop) in
			let stmt_ir3_list = convert_stmts_list jstmt_list in

			if_stmt_start :: ((GoTo3 end_loop) :: (label_start_loop :: (stmt_ir3_list @ (if_stmt_end :: [label_end_loop]))))


		| ReadStmt vid ->
			[ReadStmt3 (convert_jlite_var_id vid)]
		| PrintStmt jexp->
			let ir3_type_, ir3_expr_, ir3_stmt_list = convert_jlite_expr jexp in
			let id3_ = "yy" in
			let idc3_ = Var3 id3_ in
			(AssignDeclStmt3 (ir3_type_, id3_, ir3_expr_)) :: [PrintStmt3 idc3_]
		| AssignStmt (vid, jexp) ->
			let id3_ = convert_jlite_var_id vid in
			let ir3_type_, ir3_expr_, ir3_stmt_list = convert_jlite_expr jexp in
			ir3_stmt_list @ [AssignStmt3 (id3_, ir3_expr_)]
		| AssignFieldStmt (jexp1, jexp2) ->
			let _, ir3_expr1, ir3_stmt_list1 = convert_jlite_expr jexp1 in
			let _, ir3_expr2, ir3_stmt_list2 = convert_jlite_expr jexp2 in
			ir3_stmt_list1 @ ir3_stmt_list2 @ [AssignFieldStmt3 (ir3_expr1, ir3_expr2)]
		| MdCallStmt jexp ->
			let _, ir3_expr_, ir3_stmt_list = convert_jlite_expr jexp in
			ir3_stmt_list @ [MdCallStmt3 ir3_expr_]
		| ReturnStmt jexp ->
			let ir3_type_, ir3_expr_, ir3_stmt_list = convert_jlite_expr jexp in
			let id3_ = "yy" in
			let idc3_ = Var3 id3_ in
			ReturnStmt3 id3_ :: AssignStmt3 (id3_, ir3_expr_) :: ir3_stmt_list 
		| ReturnVoidStmt -> [ReturnVoidStmt3]
	in head_stmt_list @ (convert_stmts_list tail)
	| [] -> []

let convert_md_decl (md: md_decl) : md_decl3 = 
	{ rettype3= convert_jlite_type md.rettype; 
	  id3 = convert_jlite_var_id md.ir3id;
	  params3= convert_var_decl_list md.params;				
	  localvars3= convert_var_decl_list md.localvars;				
	  ir3stmts=convert_stmts_list md.stmts;}


let rec convert_md_decl_list (md_decl_list: md_decl list) : (md_decl3 list) = 
	match md_decl_list with
	| head::tail -> (convert_md_decl head) :: (convert_md_decl_list tail)
	| [] -> []

let convert_class_decl (class_decl: class_decl) : cdata3 * (md_decl3 list) =
	let c_name, var_decl_l, md_decl_l = class_decl
	in let var_decl3_list = convert_var_decl_list var_decl_l
	in let md_decl3_list = convert_md_decl_list md_decl_l
	in ((c_name, var_decl3_list), md_decl3_list)


let convert_jlite_typed_program (p: jlite_program) : ir3_program =
	let class_main_, class_decl_list = p in
	let convert_class_main (class_main_:class_main) : (cdata3 * md_decl3) =
		let c_name, _md_decl_main = class_main_ in
		((c_name, []), convert_md_decl _md_decl_main)
	in let rec convert_class_decl_list (class_decl_list: class_decl list) : ((cdata3 list) * (md_decl3 list)) =
		begin
			match class_decl_list with 
			| [] -> [], []
			| head :: tail -> 
			let cdata_list, md_decl3_ = convert_class_decl_list tail in
			let cdata, md_decl3_list = convert_class_decl head in
			(cdata:: cdata_list, md_decl3_ @ md_decl3_list)
		end
	in
	let c_main, main_md = convert_class_main class_main_ in
	let cdata3_list, md_decl3_list = convert_class_decl_list class_decl_list in
	((c_main :: cdata3_list), main_md, md_decl3_list)