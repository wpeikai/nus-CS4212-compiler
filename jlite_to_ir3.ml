
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


let rec convert_jlite_expr (j_exp: jlite_exp) : ir3_exp * (ir3_stmt list)=
	match j_exp with 
	| TypedExp (e, jtype) ->
		begin match e with
			| UnaryExp (jop, jexp) ->
			let id3_var : id3 = "t1" in
			let idc3_var : idc3 = Var3 id3_var  in
			let (expr_ir3:ir3_exp), (ir3_stmt_list : (ir3_stmt list)) = convert_jlite_expr jexp in
			let stmt = AssignDeclStmt3 ((convert_jlite_type jtype), id3_var, expr_ir3) in
			UnaryExp3 ((convert_jlite_op jop), idc3_var), stmt :: ir3_stmt_list
			| _ -> failwith "5"
		end
	| _ -> failwith "4"


let rec convert_stmts_list (stmts_list: jlite_stmt list) : (ir3_stmt list) =

	let convert_stmt (stmt: jlite_stmt) : (ir3_stmt list) = 
		match stmt with
		| IfStmt (exp1, jsmtm1_list, jsmtm2_list) -> 
			let exprir3, stmts_list = convert_jlite_expr exp1 in
			let ifstmt = IfStmt3 (exprir3, 1) in
			let label1  = Label3 1 in
			let stmt1_ir3_list = convert_stmts_list jsmtm1_list in
			let stmt2_ir3_list = convert_stmts_list jsmtm2_list in
			let returnint = 2 in
			ifstmt :: stmt2_ir3_list @ GoTo3 returnint :: label1 :: (stmt1_ir3_list @ [Label3 returnint])
		| WhileStmt (jexp, jstmt_list) ->
			let id3_expr, jexp_stmts = convert_jlite_expr jexp in
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

		| _ -> failwith "3"
	in

	(* Function begin here *)
	match stmts_list with
	| head::tail -> (convert_stmt head) @ (convert_stmts_list tail)
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