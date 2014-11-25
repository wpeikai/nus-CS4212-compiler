
(* ===================================================== *)
(* ============== CS4212 Compiler Design ============== *)
(*     TypeChecking of Jlite programs *)
(*     A Simplified version  *)
(*     Submitting at your own risk  *)
(* ===================================================== *)

open Jlite_structs

(* Compare two variable ids *) 	
let compare_var_ids v1 v2 =
	match v1,v2 with
	| SimpleVarId id1, SimpleVarId id2 -> 
		((String.compare id1 id2) == 0)
	| SimpleVarId id1, TypedVarId (id2,t,s) -> 
		((String.compare id1 id2) == 0)	
	| TypedVarId (id1,t,s), SimpleVarId id2 -> 
		((String.compare id1 id2) == 0)		
	| TypedVarId (id1,t1,s1), TypedVarId (id2,t2 ,s2) ->
		((String.compare id1 id2) == 0) && (s1 == s2)

(* Compare two jlite_types *) 	
let compare_jlite_type t1 t2 =
	match t1,t2 with
	| ObjectT o1, ObjectT o2 -> 
		((String.compare o1 o2) == 0)
	| j1, j2 -> (j1 == j2)

(* Compare two jlite_types. We consider that Object a and Null are the same *) 	
let compare_jlite_type_almost_equal t1 t2 =
	match t1,t2 with
	| ObjectT o1, ObjectT o2 -> 
		((String.compare o1 o2) == 0 ||
		(String.compare o1 "null") == 0 ||
		(String.compare "null" o2) == 0)
	| j1, j2 -> (j1 == j2)


(* Compare two var_decl *) 	
let compare_var_decl var_1 var_2 =
	let (t1, v1), (t2, v2) = (var_1, var_2) in
	(compare_jlite_type t1 t2) && 
		(compare_var_ids v1 v2)

(* Compare two var_decl_list *) 	
let rec compare_var_decl_list l1 l2 =
	match l1, l2 with
	| t1::tail_1, t2::tail_2 -> 
		(compare_var_decl t1 t2) &&
			(compare_var_decl_list tail_1 tail_2)
	| [], [] -> true
	| _, _ -> false

(* Compare two jlite_type_list *) 	
let rec compare_jlite_type_list l1 l2 =
	match l1, l2 with
	| t1::tail_1, t2::tail_2 -> 
		(compare_jlite_type t1 t2) &&
			(compare_jlite_type_list tail_1 tail_2)
	| [], [] -> true
	| _, _ -> false

(* Compare one jlite_type_list and one var_decl_list *) 	
let rec compare_jlite_type_list_var_decl
	(l1:jlite_type list) (l2: var_decl list) =
	let l2_type = List.map (fun (typ, vid) -> typ) l2 in
	compare_jlite_type_list l1 l2_type

(* Compare two methods declarations *) 	
let compare_methods_decl m1_decl m2_decl =
	(compare_var_ids m1_decl.jliteid m2_decl.jliteid) &&
	(compare_jlite_type m1_decl.rettype m2_decl.rettype) &&
	(compare_var_decl_list m1_decl.params m2_decl.params)


let rec find_md_decl_in_list (mdlst: md_decl list) (md: md_decl) =
	match mdlst with
		| [] -> failwith "Method not found"
		| m::tail_md -> if (compare_methods_decl m md)
						then m
						else (find_md_decl_in_list tail_md md)

let rec exist_md_decl_in_list (mdlst: md_decl list) (md: md_decl) =
	let rec compare_jlite_type_list (var_list1: var_decl list) (var_list2: var_decl list) =
		match var_list1, var_list2 with 
		| [], [] -> true
		| (h1::tail1, h2::tail2) -> 
			begin
			match (h1, h2) with 
			| (type1,_), (type2 ,_) -> (compare_jlite_type type1 type2) && (compare_jlite_type_list tail1 tail2)
			end 
		| _, _ -> false
	in let has_same_param_and_var_id m1_decl m2_decl =
	(compare_var_ids m1_decl.jliteid m2_decl.jliteid) &&
	(compare_jlite_type_list m1_decl.params m2_decl.params) in

	match mdlst with
		| [] -> false 
		| m::tail_md -> if (has_same_param_and_var_id m md)
						then true
						else (exist_md_decl_in_list tail_md md)

let rec find_md_id_params_in_list (mdlst: md_decl list) (md_id: var_id) (md_params_type_list: jlite_type list) =
	match mdlst with
		| [] -> failwith "Method not found"
		| m::tail_md ->
			if (compare_var_ids m.jliteid md_id) &&
				(compare_jlite_type_list_var_decl md_params_type_list m.params)
			then m
			else (find_md_id_params_in_list tail_md md_id md_params_type_list)


let find_class_in_prog (p: jlite_program) (cid: class_name) =
	let rec find_class_in_list (class_decl_list: class_decl list) (cid: class_name) =
	match class_decl_list with
		| [] -> failwith "Class not found"
		| c::tail_class -> 
			let c_name, var_decl_list, md_decl_list = c in
			if ((String.compare c_name cid) == 0)
				then c
				else (find_class_in_list tail_class cid)
	in
	let (_, class_decl_list) = p in
	find_class_in_list class_decl_list cid

let find_md_decl_in_p (p: jlite_program) (cid:class_name) (md: md_decl) = 
	let c = find_class_in_prog p cid in
	let _, _, md_decl_list = c in
	find_md_decl_in_list md_decl_list md

let find_md_ids_params_in_p (p: jlite_program) (cid:class_name) (md_id: var_id) (md_params_type: jlite_type list) = 
	let c = find_class_in_prog p cid in
	let _, _, md_decl_list = c in
	find_md_id_params_in_list md_decl_list md_id md_params_type

(* Find the declared type of a variable *) 		
let rec find_var_decl_type 
	(vlst: var_decl list) (vid:var_id) =
  match vlst with
    | [] -> failwith ("Var " ^ string_of_var_id vid ^ " not found" )
    | (t,v)::lst -> 
		if (compare_var_ids v vid) 
		then (t,v) 
		else (find_var_decl_type lst vid)

let find_var_decl_type_class (p: jlite_program) (cid:class_name) (vid: var_id) =
	let c = find_class_in_prog p cid in
	let (_, var_decl_list, _) = c in
	find_var_decl_type var_decl_list vid

(* Check if a variable id exists *)
let exists_var_id 
	(vlst: var_decl list) (vid: var_id) : bool =
	let is_eq_id ((t,v): var_decl):bool =
		(compare_var_ids v vid) 
	in (List.exists is_eq_id vlst) 	

(* Check if the declaration of a class exists *) 			  
let exists_class_decl 
	((cm,clst): jlite_program) (cid:class_name) =
	(List.exists 
		(fun (cname,cvars,cmtd) ->
			(String.compare cname cid) == 0)
	clst)

(* Annotate a list of variable declarations with their scope *)	
let rec create_scoped_var_decls
	(vlst: var_decl list) (scope:int) =
	let helper ((vt,vid):var_decl) =
		match vid with
		| SimpleVarId id -> 
			(vt, TypedVarId (id, vt, scope))
		| TypedVarId (id,t,s) -> 
			(vt,TypedVarId (id, vt, scope))
	in (List.map helper vlst)
  
(* Type check a list of variable declarations 
  1) Determine if all object types exist
  2) Find and return duplicate variable names -- TODO-> Check error message

    This type check function is a simple version in 
    which there is only one variable declaration in 
    the declaration list
	The function returns the 
	typecheck result (true or false) and an error message.
*)  
let rec type_check_var_decl_list
	(p: jlite_program) 
	(vlst: var_decl list) =
	let rec helper 
		(vlst: var_decl list) :var_id list =
		match vlst with
		| (typ,vid)::tail_vlst -> 
			begin
			match typ with
			| ObjectT cname -> 
	        (* check if the declared class name exists *)
				if (exists_class_decl p cname) 
				then match (exists_var_id tail_vlst vid) with
					| true -> vid :: helper tail_vlst
					| false -> helper tail_vlst
				(* return the undefined type *)
				else vid :: helper tail_vlst
			(* Primitive type *)
			| _ -> match (exists_var_id tail_vlst vid) with
						| true -> vid :: helper tail_vlst
						| false -> helper tail_vlst 
			end
		| [] -> []
	in match (helper vlst) with
		| [] ->  (true,"")
		| lst -> (false, ("Undefined type or duplicate variable names, or class names: " 
				^ (string_of_list lst string_of_var_id ",")))

 
(* Type check a list of method declarations 
  1) Determine if there is illegal overloading
  2) Find and return overloaded method names	

    This simplified version always returns true
	---  TODO  ---
*)  
let rec type_check_md_overloading 
	(classid: class_name) (mdlst: md_decl list) (counter_methd: int ref) (p:jlite_program)=
	let rec helper 
		(mdls: md_decl list) (counter: int ref) : var_id list =
		match mdls with
		| md::tail_mdlst -> 
			begin
				counter := !counter + 1;
				let a = match md.jliteid with
					| SimpleVarId s ->  SimpleVarId (s ^ "_" ^ classid ^ "_" ^ (string_of_int (!counter-1)))
					| TypedVarId (s, t, i) -> SimpleVarId (s ^ "_" ^ classid ^ "_" ^ (string_of_int (!counter-1))) in
				md.ir3id <- a;
				match exist_md_decl_in_list tail_mdlst md with 
			    | true -> md.jliteid :: helper tail_mdlst counter
			    | false -> helper tail_mdlst counter
			end
		| [] -> []
	in match (helper mdlst counter_methd) with
		| [] ->  (true,"")
		| lst -> (false, ("overloaded method names: " 
				^ (string_of_list lst string_of_var_id ",")))


(* Type check an expression *)
(* Return the type of the Expression 
    and a new TypedExpession *)  
let rec type_check_expr 
	(p: jlite_program)(env: var_decl list) 
	(classid: class_name) (exp:jlite_exp) = 
	let rec helper e  : (jlite_type * jlite_exp) =
		match e with
		| BoolLiteral v -> (BoolT, TypedExp (e, BoolT))
		| IntLiteral v -> (IntT, TypedExp (e, IntT))
		| StringLiteral v -> (StringT, TypedExp (e, StringT))
		| ThisWord -> 
			((ObjectT classid), TypedExp (e,(ObjectT classid)))
		| NullWord -> 
			((ObjectT "null") , TypedExp (e,(ObjectT "null")))
		| Var v -> 
			let (vtyp,vid) = (find_var_decl_type env v) in
			(vtyp, TypedExp (Var vid,vtyp)) 
		| ObjectCreate c -> 
			if (exists_class_decl p c)  
			then ((ObjectT c), TypedExp (e,(ObjectT c)))
			else failwith 
						("\nType-check error in " 
						^ classid
						^ ". ObjectCreate " ^ c ^ "does not exist " 
						^ " type checking fails:\n" 
						^ string_of_jlite_expr exp ^ "\n")
		| UnaryExp (op, e) -> 
			let (e_type, e_expr) = type_check_expr p env classid e in
			begin
			match (op, e_type) with
			| (UnaryOp operator, BoolT) -> 
				if operator = "!" then (BoolT, TypedExp (UnaryExp (op, e_expr), BoolT))
					else failwith 
						("\nType-check error in " 
						^ classid 
						^ ". UnaryExp type checking fails:\n" 
						^ string_of_jlite_expr exp ^ "\n")
			| (UnaryOp operator, IntT) -> 
				if operator = "-" then (IntT, TypedExp (UnaryExp (op, e_expr), IntT))
					else failwith 
						("\nType-check error in " 
						^ classid 
						^ ". UnaryExp type checking fails:\n" 
						^ string_of_jlite_expr exp ^ "\n")
			| (_, _) -> failwith 
					("\nType-check error in " 
					^ classid 
					^ ". UnaryExp type checking fails:\n" 
					^ string_of_jlite_expr exp ^ "\n")
			end
		| BinaryExp (op, e1, e2) -> 
			let (e_type1, e_expr1) = type_check_expr p env classid e1 in
			let (e_type2, e_expr2) = type_check_expr p env classid e2 in
			begin
			match (op, e_type1, e_type2) with
			| (RelationalOp operator, IntT, IntT) -> (BoolT, TypedExp (BinaryExp (op, e_expr1, e_expr2), BoolT))
			| (AritmeticOp operator, IntT, IntT) -> (IntT, TypedExp (BinaryExp (op, e_expr1, e_expr2), IntT))
			| (BooleanOp operator, BoolT, BoolT) -> (BoolT, TypedExp (BinaryExp (op, e_expr1, e_expr2), BoolT))
			| (RelationalOp operator, f1, f2) ->
				begin
				match operator with 
				| "==" 
				| "!=" -> 
					if (compare_jlite_type_almost_equal f1 f2)
						then (BoolT, TypedExp (BinaryExp (op, e_expr1, e_expr2), BoolT))
					    else failwith 
							("\nType-check error in " 
							^ classid ^ " we have " ^ string_of_jlite_type e_type2 ^ " and "
							^ string_of_jlite_type e_type1 ^ " and " ^ string_of_jlite_op op
							^ ". BinaryExp type checking fails:\n" 
							^ string_of_jlite_expr exp ^ "\n")
				| _ -> failwith 
					("\nType-check error in " 
					^ classid ^ " we have " ^ string_of_jlite_type e_type2 ^ " and "
					^ string_of_jlite_type e_type1 ^ " and " ^ string_of_jlite_op op
					^ ". BinaryExp type checking fails:\n" 
					^ string_of_jlite_expr exp ^ "\n")
				end
			| (_, _, _) -> failwith 
					("\nType-check error in " 
					^ classid ^ " we have " ^ string_of_jlite_type e_type2 ^ " and "
					^ string_of_jlite_type e_type1 ^ " and " ^ string_of_jlite_op op
					^ ". BinaryExp type checking fails:\n" 
					^ string_of_jlite_expr exp ^ "\n")
			end
		| FieldAccess (e, vid) -> 
			let (e_type, e_expr) = type_check_expr p env classid e in
			begin
			match e_type with
				| ObjectT c ->
					let (idtype, v) = (find_var_decl_type_class p c vid) in
					(idtype, TypedExp( FieldAccess(e_expr, vid), idtype))
				| _ -> failwith 
					("\nType-check error in " 
					^ classid 
					^ ". FieldAccess type checking fails:\n" 
					^ string_of_jlite_expr exp ^ "\n")
			end
		| MdCall (e, e_list) -> 
			let get_jlite_type x = 
				let res, _ = type_check_expr p env classid x in
				res in
			let get_jlite_expr x = 
				let  _, res = type_check_expr p env classid x in
				res in

			let e_type_list = List.map get_jlite_type e_list in
			let e_expr_list = List.map get_jlite_expr e_list in
			begin
			let m_type, e_expr = match e with
			| FieldAccess (object_name_e, method_name) -> 

				let object_name_type, object_name_expr = type_check_expr p env classid object_name_e in
				begin

				match object_name_type with
				| ObjectT c_name -> 

					let method_decl = find_md_ids_params_in_p p c_name method_name e_type_list in
					method_decl.rettype, TypedExp( FieldAccess (object_name_expr, method_name), object_name_type)
				| _ ->  failwith 
					("Object non recognized\n")
				end
			| Var method_name ->
				let method_decl = find_md_ids_params_in_p p classid method_name e_type_list in
				(* Thisword missing. Return field access with this set to the current class id. *)
				method_decl.rettype, TypedExp(FieldAccess (TypedExp (ThisWord, ObjectT classid), 
											               method_name),
			                                  ObjectT classid)
			| _ -> failwith 
					("FieldAccess non recognized" ^ string_of_jlite_expr e ^ "\n")
			in m_type, TypedExp (MdCall (e_expr, e_expr_list), m_type)
			end
		| _ -> failwith 
					("\nType-check error in " 
					^ classid 
					^ ". Expression not recognized:\n" 
					^ string_of_jlite_expr exp ^ "\n")
	  in  
	  helper exp

(* Type check a list of statements and determine the return type.
   Exceptions are thrown when a statement does not type check 
   and when dead code is found
*)  
let rec type_check_stmts 
	(p: jlite_program)(env: var_decl list) 
	(classid: class_name) 
	(mthd: md_decl) 
	(stmtlst:jlite_stmt list)
	(rettype: jlite_type option) 
	: (jlite_type option *(jlite_stmt list))  =
	match stmtlst with
	| [] -> (rettype,[])
	| s::tail_lst -> 
		let rec helper s 
		: (jlite_type option * jlite_stmt) =
		match s with
		| ReturnStmt e ->  
			let (expr_type,exprnew) = 
			 (type_check_expr p env classid e) in
			begin
			match expr_type with
			| Unknown -> 
				failwith 
				("\nType-check error in " 
				^ classid ^ "." ^ string_of_var_id mthd.jliteid 
				^ ". Return expression fails:\n" 
				^ string_of_jlite_stmt s ^ "\n")
			| _ ->  
			(Some expr_type, ReturnStmt exprnew)
			end
		| ReturnVoidStmt ->  
			(Some VoidT,ReturnVoidStmt)
		| ReadStmt id -> 
			let (idtype,scopedid) = (find_var_decl_type env id) in
			begin
			match idtype with
			| ObjectT _ | Unknown  -> 
				failwith 
				("\nType-check error in " 
				^ classid ^ "." ^ string_of_var_id mthd.jliteid 
				^ ". Read statement fails:\n" 
				^ string_of_jlite_stmt s ^ "\n")
			| _ ->  (None, ReadStmt scopedid)
			end
		| PrintStmt e -> 
			let (expr_type,exprnew) = 
			 (type_check_expr p env classid e) in
			begin
			match expr_type with
			| Unknown | ObjectT _ -> 
				failwith 
				("\nType-check error in " 
				^ classid ^ "." ^ string_of_var_id mthd.jliteid 
				^ ". Print statement fails:\n" 
				^ string_of_jlite_stmt s ^ "\n")
			| _ ->  (None, PrintStmt exprnew)
			end
		| MdCallStmt e -> 
			let (expr_type, exprnew) = 
			 (type_check_expr p env classid e) in
			begin
			match expr_type with
			| Unknown -> 
				failwith 
				("\nType-check error in " 
				^ classid ^ "." ^ string_of_var_id mthd.jliteid 
				^ ". Method call statements expression fails:\n" 
				^ string_of_jlite_stmt s ^ "\n")
			| _ ->  
			(Some expr_type, MdCallStmt exprnew)
			end
		| AssignFieldStmt (e, f) -> 
			let (expr_type_e, exprnew_e) = 
			 (type_check_expr p env classid e) in
			let (expr_type_f, exprnew_f) = 
			 (type_check_expr p env classid f) in

			begin
			match (expr_type_e, expr_type_f) with
			| (_, Unknown) | (Unknown, _) -> 
				failwith 
				("\nType-check error in " 
				^ classid ^ "." ^ string_of_var_id mthd.jliteid 
				^ ". AssignFieldStmt expression fails:\n" 
				^ string_of_jlite_stmt s ^ "\n"
				^ string_of_jlite_type expr_type_e ^ "\n")
			| (_, _) ->  
				begin
					if expr_type_e = expr_type_f then (None, AssignFieldStmt (exprnew_e, exprnew_f))
						else failwith 
								("\nType-check error in " 
								^ classid ^ "." ^ string_of_var_id mthd.jliteid 
								^ ". AssignFieldStmt expression fails:\n" 
								^ string_of_jlite_stmt s ^ "\n")
				end
			end
		| AssignStmt (var_id, e) -> 
			let (idtype,scopedid) = (find_var_decl_type env var_id) in
			let (expr_type, exprnew) = 
			 (type_check_expr p env classid e) in

			begin
			match (idtype, expr_type) with
			| (Unknown, _) | (_, Unknown)  -> 
				failwith 
				("\nType-check error in " 
				^ classid ^ "." ^ string_of_var_id mthd.jliteid 
				^ ". AssignStmt statement fails:\n" 
				^ string_of_jlite_stmt s ^ "\n")
			| (_, _) ->  
				begin
					if idtype = expr_type then (None, AssignStmt (scopedid, exprnew))
						else failwith 
								("\nType-check error in " 
								^ classid ^ "." ^ string_of_var_id mthd.jliteid 
								^ ". AssignStmt expression fails:\n"
								^ string_of_jlite_stmt s ^ string_of_jlite_type expr_type ^ "\n")
				end
			end
		| WhileStmt (e, stmts) -> 
			let (e_expr_type, e_expr) = (type_check_expr p env classid e) in
			if not (e_expr_type = BoolT)
				then
					failwith 
							("\nType-check error in " 
							^ classid ^ "." ^ string_of_var_id mthd.jliteid
							^ string_of_jlite_type e_expr_type ^ "\n" 
							^ ". While1 expression fails:\n" 
							^ string_of_jlite_stmt s ^ "\n"
						)
			else let ret_type, ret_stmts = type_check_stmts p env classid mthd stmts rettype in
			(ret_type, WhileStmt(e_expr, ret_stmts))
		| IfStmt (if_e, then_e, else_e) -> 
			let (if_expr_type, if_expr) = (type_check_expr p env classid if_e) in
			begin
			if not (if_expr_type = BoolT)
				then
					failwith 
							("\nType-check error in " 
							^ classid ^ "." ^ string_of_var_id mthd.jliteid
							^ string_of_jlite_type if_expr_type ^ "\n" 
							^ ". IfExpr1 expression fails:\n" 
							^ string_of_jlite_stmt s ^ "\n"
						)
				else
					let (then_expr_type, then_expr) = (type_check_stmts p env classid mthd then_e None) in
					let (else_expr_type, else_expr) = (type_check_stmts p env classid mthd else_e None) in
					match then_expr_type, else_expr_type with
					| Some ObjectT c1, Some ObjectT c2 ->
						if (compare_jlite_type_almost_equal (ObjectT c1) (ObjectT c2))							(* if c2 != "null" return c2 else return c1 *)
							then if compare_jlite_type (ObjectT "null") (ObjectT c2)
								 	then (Some (ObjectT c1), IfStmt(if_expr, then_expr, else_expr))
								 	else (Some (ObjectT c2), IfStmt(if_expr, then_expr, else_expr))
							else failwith 
					                ("\nType-check error in " 
					                ^ classid ^ "." ^ string_of_var_id mthd.jliteid
					                ^ "\n" ^ string_of_jlite_type if_expr_type ^ "\n" 
					                ^ ". IfExp2 expression fails:\n" 
					                (* ^ string_of_jlite_stmt s ^ "\n" *)
					                ^ string_of_jlite_type (ObjectT c1) ^ "\n"
					                ^ string_of_jlite_type (ObjectT c2) ^ "\n")
					| Some jlite_type1, Some jlite_type2 -> 
						if (compare_jlite_type jlite_type1 jlite_type2) 
							then (Some jlite_type1, IfStmt(if_expr, then_expr, else_expr)) 
							else failwith 
						        ("\nType-check error in " 
						        ^ classid ^ "." ^ string_of_var_id mthd.jliteid
						        ^ string_of_jlite_type if_expr_type ^ "\n" 
						        ^ ". IfExpr3 expression fails:\n" 
						        ^ string_of_jlite_stmt s ^ "\n")
					| None, Some jlite_type1
					| Some jlite_type1, None -> (Some jlite_type1, IfStmt(if_expr, then_expr, else_expr))
					| None, None -> (None, IfStmt(if_expr, then_expr, else_expr))
					(* | _, _ -> failwith 
						        ("\nType-check error in " 
						        ^ classid ^ "." ^ string_of_var_id mthd.jliteid
						        ^ string_of_jlite_type if_expr_type ^ "\n" 
						        ^ ". IfExpr4 expression fails:\n" 
						        ^ string_of_jlite_stmt s ^ "\n") *)

			end

					(* match then_expr_type, else_expr_type with *)
						(* | Some Unknown -> failwith  *)
							(* ("\nType-check error in "  *)
							(* ^ classid ^ "." ^ string_of_var_id mthd.jliteid  *)
							(* ^ ". IfExpr2 expression fails:\n"  *)
							(* ^ string_of_jlite_stmt s ^ "\n" *)
							(* ^ "It does not return the same type") *)
						(* | _ -> (then_expr_type, IfStmt(if_e, then_e, else_e)) *)
	  in
      let (newrettype,newstmt) = (helper s) in
      (* print_string ((string_of_jlite_stmt newstmt) ^ "\n"); *)
	  	(* tail_st is not empty and we have a return type! *)
	  match newrettype, newstmt, tail_lst with
		| _, ReturnStmt r, head::tail  -> 
			failwith 
			("\nType-check error in " ^ classid ^ "." 
			 ^ string_of_var_id mthd.jliteid 
			 ^ ". Dead Code:\n" 
			 ^ (string_of_list tail_lst string_of_jlite_stmt "\n" ^ "\n"))
		| _, ReturnVoidStmt, head::tail ->
			failwith 
			("\nType-check error in " ^ classid ^ "." 
			 ^ string_of_var_id mthd.jliteid 
			 ^ ". Dead Code:\n" 
			 ^ (string_of_list tail_lst string_of_jlite_stmt "\n" ^ "\n"))

		| _,_, _ ->  
			let (rettype,stmts) = 
				(type_check_stmts p env classid mthd tail_lst newrettype) in
				(rettype,(newstmt::stmts))
  
(* TypeCheck a JLite Method Declaration *)
let type_check_mthd_decl p env cname m : md_decl = 
	let mthdenv = 
		List.append m.params m.localvars in 
	let (retval, errmsg) = 
		(type_check_var_decl_list p mthdenv)
	in if (retval == false) 
		then failwith 
		 ("\nType-check error in " ^ cname ^ "." 
		  ^ string_of_var_id m.jliteid 
		  ^ " parameter or local variables declarations.\n"
		  ^ errmsg ^ "\n")
		else
		let scopedEnv = List.append 
				(create_scoped_var_decls mthdenv 2) env in 
		(* TypeCheck the body of the method *)
			let (rettyp,newstmts) = 
				(type_check_stmts p scopedEnv cname m m.stmts None) in
		(* TypeCheck the return type of the method *)
			let _ = match rettyp,m.rettype with
			| None, VoidT -> true
			| Some VoidT, VoidT -> true
			| None, t -> 
				failwith 
				("\nType-check error in " ^ cname ^ "." 
				^ string_of_var_id m.jliteid 
				^ ". This method must return a result of type "
				^ string_of_jlite_type m.rettype ^ ". \n")
			| Some (ObjectT t1), (ObjectT t2) ->
				(* The type returned may be NULL, and this is a correct return type for any Object t *)
				if ((String.compare t1 t2) == 0 ||
					(String.compare t1 "null") == 0 ||
					(String.compare t2 "null") == 0) 
				then true
				else failwith 
					("\nType-check error in " ^ cname ^ "." 
					^ string_of_var_id m.jliteid 
					^ ". Type mismatch. Return type of method " 
					^ "is different from declared type \n"
					^ t1 ^ ". \n"
					^ t2 ^ ". \n")
			| Some t1, t2 -> 
				if (t1!= t2) 
				then failwith 
					("\nType-check error in " ^ cname ^ "." 
					^ string_of_var_id m.jliteid 
					^ ". Type mismatch. Return type of method "
					^ "is different from declared type "
					^ string_of_jlite_type m.rettype 
					^ string_of_jlite_type t1 ^ ". \n")
				else true
			in { m with stmts=newstmts;
				}

let exists_class_decl_list  (class_decl_list:class_decl list) (cid:class_name) =
	(List.exists 
		(fun (cname,cvars,cmtd) ->
			(String.compare cname cid) == 0)
	class_decl_list)

(* Check that two classes have distinct names *)
let rec distinct_class_name (class_decl_list: class_decl list) : bool = 
	match class_decl_list with
	| [] -> true
	| h :: tail -> 
		let cname, _, _ = h in
		not (exists_class_decl_list tail cname) && distinct_class_name tail


(* TypeCheck a JLite Program. 
   Return a new JLite Program 
   where expressions are annotated with types
*)
let type_check_jlite_program  
	(p:jlite_program) : jlite_program=

	let type_check_class_main 
		((cname,mmthd):class_main ) =
		(cname,(type_check_mthd_decl p [] cname mmthd )) in
	let rec type_check_class_decl 
		(counter_methd:int ref)
		((cname,cvars,cmthds):class_decl)  =
		(* TypeCheck field declarations *)
		let (retval, errmsg) = 
			(type_check_var_decl_list p cvars) in
		if (retval==false) then 
			failwith 
			("\nType-check error in " ^ cname 
			^ " field declarations." ^ errmsg ^ "\n")
		(* TypeCheck methods overloading *)
		else let (retval, errmsg) = 
			(type_check_md_overloading cname cmthds counter_methd p) in
			if (retval==false) then 
				failwith 
				("\nType-check error in " ^ cname 
				^ " method declarations." ^ errmsg ^ "\n")
			(* TypeCheck method declarations *)
			else let env = (create_scoped_var_decls cvars 1) 
			in (cname, cvars, 
				List.map 
				(type_check_mthd_decl p env cname) cmthds
				)
	in 

	let check_distinct_class_names classes = 
		let res = distinct_class_name classes in
		if (res==true) then true
		else failwith "Two classes have the same name" in
	begin
		let (mainclass, classes) = p in
		let a, b = mainclass in
		let main_with_var_decl = a, [], [b] in 
		let _ = check_distinct_class_names (main_with_var_decl :: classes) in 
		let newmain = 
			(type_check_class_main mainclass) in
	    let counter_methd : int ref = ref 0 in
		let newclasses=
			(List.map (type_check_class_decl counter_methd)  classes ) in
		(newmain, newclasses)
	end
