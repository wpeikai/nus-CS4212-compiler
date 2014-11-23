
(* ===================================================== *)
(* ============== CS4212 Compiler Design ============== *)
(*   Transformation to intermediary representation IR3   *)
(* ===================================================== *)

open Arm_structs
open Ir3_structs
open Jlite_structs

let labelcount = ref 0 
let fresh_label () = 
	(labelcount:=!labelcount+1; !labelcount)

let varcount = ref 0 
let fresh_var () = 
	(varcount:=!varcount+1; (string_of_int !varcount))

let compare_id3 (i1:id3) (i2:id3): bool =
	((String.compare i1 i2) == 0)

let get_offset (md:md_decl3)  (var:id3): int =
	let rec helper(i:int) (var_list:var_decl3 list) (var:id3): int = 
		match var_list with
		| (_,head_id3)::tail ->
			if (compare_id3 head_id3 var)
			then i
			else helper (i+1)  tail var
		| _ ->
			failwith "#52 This should not happen"
	in 24 + 4 * (helper 0 md.localvars3 var)
	
	(* Only works on simple types at the moment *)
let get_stack_space (md:md_decl3): int =	
	24 + 4 * List.length(md.localvars3)
			
let convert_ir3_expr (exp:ir3_exp) (md:md_decl3) : arm_program=
	let all_instructions = [] in

	match exp with
	| BinaryExp3 (ir3_op_1, idc3_1, idc3_2) ->
		begin
		match ir3_op_1 with 
		| AritmeticOp _ ->
			let instructions1 = 
				begin
				match idc3_1 with
				| IntLiteral3 i -> 
					[MOV ("", false, "a1", (ImmedOp ("#" ^ (string_of_int i))))]
				| _ -> failwith "#55"
				end
			in
			let instructions2 = 
				begin
				match idc3_2 with
				| IntLiteral3 i ->
					[MOV ("", false, "a2", (ImmedOp ("#" ^ (string_of_int i))))]
				| _ -> failwith "#55"
			end
			in
			all_instructions @ instructions1 @ instructions2 @ [ADD ("", false, "v1", "a1", (RegOp "a2"))]
		| _ -> failwith "#54: Unknown binary operator"
		end
	| _ ->
		failwith "#50: Expression not yet implemented"

let convert_ir3_stmt (md:md_decl3) (stmt:ir3_stmt): arm_program = 
	match stmt with
	| AssignStmt3 (id3_1, ir3_exp_1) ->
		(* Maybe It should be RegPreIndexed*)
		(convert_ir3_expr ir3_exp_1 md) @ [STR ("", "", "v1", (RegPostIndexed ("fp", get_offset md id3_1)))]
	| _ ->
		failwith "#51: Statement not yet implemented"

let convert_ir3_md_decl (md:md_decl3): arm_program =
	let rec helper (stmts: ir3_stmt list): arm_program =
		match stmts with
		| head::tail -> 
			(convert_ir3_stmt md head) @ helper(tail)
		| [] ->
			[]
	in 
	(*Label with function name*)
	PseudoInstr (md.id3 ^ ":") ::
	(*Store registers on the stack*)
	STMFD ("fp" :: "lr" :: "v1" :: "v2" :: "v3" :: "v4" :: "v5" :: []) ::
	(* sp = fp - 24 *)
	ADD ("", true, "fp", "sp", ImmedOp "#24") ::
	(* allocate local variables*)
	SUB ("", true, "sp", "fp", ImmedOp ("#" ^ (string_of_int (get_stack_space md)))) ::
	helper(md.ir3stmts) @ 
	(* Maybe we should put a L#exit label here *)
	[SUB ("", true, "sp", "fp", ImmedOp "#24")] @
	[LDMFD ("fp" :: "pc" :: "v1" :: "v2" :: "v3" :: "v4" :: "v5" :: [])]

let ir3_program_to_arm ((_, main, mds):ir3_program):arm_program =
	let rec helper(mds:md_decl3 list):arm_program =
		match mds with
		| head::tail -> 
			(convert_ir3_md_decl head) @ (helper tail)
		| [] ->
			[]
	in 
		PseudoInstr (".text") ::
		PseudoInstr (".global main") ::
	   	helper(main::mds)

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
