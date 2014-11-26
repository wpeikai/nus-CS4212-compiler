
open Arm_structs
open Ir3_structs
open Jlite_structs

let table_size_init = 1000;;

let stmtcount = ref 0
let fresh_stmt () = 
	(stmtcount := !stmtcount+1; !stmtcount)


(* Key for a stmt in the stmt table *)
type stmt_key = int
(* Key for a variable in the var table *)
type var_key = string

(*Structure of a node in the Hashtable *)
type stmt_node = 
	{
		id: stmt_key;
		mark: bool;
		stmt: ir3_stmt;
		md: md_decl3;
		mutable pred: (stmt_key list);
		mutable succ: (stmt_key list);
		def: (id3 list);
		use: (id3 list);
		live_in: (id3 list);
		live_out: (id3 list);
	}

(* Hashtable of statements *)
type stmt_table = 
	(stmt_key, stmt_node) Hashtbl.t

(* Find the stmt id of the label stmt which corresponds to the given label *)
let rec find_label (node_list: stmt_node list) (label:label3): stmt_key =
	match node_list with
	| head::tail ->
		begin
		match head.stmt with
		| Label3 l ->
			if label == l
			then head.id
			else find_label tail label
		| _ ->
			find_label tail label
		end
	| [] ->
		failwith ("#The Label " ^ string_of_int label ^ " was not found here, this should not happen")

(*find the successors of a statement*)
let find_successors (node:stmt_node) (node_list: stmt_node list): stmt_node = 
	match node.stmt with
	| GoTo3 label ->
		node.succ <- (find_label node_list label) :: node.succ;
		node
	| IfStmt3 (_,label) ->
		node.succ <- (find_label node_list label) :: (node.id + 1) :: node.succ;
		node
	| ReturnStmt3 _ -> (*No successor *)
		node
	| ReturnVoidStmt3 -> (*No successor *)
		node
	| _ -> (* Simple case*)
		node.succ <- (node.id + 1) :: node.succ;
		node

(*find the successors for every statement in a list of statement *)
(*we search the successors within the given list *)
let find_all_successors (node_list: stmt_node list): stmt_node list = 
	let rec helper (partial_list: stmt_node list) (complete_list: stmt_node list): stmt_node list =
		match partial_list with
		| head::tail ->
			(find_successors head complete_list)::(helper tail complete_list)
		| [] ->
			[]
	in (helper node_list node_list)

(* Determines the used variables in a statement *)
(* I consider we dont have AssignDeclStmt3 *)
 let def_vars (stmt:ir3_stmt): id3 list =
	match stmt with
	| AssignStmt3 (var, _)->
		[var]
	| _ ->
		[]

(* returns the variables used in an idc3 *)	
let used_vars_in_idc3 (varid: idc3): id3 list =
	match varid with
	| Var3 v ->
		[v]
	| _ ->
		[]

let rec used_vars_in_idc3_list (varid_list: idc3 list): id3 list =
	match varid_list with
	| head::tail -> 
		(used_vars_in_idc3 head) @ (used_vars_in_idc3_list tail)
	| [] ->
		[]

(* returns the variables used in an ir3 expression *)
let rec used_vars_in_expr (expr:ir3_exp): id3 list =
	match expr with
	| BinaryExp3 (_, a, b) ->
		(used_vars_in_idc3 a) @ (used_vars_in_idc3 b)
	| UnaryExp3 (_, a) ->
		(used_vars_in_idc3 a)
	| FieldAccess3 (obj, _) ->
		[obj]
	| Idc3Expr a ->
		(used_vars_in_idc3 a)
	| MdCall3 (_, var_list) ->
		(used_vars_in_idc3_list var_list)
	| ObjectCreate3 _ ->
		[]


(* returns the list of the variables used in a statement *)
let used_vars (stmt:ir3_stmt): id3 list =
	match stmt with
	| IfStmt3 (expr, _) ->
		used_vars_in_expr expr
	| PrintStmt3 varid ->
		used_vars_in_idc3 varid
	| AssignStmt3 (_, expr) ->
		used_vars_in_expr expr
	| AssignFieldStmt3 (expr1, expr2) -> (* Should not be used *)
		(used_vars_in_expr expr1) @ (used_vars_in_expr expr2)
	| MdCallStmt3 expr ->
		used_vars_in_expr expr
	| ReturnStmt3 var ->
		[var]
	| _ ->
		[]

(*gives a unique id to each statement node *)
let rec create_stmt_node_list (stmt_list: ir3_stmt list) (mthd:md_decl3): stmt_node list = 
	match stmt_list with
	| head::tail ->
		{
			id = fresh_stmt(); 
			mark = false;
			stmt = head;
			md = mthd; 
			pred = [];
			succ = [];
			def = (def_vars head);
			use = [];
			live_in = []; 
			live_out = [];
		}::(create_stmt_node_list tail mthd)
	| [] ->
		[]

(* Given a statement k, it adds its id to all its successors *)
let add_predecessor (k:stmt_key) (node: stmt_node) (table:stmt_table): unit = 
	let rec helper (k:stmt_key) (preds:stmt_key list) (table:stmt_table): unit =
		match preds with
		| head::tail ->
			(Hashtbl.find table head).pred <- k :: (Hashtbl.find table head).pred;
			helper k tail table
		| [] -> 
			()
	in (helper k node.succ table)

let find_predecessors (table: stmt_table): unit = 
	let map_add (k:stmt_key) (node: stmt_node):unit =
		add_predecessor k node table
	in (Hashtbl.iter map_add table)

(*creates a list of statement nodes, with correct successors given the program *)
let rec create_updated_stmt_node_list ((_,main,mds):ir3_program): stmt_node list =
	let rec helper (mds:md_decl3 list): stmt_node list =
		match mds with
		| head::tail ->
			(find_all_successors (create_stmt_node_list head.ir3stmts head))@(helper tail)
		| [] ->
			[]
	in (helper (main::mds)) 

(* create a hash table of statement where the unique id of a statement is the key *)
let create_stmt_node_table (p:ir3_program): stmt_table = 
	let rec helper (table:stmt_table) 
                   (nodes: stmt_node list)
                   :stmt_table =
		match nodes with
		| head::tail -> 
			(Hashtbl.add table head.id head);
			(helper table tail)
		| [] ->
			table
	in let nodes = (create_updated_stmt_node_list p) 
	(* Initial table size so that Ocaml do not increase size too often *)
	in let table = Hashtbl.create table_size_init
	in (helper table nodes)

let filter (k:stmt_key) (n:stmt_node) (init:stmt_key list): stmt_key list=
	match n.succ with 
	| [] -> k :: init 
	| _ -> init

(* Find all the statements in the program which dont have sucessors *)
(* We need them for liveness analysis *)
let find_stmts_without_successors (table: stmt_table): stmt_key list =
	Hashtbl.fold filter table []

let liveness_analysis (table: stmt_table): (stmt_table) = 
	table		
