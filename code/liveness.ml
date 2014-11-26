
open Arm_structs
open Ir3_structs
open Jlite_structs

module Id3Set = Set.Make(
	struct
	    let compare = String.compare
	    type t = id3
	  end
	);;

type id3_set = Id3Set.t
type edge = id3 * id3

let compare_edges (e1, e2:edge) (f1, f2:edge): int = 
	abs (String.compare e1 f1) + abs (String.compare e2 f2)

module EdgeSet = Set.Make(
	struct
	    let compare = compare_edges
	    type t = id3 * id3
	  end
	);;

type edge_set = EdgeSet.t

let table_size_init = 1000;;

let stmtcount = ref 0
let fresh_stmt () = 
	(stmtcount := !stmtcount+1; !stmtcount)

let are_equal_id3 (i1:id3) (i2:id3): bool =
	((String.compare i1 i2) == 0)

(* Is var in list? *)
let rec is_var_in_id3_list (l:id3 list) (v:id3): bool =
	match l with 
	| head :: tail ->
		if are_equal_id3 head v
		then false
		else is_var_in_id3_list tail v
	| _ -> true

(* Is var not in list? *)
let is_var_not_in_id3_list l v =
	not (is_var_in_id3_list l v)

let rec is_l1_included_in_l2 l1 l2 :bool=
	match l1 with
	| head::tail -> if is_var_in_id3_list l2 head
					then is_l1_included_in_l2 tail l2
					else false
	| [] -> true

let rec compare_id3_list (l1:id3 list) (l2:id3 list): bool =
	is_l1_included_in_l2 l1 l2 && is_l1_included_in_l2 l2 l1

(* Key for a stmt in the stmt table *)
type stmt_key = int
(* Key for a variable in the var table *)
type var_key = string

(*Structure of a node in the Hashtable *)
type stmt_node = 
	{
		id: stmt_key;
		mutable changed: bool;
		mutable visited: bool;
		stmt: ir3_stmt;
		md: md_decl3;
		mutable pred: (stmt_key list);
		mutable succ: (stmt_key list);
		mutable def: id3_set;
		mutable use: id3_set;
		mutable live_in: id3_set;
		mutable live_out: id3_set;
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

(*find the succeId3Setors of a statement*)
let find_succeId3Setors (node:stmt_node) (node_list: stmt_node list): stmt_node = 
	match node.stmt with
	| GoTo3 label ->
		node.succ <- (find_label node_list label) :: node.succ;
		node
	| IfStmt3 (_,label) ->
		node.succ <- (find_label node_list label) :: (node.id + 1) :: node.succ;
		node
	| ReturnStmt3 _ -> (*No succeId3Setor *)
		node
	| ReturnVoidStmt3 -> (*No succeId3Setor *)
		node
	| _ -> (* Simple case*)
		node.succ <- (node.id + 1) :: node.succ;
		node

(*find the succeId3Setors for every statement in a list of statement *)
(*we search the succeId3Setors within the given list *)
let find_all_succeId3Setors (node_list: stmt_node list): stmt_node list = 
	let rec helper (partial_list: stmt_node list) (complete_list: stmt_node list): stmt_node list =
		match partial_list with
		| head::tail ->
			(find_succeId3Setors head complete_list)::(helper tail complete_list)
		| [] ->
			[]
	in (helper node_list node_list)

(* Determines the used variables in a statement *)
(* I consider we dont have AId3SetignDeclStmt3 *)
 let def_vars (stmt:ir3_stmt): id3_set =
	match stmt with
	| AssignStmt3 (var, _)->
		Id3Set.singleton var
	| _ ->
		Id3Set.empty

(* returns the variables used in an idc3 *)	
let used_vars_in_idc3 (varid: idc3): id3_set =
	match varid with
	| Var3 v ->
		Id3Set.singleton v
	| _ ->
		Id3Set.empty

let rec used_vars_in_idc3_list (varid_list: idc3 list): id3_set =
	match varid_list with
	| head::tail -> 
		Id3Set.union (used_vars_in_idc3 head) (used_vars_in_idc3_list tail)
	| [] ->
		Id3Set.empty

(* returns the variables used in an ir3 expreId3Setion *)
let rec used_vars_in_expr (expr:ir3_exp): id3_set =
	match expr with
	| BinaryExp3 (_, a, b) ->
		Id3Set.union (used_vars_in_idc3 a) (used_vars_in_idc3 b)
	| UnaryExp3 (_, a) ->
		(used_vars_in_idc3 a)
	| FieldAccess3 (obj, _) ->
		Id3Set.singleton obj
	| Idc3Expr a ->
		(used_vars_in_idc3 a)
	| MdCall3 (_, var_list) ->
		(used_vars_in_idc3_list var_list)
	| ObjectCreate3 _ ->
		Id3Set.empty


(* returns the list of the variables used in a statement *)
let used_vars (stmt:ir3_stmt): id3_set =
	match stmt with
	| IfStmt3 (expr, _) ->
		used_vars_in_expr expr
	| PrintStmt3 varid ->
		used_vars_in_idc3 varid
	| AssignStmt3 (_, expr) ->
		used_vars_in_expr expr
	| AssignFieldStmt3 (expr1, expr2) -> (* Should not be used *)
		Id3Set.union (used_vars_in_expr expr1) (used_vars_in_expr expr2)
	| MdCallStmt3 expr ->
		used_vars_in_expr expr
	| ReturnStmt3 var ->
		Id3Set.singleton var
	| _ ->
		Id3Set.empty

(*gives a unique id to each statement node *)
let rec create_stmt_node_list (stmt_list: ir3_stmt list) (mthd:md_decl3): stmt_node list = 
	match stmt_list with
	| head::tail ->
		{
			id = fresh_stmt(); 
			changed = true;
			visited = false;
			stmt = head;
			md = mthd; 
			pred = [];
			succ = [];
			def = (def_vars head);
			use = (used_vars head);
			live_in = Id3Set.empty; 
			live_out = Id3Set.empty;
		}::(create_stmt_node_list tail mthd)
	| [] ->
		[]

(* Given a statement k, it adds its id to all its succeId3Setors *)
let add_predeceId3Setor (k:stmt_key) (node: stmt_node) (table:stmt_table): unit = 
	let rec helper (k:stmt_key) (preds:stmt_key list) (table:stmt_table): unit =
		match preds with
		| head::tail ->
			(Hashtbl.find table head).pred <- k :: (Hashtbl.find table head).pred;
			helper k tail table
		| [] -> 
			()
	in (helper k node.succ table)

let find_predeceId3Setors (table: stmt_table): unit = 
	let map_add (k:stmt_key) (node: stmt_node):unit =
		add_predeceId3Setor k node table
	in (Hashtbl.iter map_add table)

(*creates a list of statement nodes, with correct succeId3Setors given the program *)
let rec create_updated_stmt_node_list ((_,main,mds):ir3_program): stmt_node list =
	let rec helper (mds:md_decl3 list): stmt_node list =
		match mds with
		| head::tail ->
			(find_all_succeId3Setors (create_stmt_node_list head.ir3stmts head))@(helper tail)
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

(* Find all the statements in the program which dont have suceId3Setors *)
(* We need them for liveneId3Set analysis *)
let find_stmts_without_succeId3Setors (table: stmt_table): stmt_key list =
	Hashtbl.fold filter table []

let compute_in_from_out (out: id3_set) (use: id3_set) (def: id3_set): id3_set=
	Id3Set.union use (Id3Set.diff out def)

let union_list (l: (id3_set) list) :id3_set=
	List.fold_left Id3Set.union Id3Set.empty l

(* Given a node of key k, we will extract all the ins of the predeceId3Setors to build the out *)
let update_out (table: stmt_table) (k: stmt_key): id3_set=
	let succeId3Setors = (Hashtbl.find table k).succ in
	let rec helper (succ: stmt_key list): id3_set list = 
		match succ with
		| head::tail ->
			let n = (Hashtbl.find table head) in
			n.live_in :: helper tail
		| [] -> []
	in let in_list = helper succeId3Setors
	in union_list in_list

let rec visit_node (k: stmt_key) (table: stmt_table): bool=
	let node = (Hashtbl.find table k) in
	if node.visited
	then node.changed
	else
		begin
			(* Set node visited *)
			node.visited <- true;
			node.live_out <- update_out table k;
			let former_in = node.live_in in
			node.live_in <- compute_in_from_out node.live_out node.use node.def;
			node.changed <- not (Id3Set.equal former_in node.live_in);
			let rec helper l =
			match l with
				| head::tail -> (visit_node head table) || (helper tail)
				| [] -> false
			in ((helper node.pred) || node.changed)
		end

let rec visit_terminals (stmt_list: stmt_key list) (table: stmt_table): bool=
	match stmt_list with
		| head::tail -> (visit_node head table) || (visit_terminals tail table)
		| [] -> false

let liveness_analysis (table: stmt_table): unit = 
	let terminals = find_stmts_without_succeId3Setors table in
	let rec helper (stmt_list:stmt_key list): unit =
		if visit_terminals stmt_list table
		then helper stmt_list
		else ()
	in helper terminals

let f v1 v2 (e_set:edge_set) =
	EdgeSet.add (v1, v2) e_set

(* Given a set of id3, compute all possible couples  *)
let cartesian_square (s1:id3_set): edge_set=
	let elem_list = Id3Set.elements s1 in
	let rec helper elem_list e_set =
		match elem_list with
		| head::tail ->
			let f v1 init = if (are_equal_id3 v1 head)
							then init
							else EdgeSet.add (v1, head) init in
			EdgeSet.union (Id3Set.fold f e_set EdgeSet.empty) (helper tail e_set)
		| [] -> EdgeSet.empty
	in (helper elem_list s1)