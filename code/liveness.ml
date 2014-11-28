
open Arm_structs
open Jlite_structs
open Ir3_structs

module Id3Set = Set.Make(
	struct
	    let compare = String.compare
	    type t = id3
	  end
	);;

type id3_set = Id3Set.t
type edge = id3 * id3

let compare_edges (e1, e2:edge) (f1, f2:edge): int =
	String.compare (e1^"!"^e2) (f1^"!"^f2)

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

(* Hashtable of graphs *)
type graph_table = 
	(id3, edge_set) Hashtbl.t

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
		node.succ <- (find_label node_list label) :: (node.id - 1) :: node.succ;
		node
	| ReturnStmt3 _ -> (*No successor *) 
		node
	| ReturnVoidStmt3 -> (*No successor *)
		node
	| _ -> (* Simple case*)
		node.succ <- (node.id - 1) :: node.succ;
		node

(*find the successors for every statement in a list of statement *)
(*we search the successors within the given list *)
let find_all_successors (node_list: stmt_node list): stmt_node list = 
	let rec helper (partial_list: stmt_node list) (complete_list: stmt_node list): stmt_node list =
		match partial_list with
		| head::[] ->
			[head]
		| head::tail ->
			(find_successors head complete_list)::(helper tail complete_list)

		| [] ->
			[]
	in (helper node_list node_list)

(* Determines the used variables in a statement *)
(* I consider we dont have AssignDeclStmt3 *)
 let def_vars (stmt:ir3_stmt): id3_set =
	match stmt with
	| AssignStmt3 (var, _)->
		Id3Set.singleton var
	| LoadStmt3 var ->
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

(* returns the variables used in an ir3 expression *)
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
	| StrStmt3 var ->
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

(* Given a statement k, it adds its id to all its successors *)
let add_predecessor (k:stmt_key) (node: stmt_node) (table:stmt_table): unit = 
	let rec helper (k:stmt_key) (succ:stmt_key list) (table:stmt_table): unit =
		match succ with
		| head::tail ->
			begin
			let temp = k :: (Hashtbl.find table head).pred in
			(Hashtbl.find table head).pred <- temp;
			helper k tail table
			end
		| [] -> 
			()
	in (helper k node.succ table)

let find_predecessors (table: stmt_table): unit = 
	let map_add (k:stmt_key) (node: stmt_node):unit =
		add_predecessor k node table
	in (Hashtbl.iter map_add table)

let filter (k:stmt_key) (n:stmt_node) (init:stmt_key list): stmt_key list=
	match n.succ with 
	| [] -> k :: init 
	| _ -> init

(* Find all the statements in the program which dont have sucessors *)
(* We need them for liveness analysis *)
let find_stmts_without_successors (table: stmt_table): stmt_key list =
	Hashtbl.fold filter table []

let compute_in_from_out (out: id3_set) (use: id3_set) (def: id3_set): id3_set=
	Id3Set.union use (Id3Set.diff out def)

let union_list (l: (id3_set) list) :id3_set=
	List.fold_left Id3Set.union Id3Set.empty l

(* Given a node of key k, we will extract all the ins of the predecessors to build the out *)
let update_out (table: stmt_table) (k: stmt_key): id3_set=
	let node = (Hashtbl.find table k) in
	let successors = node.succ in
	let rec helper (succ: stmt_key list): id3_set list = 
		match succ with
		| head::tail ->
			let n = (Hashtbl.find table head) in
			n.live_in :: helper tail
		| [] -> []
	in let in_list = helper successors
	in let return_out = match node.stmt with
		   				| ReturnStmt3 var ->
							Id3Set.singleton var
						| _ ->
							Id3Set.empty
	in union_list (return_out::in_list)

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

let reset_bools (table:stmt_table): unit = 
	let rec f (k:stmt_key) (n:stmt_node):unit  =
		begin
			n.changed <- false;
			n.visited <- false;
		end
	in Hashtbl.iter f table

(* Analyze the stmt table and modify the stmt node *)
let liveness_analysis (table: stmt_table): unit = 
	let terminals = find_stmts_without_successors table in
	let rec helper (stmt_list:stmt_key list): unit =
		if visit_terminals stmt_list table
		then 
			begin
				reset_bools table;
				helper stmt_list
			end
		else ()
	in helper terminals

let rec string_of_list_of_int (l: int list):string =
	match l with
	| head::tail ->
		(string_of_int head) ^ " " ^ (string_of_list_of_int tail)
	| [] ->
		 ""

let print_id3_color (v:id3) color_graph:string =
	let color = Hashtbl.find color_graph v in
	("\t" ^ v ^ "  -> " ^ (string_of_int color) ^ "\n")

let print_node node color_graph :unit =
	let format_id3 e = print_id3_color e color_graph in
	begin
	print_string ((string_of_int node.id) ^ ": " ^ (string_of_ir3_stmt node.stmt) ^ "\n");
	print_string ("\tPredecessors: " ^ (string_of_list node.pred string_of_int " ") ^ "\n" );
	print_string ("\tSuccessors: " ^ (string_of_list node.succ string_of_int " ") ^ "\n" );
	print_string ("\tdef:\n" ^ (string_of_list (Id3Set.elements  node.def) (format_id3) " ") ^ "\n" );
	print_string ("\tuse:\n" ^ (string_of_list (Id3Set.elements  node.use) (format_id3) " ") ^ "\n" );
	print_string ("\tLive IN: \n" ^ (string_of_list (Id3Set.elements  node.live_in) (format_id3) " ") ^ "\n" );
	print_string ("\tLive OUT: \n" ^ (string_of_list (Id3Set.elements  node.live_out) (format_id3) " ") ^ "\n" );
	end

let print_stmt_list (stmt_l: stmt_node list) (color_graph): unit =
	let f node = print_node node color_graph in
	List.iter f stmt_l;;

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

let f k v init = v :: init

let stmt_list_from_hashtable (hashtable:stmt_table):stmt_node list =
	Hashtbl.fold f hashtable []
(* 
(* Retrieve the edge set from the graph table *)
let find_graph (mthd:id3) (table:graph_table) :edge_set =
	let all_graphs_found = Hashtbl.find_all table mthd in
	match all_graphs_found with 
	| head:: [] ->
		head
	| [] ->
		let new_edge_set =  in
		begin
			Hashtbl.add table mthd new_edge_set;
			new_edge_set
		end
	| _ -> failwith "99"


let update_graph graph_key new_edge_set1 new_edge_set2 all_graphs:unit =
	let graph = find_graph graph_key all_graphs in
	let new_graph = EdgeSet.union graph new_edge_set1 in
	let new_new_graph = EdgeSet.union new_graph new_edge_set2 in
	begin
		Hashtbl.remove all_graphs graph_key;
		Hashtbl.add all_graphs graph_key new_new_graph;
	end *)

(* Create the edge set for one method *)
let create_graph_from_stmt_table (table:stmt_table):edge_set =
	let stmt_list = stmt_list_from_hashtable table in
	let rec helper (graph:edge_set) 
                   (stmt_list: stmt_node list) :edge_set =
		match stmt_list with
		| head::tail -> 
			let live_in_edges = cartesian_square head.live_in in
			let live_out_edges = cartesian_square head.live_out in
			let new_graph = EdgeSet.union graph live_in_edges in
			let new_new_graph = EdgeSet.union new_graph live_out_edges in
			(helper new_new_graph tail)
		| [] ->
			graph
	(* Initial table size so that Ocaml do not increase size too often *)
	in (helper (EdgeSet.empty) stmt_list)


(* Hashtable of the color of the variables *)
type colored_table = 
	(id3, int) Hashtbl.t

module ColorSet = Set.Make(
	struct
	    let compare :int -> int -> int = compare 
	    type t = int
	  end
	);;

type color_set = ColorSet.t

let is_linked (e1, e2:edge) (f:id3):bool=
	(are_equal_id3 e1 f) || (are_equal_id3 e2 f)

let find_all_linked_var e_set var =
	let return_linked_var (e1, e2) v init:id3_set=
		if (are_equal_id3 e1 v)
		then Id3Set.add e2 init
		else
			if (are_equal_id3 e2 v)
			then Id3Set.add e1 init
			else init
	in let f (e1, e2) init = return_linked_var (e1, e2) var init
	in EdgeSet.fold f e_set Id3Set.empty


let find_all_colors id3_all_set table_color =
	let f var init =
		match Hashtbl.find_all table_color var with
			| head::[] -> ColorSet.add head init
			| [] -> init
			| _ -> failwith "789"
	in Id3Set.fold f id3_all_set ColorSet.empty

let color_variables_stack (e_set: edge_set) (stack_variables: id3 list) : colored_table =
	let rec helper e_set stack_var max_color color_table:colored_table =
		match stack_var with
		| head::tail ->	
			let id3set_linked = find_all_linked_var e_set head in
			let all_colors_linked_set = find_all_colors id3set_linked color_table in
			let max_color_found =
				try ColorSet.max_elt all_colors_linked_set
					with Not_found -> 1
			in let rec helper_2 color_set current_color max_color =
				if current_color == max_color +1
				then max_color +1, max_color +1
				else 
					let is_same_color (c:int):bool = (c == current_color) in
					if ColorSet.exists is_same_color color_set
					then helper_2 color_set (current_color+1) max_color
					else current_color, max_color
			in let color_found, new_max_color = helper_2 all_colors_linked_set 1 max_color_found in
			begin
				Hashtbl.add color_table head color_found;
				helper e_set tail new_max_color color_table
			end
		| [] -> color_table
	in (helper e_set stack_variables 0 (Hashtbl.create 1000))

let max_color_in_colored_table colored_table =
	let f var elt init =
		if elt > init
		then elt
		else init in
	Hashtbl.fold f colored_table 0

(* Choose max_color - max_register variables *)
(* TODO: To optimize. choose the less busy color *)
(* Return a new color table *)
let choose_spilled_vars color_table (max_registers:int): id3_set =
	(* No need to find the max color here, maybe later *)
	(* let max_color = max_color_in_colored_table colored_table in *)
	let f k v init = 
		if v > max_registers
		then Id3Set.add k init
		else init in
	Hashtbl.fold f color_table Id3Set.empty

let convert_stmt_node_load_str stmt_node variables_spilled_set :ir3_stmt list =
	let f elt init = 
		let equal_elt v = are_equal_id3 elt v in
		if Id3Set.exists equal_elt stmt_node.use 
		then (LoadStmt3 elt) :: init
		else init
	in 
	let g elt init = 
		let equal_elt v = are_equal_id3 elt v in
		if Id3Set.exists equal_elt stmt_node.def
		then init @ [StrStmt3 elt]
		else init
	in let load_stmt_added = Id3Set.fold f variables_spilled_set [stmt_node.stmt]
	in Id3Set.fold g variables_spilled_set load_stmt_added
	

let add_str_load_stmt_in_ir3_program (stmt_node_list: stmt_node list) variables_spilled_set: ir3_stmt list =
(* 	let p f = print_string (f ^ "\n") in
	Id3Set.iter p variables_spilled_set;
	print_string "\n"; *)
	let rec helper ir3_stmt_list: ir3_stmt list=
		match ir3_stmt_list with
		| head::tail ->
			(* print_node ((convert_stmt_node_load_str head variables_spilled_set)); *)
			(convert_stmt_node_load_str head variables_spilled_set) @
				(helper tail)
		| [] -> []
	in (helper stmt_node_list)


(*creates a list of statement nodes, with correct successors given the program *)
let rec create_updated_stmt_node_list ((class_decl,main,mds):ir3_program): stmt_node list =
	let rec helper (mds:md_decl3 list): stmt_node list =
		match mds with
		| head::tail ->
			(find_all_successors (create_stmt_node_list head.ir3stmts head))@(helper tail)
		| [] ->
			[]
	in (helper (main::mds)) 

(* this partition returns true if none of the ends of the edge contains var *)
let partition_function (var: id3) (n1,n2:id3 * id3): bool =
	(not (are_equal_id3 var n1)) && (not (are_equal_id3 var n2))

(* Inria webpage saus such a function already exists *)
let rec add_list_to_set l s =
	match l with
	| head::tail ->
		let new_set = Id3Set.union s (Id3Set.singleton head) in
		add_list_to_set tail new_set
	| [] ->
		s
(* Returns a set of nodes adjacent to the given node *)
let get_nodes_adjacent_to (var:id3) (set:edge_set): id3_set =
	let rec helper (var: id3) (edges: (id3 * id3) list): id3 list = 
		match edges with
		| (n1, n2)::tail ->
			if are_equal_id3 n1 var 
			then n2::helper var tail
			else helper var tail
		| [] ->
			[]
	in 
	let var_list = helper var (EdgeSet.elements set) in
	let var_set = add_list_to_set var_list Id3Set.empty
	in var_set

(* returns true if graph contains edge *)
let edge_set_contains (edge: id3 * id3) (graph: edge_set): bool =
	let m = EdgeSet.cardinal graph in
	let n = EdgeSet.cardinal (EdgeSet.diff graph (EdgeSet.singleton edge)) in
	print_string ("m = " ^ (string_of_int m) ^ "\n");
	print_string ("n = " ^ (string_of_int n) ^ "\n");
	not (m == n)

let string_of_edge ((e1,e2): id3 * id3): string = 
	"(" ^ e1 ^ "," ^ e2 ^ ")"

(*returns true if the set of variables forms a clique in the given graph*)
let is_a_clique (vars: id3_set) (graph: edge_set): bool =
	let rec helper edge_list graph =
	match edge_list with
	| head::tail ->
		let (e1,e2) = head in
		print_string ("Graph: " ^ (string_of_list (EdgeSet.elements graph) (string_of_edge) " ") ^ "\n");
		print_string ("Does it contain: ("^ e1 ^ "," ^ e2 ^ ") \n");
		if not (edge_set_contains head graph)
		then false
		else helper tail graph
	| [] ->
		true
	in 
	let all_edges_set = cartesian_square vars in
	let all_edges = EdgeSet.elements all_edges_set in
	helper all_edges graph

(* Find a node whose neighbours form a clique *)
let find_node_to_remove (set:edge_set): id3 = 
	let rec helper (edges:(id3 * id3) list) (set:edge_set): id3 = 
	match edges with
	| (h,_)::tail ->
		let neighbours = get_nodes_adjacent_to h set in
		print_string ("\t" ^ h ^ ": " ^ (string_of_list  (Id3Set.elements neighbours) (fun a -> a) " ") ^ "\n");
		if is_a_clique neighbours set
		then h
		else helper tail set
	| _ -> failwith "#356 This should not happen in a chordal graph"
	in helper (EdgeSet.elements set) set

(*returns a graph where all the edges containing v are removed *)
let remove_edges_containing (v:id3) (set:edge_set):edge_set =
	EdgeSet.filter (partition_function v) set

let rec perfect_elimination_ordering_4 (set:edge_set): id3 list = 
	print_string ("####################################################\n");
	print_string ("Cardinal: " ^ (string_of_int (EdgeSet.cardinal set)) ^ "\n");
	let n = EdgeSet.cardinal set in
	match n with
	| 0 ->
		[]
	| 2 ->
		begin
			let (e1, e2) = (List.hd (EdgeSet.elements set)) in
			e1::[e2]
		end
	| _ ->
		begin
		let v = find_node_to_remove set in
		print_string (v ^ "\n");
		let new_set = remove_edges_containing v set in
		print_string ("New Graph: " ^ (string_of_list (EdgeSet.elements new_set) (string_of_edge) " ") ^ "\n");
		print_string ("Cardinal: " ^ (string_of_int (EdgeSet.cardinal new_set)) ^ "\n");
		v::(perfect_elimination_ordering_4 new_set)
		end
		
let rec perfect_elimination_ordering_2 (set:edge_set): id3 list = 
	let v = find_node_to_remove set in
	print_string (v ^ "\n");
	let new_set = remove_edges_containing v set in
	if (EdgeSet.is_empty new_set)
	then [v]
	else v::(perfect_elimination_ordering_2 new_set)

let find_remove_from_set (i_set:id3_set): id3 * id3_set =
	let element_chosen = Id3Set.min_elt i_set in
	let new_i_set = Id3Set.remove element_chosen i_set in
	element_chosen, new_i_set


let get_all_nodes e_set : id3_set = 
	let f elt init = 
		let a, b = elt in 
		let new_init = Id3Set.add a init in
		Id3Set.add b init
	in
	EdgeSet.fold f e_set Id3Set.empty

let is_edged (e_set:edge_set) (f1:id3) (f2:id3)  :bool=

	let f elt init = ((is_linked elt f1) && (is_linked elt f2)) || init in
	EdgeSet.fold f e_set false

let process_s (v:id3)(e_set:edge_set)  (i_set:id3_set): id3_set list = 

	let linked_id3_set, not_linked_id3_set =  Id3Set.partition (is_edged e_set v) i_set
	in let new_S =

				if Id3Set.is_empty not_linked_id3_set
				then []
				else [not_linked_id3_set]
	in
	if (Id3Set.is_empty linked_id3_set)
	then new_S
	else [linked_id3_set] @ new_S


let perfect_elimination_ordering (e_set:edge_set) (variables: id3 list) : id3 list =
	(* let all_nodes = get_all_nodes e_set in *)
	let all_nodes = add_list_to_set variables Id3Set.empty in

	let sigma_list = ref [all_nodes] in
	let is_sigma_list_empty = ref ((List.length !sigma_list) == 0) in
	let output_vertices = ref [] in
	begin
	while (not !is_sigma_list_empty) do
		let v, new_i_set = find_remove_from_set (List.hd (!sigma_list)) in
		begin
			sigma_list :=
				if (Id3Set.is_empty new_i_set)
				then List.tl (!sigma_list)
				else new_i_set :: (List.tl (!sigma_list))
			;
		output_vertices := !output_vertices @ [v];
		let f  (init:id3_set list)  (elt:id3_set)= (process_s v e_set elt) @ init in 
		sigma_list := List.fold_left f [] (!sigma_list);
		(* print_string (string_of_int ((List.length (!sigma_list))) ^ "\n"); *)
		(* (List.hd (!sigma_list)); *)
		is_sigma_list_empty := ((List.length (!sigma_list)) == 0);
		end
	done;
	!output_vertices
	end


type md_key = id3
type md_struct =
	{
		id: md_key;
		colored_tab: colored_table;
		md: md_decl3;
		stmt_tab: stmt_table;
		stmt_node_list: stmt_node list;
	}

let create_md_struct id_md color_table md_dec stmt_tab new_stmt_list =
	{
		id= id_md;
		colored_tab= color_table;
		md= md_dec;
		stmt_tab= stmt_tab;
		stmt_node_list= new_stmt_list;
	}

(* Hashtable of methods *)
type md_table = 
	(md_key, md_struct) Hashtbl.t

(*creates a list of statement nodes, with correct successors given the program *)
let rec create_updated_stmt_node_list_from_mthd md: stmt_node list =
	find_all_successors (create_stmt_node_list md.ir3stmts md)

let create_hash_table_from_node_list (stmt_node_list:stmt_node list):stmt_table  =
	let rec helper (stmt_node_list: stmt_node list) table:stmt_table =
		match stmt_node_list with
		| head:: tail ->
			begin
			Hashtbl.add table head.id head;
			helper tail table
			end
		| [] -> table
	in (helper stmt_node_list (Hashtbl.create 100))

(* Create stmt table from a method decl *)
let stmt_table_and_stmt_list_from_md md : stmt_table * stmt_node list=
	let nodes_list = (create_updated_stmt_node_list_from_mthd md) in
	let filled_table = create_hash_table_from_node_list nodes_list in
	begin 
		find_predecessors filled_table;
		filled_table, nodes_list
	end

let clean_graph_color (color_table:colored_table) (max_register:int): colored_table = 
	let new_table = (Hashtbl.create 1000) in
	let fff (k:id3) (v:int) :unit =
		begin
			if (v <= max_register)
			then begin (Hashtbl.add new_table k v) end
			else begin (Hashtbl.add new_table k (-1)) end
		end
	in Hashtbl.iter fff color_table;
	new_table

let create_graph_color_from_stmt_table stmt_table variables: colored_table =
	(* Create the edge set *)
	let md_edge_set = create_graph_from_stmt_table stmt_table in
	(* Create the stack variables correctly ordered*)
	let stack_var_list = perfect_elimination_ordering md_edge_set variables in
	(* Color = int *)
	(* Color the stack_var_list *)
	(* Return a hash table : variable -> color *)
	color_variables_stack md_edge_set stack_var_list

(* First create the normal node statements table,
then analyze the liveness,
then color the graphs,
then choose the spilled vars depending of the number of registers available,
then add the load and store statements,
then reanalyze the liveness,
then recolor the graph, 
and store the hash table of the colored/registers  *)
let new_stmt_table_from_md md : md_decl3 * stmt_table * colored_table * (stmt_node list) =
	(* print_string (md.id3 ^ "\n"); *)
	(* TODOOOODODOODOD *)
	let nb_registers_available = 7 in
	(* Get the stmt table from md *)
	let stmt_tab, stmt_list = stmt_table_and_stmt_list_from_md md in
	(* Analyze the stmt table such as initiaze changed, add predecessors, successors,
	and define the live_in and live_out *)
	liveness_analysis stmt_tab;
	let decl_variables = md.localvars3 @ md.params3 in
	let f init (_, v) = v :: init in
	let variables = List.fold_left f [] decl_variables in 
	let color_graph = create_graph_color_from_stmt_table stmt_tab variables in
	

	(* print_stmt_list stmt_list color_graph; *)

	(* print_string "\n\n\n*****2nd* version*************\n\n\n\n\n"; *)

	(* Then color a new table depending of the numbers of registers available *)
	(* Let s start with 5 *)
	let var_spilled = choose_spilled_vars color_graph nb_registers_available in
	(* Add the load and str statements to the ir3 program *)
	let new_ir3_stmt_list = add_str_load_stmt_in_ir3_program stmt_list var_spilled in
	(* Recreate a md object *)
	let new_md = 
		{ 
		  id3= md.id3;	
		  rettype3= md.rettype3;
		  params3= md.params3;
		  localvars3= md.localvars3;
		  ir3stmts= new_ir3_stmt_list; 
 		} in
	(* Recreate a new stmt table from md *)
	let new_stmt_tab, new_stmt_list = stmt_table_and_stmt_list_from_md new_md in

	(* Redo the analysis taking account the new load and str statements *)
	liveness_analysis new_stmt_tab;
	
	let new_color_graph = create_graph_color_from_stmt_table new_stmt_tab variables in
	let cleaned_graph_color = clean_graph_color new_color_graph nb_registers_available in
	new_md, new_stmt_tab, cleaned_graph_color, new_stmt_list

let create_new_md_struct md =
	let new_md_decl, stmt_tab, color_graph, new_stmt_list = new_stmt_table_from_md md in
	(* print_stmt_list new_stmt_list color_graph; *)

	create_md_struct new_md_decl.id3 color_graph new_md_decl stmt_tab new_stmt_list

(* create a hash table of statement where the unique id of a statement is the key *)
let create_md_table (p:ir3_program): md_table= 
	let rec helper (table:md_table) 
                   (md_list: md_decl3 list)
                   :md_table =
		match md_list with
		| head::tail -> 
			let md_structure = create_new_md_struct head in
			begin
				Hashtbl.add table md_structure.id md_structure;
				helper table tail
			end
		| [] -> table
	(* Initial table size so that Ocaml do not increase size too often *)
	in let cdata3_list, main_md, methd_list = p
	in (helper (Hashtbl.create 100) (main_md::methd_list))

let print_graph_color color_graph:unit =
	let f k v = print_string (k ^ "   ->    " ^ (string_of_int v) ^ "\n"); in
	Hashtbl.iter f color_graph

let print_md_struc md_key md_structure:unit =
	begin
	print_graph_color md_structure.colored_tab;
	print_stmt_list md_structure.stmt_node_list md_structure.colored_tab;
	end

let print_md_table table:unit =
	Hashtbl.iter print_md_struc table;;
