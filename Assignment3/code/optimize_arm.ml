
open Jlite_structs
open Ir3_structs
open Arm_structs

(* get the basic blocks from the ARM instructions *)
(* (id, leader flag, instruction ) *)
type block_instr = int * arm_instr 

(* returns in form:  *) 
(* Block: block id, label associated, instrs inside block, instrs leading out *)
type block = 
  {
    id: int;
    label: label;
    mutable instrs: block_instr list;
    mutable instrs_in: (block_instr * label) list; (* instr, label_from *)
    mutable instrs_out: (block_instr * label) list; (* instr, label_to *)
  }

(* -------- Build Basic Blocks of ARM program --------- *)
let errorcount = ref 0
let fresh_error () = (errorcount:=!errorcount+1; !errorcount)

let linecount = ref 0
let fresh_line () = (linecount:=!linecount+1; !linecount)

let blkcount = ref 0
let fresh_blk () = (blkcount:=!blkcount+1; !blkcount)

(* Get last element of a list. Input cannot be an empty list *)
let rec last ls =
  match ls with
  | l::[] -> l
  | l::ls -> last ls
  | _ -> failwith " last isn't working correctly"

(* check if s2 is a substring of s1 *)
let contains_substring s1 s2 =
  try
    let len = String.length s2 in
    for i = 0 to String.length s1 - len do
      if String.sub s1 i len = s2 then raise Exit
    done;
    false
  with Exit -> true

(* Get all prilimary basic blocks *)
let rec get_basic_blocks 
  (instrs: arm_program)
  (blocks: block list)
  (current_blk: block option)
  : block list =
  let block_helper 
    (i: arm_instr)
    (blocks: block list)
    (current_blk: block option)
    : (block list * block) = 
    let instr_helper i (* (branch: bool) *) : (block list * block) =
      let line_id = fresh_line () in
      let new_instruction = (line_id, i) in
      match current_blk with
      | Some curr_blk -> 
        curr_blk.instrs <- List.append curr_blk.instrs [new_instruction];
        (blocks, curr_blk)
      | None -> failwith "#11: instrction block current block doesn't exist." 
    in
    begin
      match i with
      | Label label -> (*create new block*)
        let blk_id = fresh_blk () in
        let line_id = fresh_line () in
        (* update current block *)
        let label_instruction = (line_id, i) in
        let new_blk = {
          id = blk_id;
          label = label;
          instrs = [label_instruction];
          instrs_in = []; 
          instrs_out = []
        } in
        begin
          match current_blk with
          | Some current_blk -> (List.append blocks [current_blk], new_blk)  (* return list of blocks and new (current) block *)
          | None -> ([], new_blk) (* No block exists, create new block *)
        end
      | PseudoInstr pseudo ->
        let line_id = fresh_line () in
        let p_instruction = (line_id, i) in
        begin
          match pseudo, current_blk with
          | _, None ->
            let blk_id = fresh_blk () in
            let new_blk = {
              id = blk_id;
              label = "PseudoInstr" ^ (string_of_int blk_id);
              instrs = [p_instruction];
              instrs_in = []; 
              instrs_out = []
            } in ([], new_blk)
          | pseudo, Some current_blk ->
            if contains_substring pseudo ".asciz" then 
            begin
              current_blk.instrs <- List.append current_blk.instrs [p_instruction];
              (blocks, current_blk)
            end
            else 
              begin
              if String.contains pseudo ':' then
                let strlen = String.length pseudo in
                let label = String.sub pseudo 1 (strlen - 2) in
                let blk_id = fresh_blk () in
                let new_blk = {
                  id = blk_id;
                  label = label;
                  instrs = [p_instruction];
                  instrs_in = []; 
                  instrs_out = [];
                } in (List.append blocks [current_blk], new_blk)
              else
              (* just random pseudo stuff, make new block *)
                let blk_id = fresh_blk () in
                let new_blk = {
                  id = blk_id;
                  label = "PseudoInstr" ^ (string_of_int blk_id);
                  instrs = [p_instruction];
                  instrs_in = []; 
                  instrs_out = [];
                } in (List.append blocks [current_blk], new_blk)
              end
        end
      | BL _ | B _
      | LDMFD _ | STMFD _ 
      | MOV _ | MVN _ | CMP _
      | LDR _ | STR _  
      | SUB _ | AND _
      | ORR _ | MUL _
      | ADD _ -> instr_helper i
      | _ -> failwith "#1: instruction not implemented in block generation"
    end
  in
  match instrs, blocks, current_blk with
  | [], blks, None -> failwith "#22: no instrs were given thus no blocks"
  | [], [], Some current_blk -> 
    [current_blk]
  | [], blks, Some current_blk -> 
    List.append blks [current_blk]
  | i::is, [], None ->
    let (blks, new_current_blk) = block_helper i [] None in
    let next_blks = get_basic_blocks is [] (Some new_current_blk) in
      List.append blks next_blks
  | i::is, [], Some current_blk ->
    let (blks, new_current_blk) = block_helper i [] (Some current_blk) in
    let next_blks = get_basic_blocks is [] (Some new_current_blk) in
      List.append blks next_blks
  | i::is, blks, Some current_blk ->
    let (blks, new_current_blk) = block_helper i blks (Some current_blk) in
    let next_blks = get_basic_blocks is blks (Some new_current_blk) in
      List.append blks next_blks
  | _ -> failwith "#23: get basic blocks error"

let make_blocks arm_instrs = get_basic_blocks arm_instrs [] None

(* Print one block's instructions *)
let print_block_instrs blk =
  let blk_id = "\nBlock " ^ (string_of_int blk.id) ^ " -- " ^ blk.label ^ ":" in
  let rec helper blk_i =
    match blk_i with
    | [] -> []
    | (line_id, i)::is -> i :: (helper is)
  in print_string (blk_id ^ (string_of_arm_prog (helper (blk.instrs))) ^ "\n\n");;

let print_all_block_instrs blks = List.map print_block_instrs blks

(* Convert blockinstr instruction to arm intsr *)
let block_instr_to_arm blk_instr =
  match blk_instr with
  | (ln, instr) -> instr
  (* | _ -> failwith "#shouldn't get here" *)

(* Convert blocks back into arm instructions *)
let blocks_to_arm blks =
  let rec helper blks =
    match blks with
    | [] -> []
    | b::bs -> List.append b.instrs (helper bs)
  in 
  let blks_instrs = helper blks in
    List.map block_instr_to_arm blks_instrs


(* ~~~~~~ PEEPHOLE ~~~~~~ *)

(* 1 -- Remove redundant loads and stores *)
(* Check 2 instructions at a time *)
let redundant_2_instrs blk update_flag : (block * bool) =
  let instrs = blk.instrs in
  if (List.length instrs) == 0 then (blk, false) (* DEFINITION SHOULD BE REMOVING THIS BLOCK :D *)
  else
    let rec helper instrs update_flag =
      let get_result i is flag = 
        if flag == true then
          let (new_instrs, new_flag) = helper is true in
            (i @ new_instrs, true)
        else
          let (new_instrs, new_flag) = helper is flag in
            (i @ new_instrs, new_flag)
      in
      let len = List.length instrs in
      match len, instrs with
      | 0, _ -> ([], update_flag)
      | 1, _ -> (instrs, update_flag)
      | _, i::is ->
        begin
          match i, (List.hd is) with
          | (ln1, LDR (_,_,rd1,addr_type1)), (ln2, STR (_,_,rd2,addr_type2)) ->
            begin
              match addr_type1, addr_type2 with
              | Reg r1, Reg r2 ->
                if (rd1 = r2) && (rd2 = r1) then
                  get_result [i] (List.tl is) true
                else get_result [i] is update_flag
              | _ -> get_result [i] is update_flag
            end
          | (ln1, STR (cond,_,rd1,addr_type1)), (ln2, LDR (_,_,rd2,addr_type2)) ->
            begin
              match addr_type1, addr_type2 with
              | Reg r1, Reg r2 ->
                if (rd1 = r2) && (rd2 = r1) then
                  get_result [i] (List.tl is) true
                else get_result [i] is update_flag
              | RegPreIndexed (r1,off1,flag1), RegPreIndexed (r2,off2,flag2) ->
                begin
                if (rd1 = rd2) && (r1 = r2) && (off1 == off2) then
                  get_result [i] (List.tl is) true
                else
                  if (r1 = r2) && (off1 == off2) then
                    get_result [(ln1, MOV (cond, flag1, rd2, RegOp rd1))] (List.tl is) true
                  else get_result [i] is update_flag
                end
              | RegPostIndexed (r1,off1), RegPostIndexed (r2,off2) ->
                begin
                if (rd1 = rd2) && (r1 = r2) && (off1 == off2) then
                  get_result [i] (List.tl is) true
                else
                  if (r1 = r2) && (off1 == off2) then
                    get_result [(ln1, MOV (cond, false, rd2, RegOp rd1))] (List.tl is) true
                  else get_result [i] is update_flag
                end
              | _ -> get_result [i] is update_flag
            end
          | (ln1, LDR (_,_,rd1,addr_type1)), (ln2, LDR (_,_,rd2,addr_type2)) ->
            if (rd1 = rd2) then
              get_result [List.hd is] (List.tl is) true
            else get_result [i] is update_flag
          | (ln1, STR (_,_,rd1,addr_type1)), (ln2, STR (_,_,rd2,addr_type2)) ->
            if (rd1 = rd2) then
              get_result [List.hd is] (List.tl is) true
            else get_result [i] is update_flag
          | (ln1, MOV (c1,_,rd1,addr_type1)), (ln2, MOV (c2,s,rd2,addr_type2)) ->
            if (rd1 = rd2  && (c1 = "") && (c2 = "")) then
              get_result [List.hd is] (List.tl is) true
            else 
              begin
                match addr_type2 with
                | RegOp rn2 -> 
                  if rd1 = rn2 && (c1 = "") && (c2 = "") then
                    get_result [(ln1, MOV (c2,s,rd2,addr_type1))] (List.tl is) true
                  else get_result [i] is update_flag
                | _ -> get_result [i] is update_flag
              end
            (* get_result [i] is update_flag *)
          | (ln1, MOV (c1,_,rd1,op_type1)), (ln2, STR (c2,suffix,rd2,addr_type2)) ->
            begin
              match op_type1 with
              | RegOp r1 ->
                if (rd1 = rd2)  && (c1 = "") then
                  get_result [(ln1, STR (c2, suffix, r1, addr_type2))] (List.tl is) true
                else get_result [i] is update_flag
              | _ -> get_result [i] is update_flag
            end
          | (ln1, MOV (c_mov,_,rd_mov,mov_addr_typ)), (ln2, next_instr) ->
            begin
              match next_instr with
              | AND (cond,s,rd_op,rn_op_1,op_type) ->
                begin
                  match op_type with
                  | RegOp rn_op_2 ->
                    if rn_op_2 = rd_mov && (c_mov = "") then
                      get_result [(ln2, AND (cond,s,rd_op,rn_op_1,mov_addr_typ))] (List.tl is) true
                    else get_result [i] is update_flag
                  | _ -> get_result [i] is update_flag
                end
              | ORR (cond,s,rd_op,rn_op_1,op_type) ->
                begin
                  match op_type with
                  | RegOp rn_op_2 ->
                    if rn_op_2 = rd_mov && (c_mov = "") then
                      get_result [(ln2, ORR (cond,s,rd_op,rn_op_1,mov_addr_typ))] (List.tl is) true
                    else get_result [i] is update_flag
                  | _ -> get_result [i] is update_flag
                end
              | ADD (cond,s,rd_op,rn_op_1,op_type) ->
                begin
                  match op_type with
                  | RegOp rn_op_2 ->
                    if rn_op_2 = rd_mov && (c_mov = "") then
                      get_result [(ln2, ADD (cond,s,rd_op,rn_op_1,mov_addr_typ))] (List.tl is) true
                    else get_result [i] is update_flag
                  | _ -> get_result [i] is update_flag
                end
              | SUB (cond,s,rd_op,rn_op_1,op_type) ->
                begin
                  match op_type with
                  | RegOp rn_op_2 ->
                    if rn_op_2 = rd_mov && (c_mov = "") then
                      get_result [(ln2, SUB (cond,s,rd_op,rn_op_1,mov_addr_typ))] (List.tl is) true
                    else get_result [i] is update_flag
                  | _ -> get_result [i] is update_flag
                end
              | MUL (cond,s,rd_mul,r_mul_1,r_mul_2) ->
                begin
                  match mov_addr_typ with
                  | RegOp rn_mov ->
                    if r_mul_2 = rn_mov && (c_mov = "") then
                      get_result [(ln2, MUL (cond,s,rd_mul,r_mul_1,rd_mov))] (List.tl is) true
                    else get_result [i] is update_flag
                  | _ -> get_result [i] is update_flag
                end
(*               | CMP (cond,rd_cmp,cmp_op_type) ->
                begin
                  match cmp_op_type with
                  | RegOp rn_cmp ->
                    if rn_cmp = rd_mov && (c_mov = "") then
                      get_result [(ln2, CMP (cond,rd_cmp,(RegOp rd_mov)))] (List.tl is) true
                    else get_result [i] is update_flag
                  | _ -> get_result [i] is update_flag
                end *)
              | _ -> get_result [i] is update_flag
            end
          | (ln1, first_instr), (ln2, MOV (c_mov,_,rd_mov,mov_addr_typ)) ->
            begin
              match first_instr with
              | ORR (cond,s,rd_op,rn_op_1,op_type) ->
                begin
                  match mov_addr_typ with
                  | RegOp rn_mov ->
                    if rd_op = rn_mov && (c_mov = "") then
                      get_result [(ln1, ORR (cond,s,rd_mov,rn_op_1,op_type))] (List.tl is) true
                    else get_result [i] is update_flag
                  | _ -> get_result [i] is update_flag
                end
              | AND (cond,s,rd_op,rn_op_1,op_type) ->
                begin
                  match mov_addr_typ with
                  | RegOp rn_mov ->
                    if rd_op = rn_mov && (c_mov = "") then
                      get_result [(ln1, AND (cond,s,rd_mov,rn_op_1,op_type))] (List.tl is) true
                    else get_result [i] is update_flag
                  | _ -> get_result [i] is update_flag
                end
              | ADD (cond,s,rd_op,rn_op_1,op_type) ->
                begin
                  match mov_addr_typ with
                  | RegOp rn_mov ->
                    if rd_op = rn_mov && (c_mov = "") then
                      get_result [(ln1, ADD (cond,s,rd_mov,rn_op_1,op_type))] (List.tl is) true
                    else get_result [i] is update_flag
                  | _ -> get_result [i] is update_flag
                end
              | SUB (cond,s,rd_op,rn_op_1,op_type) ->
                begin
                  match mov_addr_typ with
                  | RegOp rn_mov ->
                    if rd_op = rn_mov && (c_mov = "") then
                      get_result [(ln1, SUB (cond,s,rd_mov,rn_op_1,op_type))] (List.tl is) true
                    else get_result [i] is update_flag
                  | _ -> get_result [i] is update_flag
                end
              | MUL (cond,s,rd_mul,r_mul_1,r_mul_2) ->
                begin
                  match mov_addr_typ with
                  | RegOp rn_mov ->
                    if rd_mul = rn_mov && (c_mov = "") then
                      get_result [(ln1, MUL (cond,s,rd_mov,r_mul_1,r_mul_2))] (List.tl is) true
                    else get_result [i] is update_flag
                  | _ -> get_result [i] is update_flag
                end
(*               | CMP (cond,rd_cmp,cmp_op_type) ->
                begin
                  match mov_addr_typ with
                  | RegOp rn_mov ->
                    if rd_cmp = rn_mov && (c_mov = "") then
                      get_result [(ln1, CMP (cond,rd_mov,cmp_op_type))] (List.tl is) true
                    else get_result [i] is update_flag
                  | _ -> get_result [i] is update_flag
                end *)
              | _ -> get_result [i] is update_flag
            end
          | _ -> get_result [i] is update_flag
        end
      | _ -> failwith "#30: len and instrs mismatch"
    in let (new_instrs, new_flag) = helper instrs update_flag in 
      blk.instrs <- new_instrs;
      (* print_string (blk.label ^ "\t#2\t" ^ (string_of_bool new_flag) ^ "\n"); *)
      (blk, new_flag)

(* Check 3 instructions at a time *)
let redundant_3_instrs blk update_flag =
  let instrs = blk.instrs in
  if (List.length instrs) == 0 then (blk, false) (* DEFINITION SHOULD BE REMOVING THIS BLOCK :D *)
  else
    let rec helper instrs update_flag =
      let get_result i is flag = 
        if flag == true then
          let (new_instrs, new_flag) = helper is true in
            (i @ new_instrs, true)
        else
          let (new_instrs, new_flag) = helper is flag in
            (i @ new_instrs, new_flag)
      in
      let len = List.length instrs in
      match len, instrs with
      | 0, _ -> ([], false)
      | 1, _ -> (instrs, false)
      | 2, _ -> (instrs, false)
      | _, i1::is ->
        let i2 = List.hd is in
        let i3 = List.hd (List.tl is) in
        (* let iter_count = fresh_error () in *)
          (* print_string ((string_of_int iter_count)^ "  I'm here!! \n") ; *)
        begin
          match i1, i2, i3 with
          | (ln1,STR (_,_,rd1,at1)), (ln2,LDR _), (ln3,LDR (cond,_,rd3,at3)) ->
            (* print_string ((string_of_int iter_count)^ "\tSTR LDR LDR\tinc flag: " ^ (string_of_bool update_flag) ^ "\n") ; *)
            (* print_string ("\t" ^ blk.label ^ "\t\tline: " ^ (string_of_int ln1) ^ "\n\n"); *)
            begin
              match at1, at3 with
              | Reg r1, Reg r3 ->
                if (r1 = r3) && (rd1 = rd3) then
                  let new_i3 = (ln3, MOV (cond, false, rd3, RegOp rd1)) in 
                  let new_is = List.tl (List.tl is) in
                    (* print_string ("\t" ^ blk.label ^ "\t\treg reg true\n\n"); *)
                    get_result [i1; i2; new_i3] new_is true
                else
                  begin
                  (* print_string ("\t" ^ blk.label ^ "\t\treg reg no changes\n\n"); *)
                  get_result [i1] is update_flag
                  end
              | RegPreIndexed (r1,off1,_), RegPreIndexed (r3,off3,_) ->
                if (r1 = r3) && (off1 == off3) then
                  let new_i3 = (ln3, MOV (cond, false, rd3, RegOp rd1)) in 
                  let new_is = List.tl (List.tl is) in
                    (* print_string ("\t" ^ blk.label ^ "\t\tpre-reg pre-reg true\n\n"); *)
                    get_result [i1; i2; new_i3] new_is true
                else 
                (* get_result [i1] is update_flag *)
                  begin
                  (* print_string ("\t" ^ blk.label ^ "\t\tpre-reg pre-reg no changes\n\n"); *)
                  get_result [i1] is update_flag
                  end
              | RegPostIndexed (r1,off1), RegPostIndexed (r3,off3) ->
                if (r1 = r3) && (off1 == off3) then
                  let new_i3 = (ln3, MOV (cond, false, rd3, RegOp rd1)) in 
                  let new_is = List.tl (List.tl is) in
                    (* print_string ("\t" ^ blk.label ^ "\t\tpost-reg post-reg true\n\n"); *)
                    get_result [i1; i2; new_i3] new_is true
                else
                  begin
                  (* print_string ("\t" ^ blk.label ^ "\t\tpost-reg post-reg no changes\n\n"); *)
                  get_result [i1] is update_flag 
                  end
              | _ -> get_result [i1] is update_flag
            end
          | (ln1,MOV (c1,_,rd1,op1)), (_,STR _), (ln3,MOV (c2,_,rd3,op3)) ->
            (* print_string ((string_of_int iter_count)^ "\tMOV STR MOV\tinc flag: " ^ (string_of_bool update_flag) ^ "\n") ; *)
            (* print_string ("\t" ^ blk.label ^ "\t\tline: " ^ (string_of_int ln1) ^ "\n\n"); *)
            begin
              match op1, op3 with
              | ImmedOp op1, ImmedOp op3 ->
                if (rd1 = rd3) && (op1 = op3) && (c1 == "") && (c2 == "") then
                  get_result [i1; i2] (List.tl (List.tl is)) true
                else get_result [i1] is update_flag
              | RegOp r1, RegOp r3 -> 
                if (rd1 = rd3) && (r1 = r3) && (c1 == "") && (c2 == "") then
                  get_result [i1; i2] (List.tl (List.tl is)) true
                else get_result [i1] is update_flag
              | _ -> get_result [i1] is update_flag
            end

          | (ln1,MOV (c1,_,rd1,op1)), (_,LDR (_,_,rd2,at2)), (ln3,MOV (c2,_,rd3,op3)) ->
            (* print_string ((string_of_int iter_count)^ "\tMOV LDR MOV\tinc flag: " ^ (string_of_bool update_flag) ^ "\n") ; *)
            (* print_string ("\t" ^ blk.label ^ "\t\tline: " ^ (string_of_int ln1) ^ "\n\n"); *)
            begin
              match op1, op3 with
              | RegOp r1, RegOp r3 -> 
                if (rd1 = rd3) && (r1 = r3) && (rd2 = r3) && (c1 == "") && (c2 == "") then
                  get_result [i2; i3] (List.tl (List.tl is)) true
                else get_result [i1] is update_flag
              | _ -> get_result [i1] is update_flag
            end
          | _ -> get_result [i1] is update_flag
        end
      | _ -> failwith "#30: len and instrs mismatch"
    in 
    let (new_instrs, new_flag) = helper instrs update_flag in 
      blk.instrs <- new_instrs;
      (* print_string (blk.label ^ "\t#1\t" ^ (string_of_bool new_flag) ^ "\n"); *)
      (blk, new_flag)
      (* blk.instrs <- helper instrs; blk *)

let rec loop_redundancy blk : block = 
  let (blk_1, flag1) = redundant_2_instrs blk false in
  let (blk_2, flag2) = redundant_3_instrs blk_1 false in
  (* let (blk_2, flag2) = redundant_3_instrs blk false in *)
  if flag1 == false && flag2 == false then
  (* if flag1 == false then *)
    (* blk_1 *)
  (* if flag2 == false then *)
    blk_2
  else
    begin
    (* print_string ("Looping...\n"); *)
    (* loop_redundancy blk_1 *)
    loop_redundancy blk_2
    end

(* Algebraic Simplification *)
let algebraic_simplification blk =
  let instrs = blk.instrs in
  if (List.length instrs) == 0 then
    blk (* DEFINITION SHOULD BE REMOVING THIS BLOCK :D *)
  else
    let rec helper instrs = 
      match instrs with
      | [] -> []
      | (id, ADD (cond, s, rd, rn, ImmedOp op))::is->
        if op = "#0" then
          helper is (* remove instruction *)
        else
          List.append [(id, ADD (cond, s, rd, rn, ImmedOp op))] (helper is)
      | (id, SUB (cond, s, rd, rn, ImmedOp op))::is->
        if op = "#0" then
          helper is (* remove instruction *)
        else
          List.append [(id, SUB (cond, s, rd, rn, ImmedOp op))] (helper is)
      | i::is -> List.append [i] (helper is)
    in blk.instrs <- helper instrs; blk

(* Redudant Strings *)
let redundant_strings blks =
  let rec get_str blks = 
    match blks with
    | [] -> None
    | b::bs ->
      if List.length b.instrs == 2 then
        begin
          match b.instrs with
          | (l1, (Label lbl))::(l2, (PseudoInstr pseudo))::is -> 
            if contains_substring pseudo ".asciz" then Some pseudo
            else get_str bs
          | _ -> get_str bs
        end
      else get_str bs
  in
  let rec get_matching_str_labels blks label ls m_str =
    match blks with
    (* eventually delete these blocks. *)
    | [] -> []
    | b::bs ->
      begin
        if b.label = label then
          get_matching_str_labels bs label ls m_str
        else
          let check_str instrs = 
            match List.length instrs, instrs with
            | 2, (l1, (Label lbl))::(l2, (PseudoInstr pseudo))::is ->
              if m_str = pseudo then ["=" ^ lbl]
              else []
            | _, _ -> []
          in let new_ls = ls @ (check_str b.instrs) in
            get_matching_str_labels bs label new_ls m_str
      end
  in
  let rec replace_labels blks label ls = 
    match blks with
    | [] -> []
    | b::bs ->
      let rec update_label instrs = 
        begin
          match instrs with
          | [] -> []
          | (ln, LDR (c,s,rd,LabelAddr addr))::is ->
            if List.exists (fun x -> x = addr) ls then
              (ln, LDR (c,s,rd, LabelAddr ("=" ^ label))) :: update_label is
            else
              (ln, LDR (c,s,rd,LabelAddr addr)) :: update_label is
          | i::is -> i :: update_label is
        end
      in b.instrs <- update_label b.instrs;
        b :: replace_labels bs label ls
  in blks 


  (* delete blocks... later *)
  (* ADD CODE HERE *)
  (* blks *)


(* Apply peephole opts to all blocks *)
let rec apply_peephole blks =
  match blks with
  | [] -> []
  | b::bs ->
    let b_1 = loop_redundancy b in
    let b_2 = algebraic_simplification b_1 in
      List.append [b_2] (apply_peephole bs)

(* OPTIMIZE DAT ARM PROGRAM *)
let optimize_arm (instructions : arm_program) =
  let blks = make_blocks instructions in
  (* print_all_block_instrs blks; *)
  (* print_all_block_instrs_in blks; *)
  let peephole_blocks = apply_peephole blks in
    blocks_to_arm peephole_blocks

(* EOF *)