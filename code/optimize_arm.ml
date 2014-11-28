
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

(* --> Get all prilimary basic blocks *)
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
      | MOV _ | CMP _
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

let rec update_block_list_in
  (blks: block list) 
  (instr: block_instr)
  (label_to: label)
  (label_from: label)
  : block list = 
  match blks with
  | [] -> []
  | b::bs -> 
    if b.label = label_to then 
      begin
        b.instrs_in <- List.append b.instrs_in [(instr, label_from)];
        List.append [b] bs
      end
    else
      List.append [b] (update_block_list_in blks instr label_to label_from)

let rec update_blks_in_out blks =
  match blks with
  | [] -> []
  | b::bs ->
    if contains_substring b.label "Pseudo" then
      [b] @ update_blks_in_out bs
    else
      let rec helper instrs blk_list = 
        match instrs with
        | [] -> blk_list
        | i::is -> 
          begin
            match i with
            | (ln, B (cond, branch_label)) ->
              (* find block with matching label & update *)
              b.instrs_out <- b.instrs_out @ [(i, branch_label)];
              let updated_blks = update_block_list_in blk_list i branch_label b.label in
                helper is updated_blks
            | _ -> helper is blk_list
          end
        in helper b.instrs blks

let rec update_blk_links_final blks =
  match blks with
  | [] -> []
  | b::[] -> blks
  | b::bs ->
    match b, List.hd bs with
    | b1, b2 ->
      let last_instr_b1 = last b1.instrs in
        begin
          match last_instr_b1 with
          | (ln, B ("", label)) ->
            (* print_string ("I'm in~~\n"); *)
            [b1] @ (update_blk_links_final bs)
          | _ ->
            (* print_string ("I'm in~~\n"); *)
            (* gotta add in/out links *)
            let out_instr = (last_instr_b1, b2.label) in
            let in_instr = (last_instr_b1, b1.label) in
              b1.instrs_out <- b1.instrs_out @ [out_instr];
              b2.instrs_in <- b2.instrs_in @ [in_instr];
              [b1] @ (update_blk_links_final (b2 :: List.tl bs))
        end
    (* | _ -> failwith "#55191: You shouldn't be here. Run away young one... run before the debuggers find you." *)

let make_blocks arm_instrs =
  let basic_blks = get_basic_blocks arm_instrs [] None in
  (* update_blk_links_final (update_blks_in_out basic_blks) *)
  update_blks_in_out basic_blks

(* Print block in/out instructions *)
let print_block_instrs_in blk =
  let blk_id = "\nBlock " ^ (string_of_int blk.id) ^ " -- " ^ blk.label ^ ":\n" in
  let rec helper blk_in =
    match blk_in with
    | [] -> []
    | ((ln, i), in_label)::is -> i :: (helper is)
  in print_string (blk_id ^ (string_of_arm_prog (helper (blk.instrs_in))) ^ "\n\n");;

let print_all_block_instrs_in blks = List.map print_block_instrs_in blks

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
let redundant_ldr_str1 blk update_flag : (block * bool) =
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
          | (ln1, MOV (_,_,rd1,addr_type1)), (ln2, MOV (_,_,rd2,addr_type2)) ->
            if (rd1 = rd2) then
              get_result [List.hd is] (List.tl is) true
            else get_result [i] is update_flag
          | (ln1, MOV (_,_,rd1,op_type1)), (ln2, STR (cond,suffix,rd2,addr_type2)) ->
            begin
              match op_type1 with
              | RegOp r1 ->
                if (rd1 = rd2) then
                  get_result [(ln1, STR (cond, suffix, r1, addr_type2))] (List.tl is) true
                else get_result [i] is update_flag
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
let redundant_ldr_str_2 blk update_flag =
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
          | (ln1,STR (cond,_,rd1,at1)), (ln2,LDR (_,_,rd2,at2)), (ln3,LDR (_,_,rd3,at3)) ->
            (* print_string ((string_of_int iter_count)^ "  I'm here!! STR LDR STR \n") ; *)
            (* print_string ("\t" ^ blk.label ^ "\t\tline: " ^ (string_of_int ln1) ^ "\n\n"); *)
            begin
              match at1, at2, at3 with
              | Reg r1, Reg r2, Reg r3 -> 
                if (r1 = r3) && (r2 <> r3) && (rd1 <> rd2) then
                  let new_i3 = (ln3, MOV (cond, false, rd3, RegOp r1)) in 
                  let new_is = List.tl (List.tl is) in
                    get_result [i1; i2; new_i3] new_is true
                  else get_result [i1] is update_flag
              | RegPostIndexed (r1,off1), RegPostIndexed (r2,off2), RegPostIndexed (r3,off3) -> 
                if (r1 = r3) && (off1 == off3) && (off1 != off2) then
                  let new_i3 = (ln3, MOV (cond, false, rd3, RegOp r1)) in
                  let new_is = List.tl (List.tl is) in
                    get_result [i1; i2; new_i3] new_is true
                  else get_result [i1] is update_flag
              | RegPreIndexed (r1,off1,f1), RegPreIndexed (r2,off2,_), RegPreIndexed (r3,off3,_) -> 
                if (r1 = r3) && (off1 == off3) && (off1 != off2) then
                  let new_i3 = (ln3, MOV (cond, f1, rd3, RegOp r1)) in 
                  let new_is = List.tl (List.tl is) in
                    get_result [i1; i2; new_i3] new_is true
                    (* List.append [i1; i2; new_i3] (helper new_is) *)
                  else get_result [i1] is update_flag
                    (* i1 :: helper is *)
              | _ -> get_result [i1] is update_flag
            end
          (* | (ln1,MOV (_,_,rd1,op1)), (ln2,STR (_,_,rd2,at2)), (ln3,MOV (_,_,rd3,op3)) -> *)
          | (ln1,MOV (_,_,rd1,op1)), (_, STR _), (ln3,MOV (_,_,rd3,op3)) ->
            (* print_string ((string_of_int iter_count) ^ "  I'm here!! MOV STR MOV\n"); *)
            (* print_string ("\t" ^ blk.label ^ "\t\tline: " ^ (string_of_int ln1) ^ "\n\n"); *)
            begin
              match op1, op3 with
              | RegOp r1, RegOp r3 -> 
                if (r1 = r3) && (rd1 = rd3) then
                  get_result [i1; i2] is true
                else get_result [i1] is update_flag
              | ImmedOp op1, ImmedOp op3 ->
                if (op1 = op3) && (rd1 = rd3) then
                  get_result [i1; i2] (List.tl (List.tl is)) true
                else get_result [i1] is update_flag
              | _ -> get_result [i1] is update_flag
            end
          (* | (ln1,MOV (_,_,rd1,op1)), (ln2,LDR (_,_,rd2,at2)), (ln3,MOV (_,_,rd3,op3)) -> *)
          | (ln1,MOV (_,_,rd1,op1)), (_, LDR _), (ln3,MOV (_,_,rd3,op3)) ->
            (* print_string ((string_of_int iter_count) ^ "  I'm here!! MOV LDR MOV\n") ; *)
            begin
              match op1, op3 with
              | RegOp r1, RegOp r3 -> 
                if (r1 = r3) && (rd1 = rd3) then
                  get_result [i1; i2] is true
                else get_result [i1] is update_flag
              | ImmedOp op1, ImmedOp op3 ->
                if (op1 = op3) && (rd1 = rd3) then
                  get_result [i1; i2] is true
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
  let (blk_1, flag1) = redundant_ldr_str1 blk false in
  let (blk_2, flag2) = redundant_ldr_str_2 blk_1 false in
  if flag1 == false && flag2 == false then
    blk_2
  else
    loop_redundancy blk_2

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


(*let flow_of_control blks : block list = 
  let rec find_asciz b asciz_str blks = 
    if List.length b.instrs > 1 then
      match List.hd b.instrs, List.hd (List.tl b.instrs) with
      | (_, Label label), (_, PseudoInstr pseudo) ->
(*        if contains_substring pseudo ".asciz" then *)
        if asciz_str = pseudo then 
          let rec finder blks =
            begin
              match blks with
              | [] -> []
              | b::bs ->
                if b.label = label then
                  (* continue on to next block *)
                  b :: finder bs
                else 
                  let rec gothroughinstrs instrs = 
                    match instrs with
                    | [] -> []
                    | (ln, LDR (_,_,_,LabelAddr (addr))) ->
                      if string_contains label addr then

                    | _ -> continue searching
            end
           (* go through each block and check if there's BL's
              with references to the label *)
          (* check against the one of the in coming block *)
          
      | (_, PseudoInstr pseudo) ->
        if contains_substring pseudo ".asciz" then
          (* do that stuff *)
        else (* go to next block *)
      | _ -> yada(* go to next block *)
    else (* go to the next blk *)
    in let update label bs = 
    
    match List.head b.instrs with
    | _ -> yada (* go to next block *)
  in
  match blks with
  | [] -> 
*)


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










(* ----------- TESTS ----------- *)

(* let test_instrs = [Label "hello"];; *)
(* let test_instrs0 = Label "hello" :: ADD ("", false, "a1", "a0", RegOp ("v1")) :: ADD ("", false, "a1", "a0", RegOp ("v1")) :: Label "bye":: [];; *)
(* let test_instrs1 = Label "hello" :: ADD ("", false, "a1", "a0", RegOp ("v1")) :: LDR ("", "", "v1", Reg ("a1")) :: [];; *)
let test_instrs2 = 
     PseudoInstr (".data")
  :: PseudoInstr (".test")
  :: Label "hello" 
  :: ADD ("", false, "a1", "a0", RegOp ("v1"))
  :: LDR ("", "", "v1", Reg ("a1"))
  :: STR ("", "", "a1", Reg ("v1"))
  :: STR ("", "", "v2", (RegPreIndexed ("fp", -3 , false)))
  :: STR ("", "", "v2", (RegPreIndexed ("fp", -3 , false)))
  :: STR ("", "", "a1", (RegPreIndexed ("fp", -124 , false)))
  :: LDR ("", "", "v2", (RegPreIndexed ("fp", -999 , false)))
  :: LDR ("", "", "a2", (RegPreIndexed ("fp", -124 , false)))
  :: LDR ("", "", "v2", (RegPreIndexed ("fp", -3 , false)))
  :: LDR ("", "", "v2", (RegPreIndexed ("fp", -3 , false)))
  :: LDR ("", "", "v2", (RegPreIndexed ("fp", -3 , false)))
  :: SUB ("", false, "a1", "a0", ImmedOp "#0")
  :: SUB ("", false, "a1", "a0", ImmedOp "#0")
  :: PseudoInstr "\n.L1exit:"
  :: AND ("", false, "a1", "a0", RegOp ("v1"))
  :: STR ("", "", "v1", (RegPreIndexed ("fp", -5 , false)))
  :: LDR ("", "", "v2", (RegPreIndexed ("fp", -5 , false)))
  :: ORR ("", false, "a1", "a0", RegOp ("v1"))
  :: BL ("", "printf(PLT)") (* only storing last instr *)
  :: ORR ("", false, "a1", "a0", RegOp ("v1"))
  :: Label "bye"
  :: B ("", "bye")
  :: [];;

(* let test_blocks = get_basic_blocks test_instrs [] None;; *)
(* let test_blocks0 = get_basic_blocks test_instrs0 [] None;; *)
(* let test_blocks1 = get_basic_blocks test_instrs1 [] None;; *)
(* let test_blocks2 = make_blocks test_instrs2;;
(* print_string ("Test instrs set 0");;
print_string ("\n****Input instrs:" ^ (string_of_arm_prog test_instrs0) ^ "\n");;
print_string ("Num of blocks: " ^ (string_of_int (List.length (test_blocks0))));; (* flush std_out;; *)
print_len_block_instrs_head (test_blocks0);;
print_block_instrs (List.hd (test_blocks0));; *)


print_string ("\n\n~~~~~~ Test instrs set 1 ~~~~~~~");;
(* print_string ("\n****Input instrs:" ^ (string_of_arm_prog test_instrs1) ^ "\n");; *)
print_string ("Num of blocks: " ^ (string_of_int (List.length (test_blocks1))));; (* flush std_out;; *)
print_len_block_instrs_head (test_blocks1);;
print_block_instrs (List.hd (test_blocks1));;


(* print_string ("\n\n~~~~~~ Test instrs set 2 ~~~~~~~");; *)
(* print_string ("\n****Input instrs:" ^ (string_of_arm_prog test_instrs2) ^ "\n");; *)
print_string ("\nNum of blocks: " ^ (string_of_int (List.length (test_blocks2))));; (* flush std_out;; *)
print_all_block_instrs test_blocks2;;
print_string ("******* IN LINKS ******");;
print_all_block_instrs_in test_blocks2;;

print_string ("******* PEEPHOLE ******");;
(* print_len_block_instrs_head (test_blocks2);; *)
(* print_block_instrs (List.hd (test_blocks2));; *)
print_string ("\n\nPeephole: rm redundant load/store (on block1)\n");;
let peephole_all_blks = apply_peephole test_blocks2;;
print_all_block_instrs peephole_all_blks;; *)
(* print_string ("\t~~ 1 - LD/STR ~~");;
let peephole1_block = redundant_ldr_str1 (List.hd test_blocks2);;
print_block_instrs peephole1_block;;
print_string ("\t~~ 2 - ALGEBRAIC ~~");;
let peephole2_block = algebraic_simplification (List.hd test_blocks2);;
print_block_instrs peephole1_block;; *)



(* EOF *)
