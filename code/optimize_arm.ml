
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
    mutable instrs_in: (block_instr * int * label) list;
    mutable instrs_out: (block_instr * int * label) list
  }

(* -------- Build Basic Blocks of ARM program --------- *)

let linecount = ref 0
let fresh_line () = (linecount:=!linecount+1; !linecount)

let blkcount = ref 0
(*let fresh_blk () = (blkcount:=!blkcount+1; (string_of_int !blkcount)) *)
let fresh_blk () = (blkcount:=!blkcount+1; !blkcount)

let rec last ls =
(*let rec last (ls: (block_instr * int * label) list) = *)
  match ls with
  | l::[] -> l
  | l::ls -> last ls
  | _ -> failwith " last isn't working right"

(* check if s2 is a substring of s1 *)
let contains_substring s1 s2 =
  try
    let len = String.length s2 in
    for i = 0 to String.length s1 - len do
      if String.sub s1 i len = s2 then raise Exit
    done;
    false
  with Exit -> true

(* Print one block's instructions *)
let print_block_instrs blk =
  let blk_id = "\nBlock " ^ (string_of_int blk.id) ^ " instructions:" in
  let rec helper blk_i =
    match blk_i with
    | [] -> []
    | (line_id, i)::is -> i :: (helper is)
  in print_string (blk_id ^ (string_of_arm_prog (helper (blk.instrs))) ^ "\n\n");;

(* print length of one block's instructions *)
let get_block_instrs_len blk = List.length (blk.instrs)

(* Print first block's instructions length *)
let print_len_block_instrs_head blks =
  let len = get_block_instrs_len (List.hd blks) in
  print_string ("\nhead instrs len:" ^ string_of_int len ^ "\n");;

(* Print first block's instructions in block list *)
let print_block_instrs_head blks =
  let blk = List.hd blks in
  let rec helper blk_i =
    match blk_i with
    | [] -> []
    | (line_id, i)::is -> i :: (helper is)
  in print_string ("**head block insts \n"^ (string_of_arm_prog (helper (blk.instrs))) ^ "\n\n");;

let print_all_block_instrs blks = 
  List.map print_block_instrs blks


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
          match current_blk with
          | Some current_blk ->
            (* check if label or just random *)
            if String.contains pseudo ':' then
              begin
              let strlen = String.length pseudo in
              let label = String.sub pseudo 2 (strlen - 3) in
              (* let label = "Filler!" in *)
              let blk_id = fresh_blk () in
              let new_blk = {
                id = blk_id;
                label = label;
                instrs = [p_instruction];
                instrs_in = []; 
                instrs_out = []
              } in 
              (List.append blocks [current_blk], new_blk)
              end
            else 
              begin
              (* just random pseudo stuff add to end of block *)
              current_blk.instrs <- List.append current_blk.instrs [p_instruction];
              (blocks, current_blk)
              end
          | None ->
            let blk_id = fresh_blk () in
            let new_blk = {
              id = blk_id;
              label = "PseudoStart" ^ (string_of_int blk_id);
              instrs = [p_instruction];
              instrs_in = []; 
              instrs_out = []
            } in ([], new_blk)
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
    (* print_string ("last -- 1 blk\n"); *)
    (* print_block_instrs current_blk; *)
    [current_blk]
  | [], blks, Some current_blk -> 
    (* print_string ("last -- |blks| > 1\n"); *)
    (* print_block_instrs current_blk; *)
    List.append blks [current_blk]
  | i::is, [], None ->
    (* print_string ("pass #1\n"); *)
    let (blks, new_current_blk) = block_helper i [] None in
    let next_blks = get_basic_blocks is [] (Some new_current_blk) in
      (* print_block_instrs new_current_blk; *)
      List.append blks next_blks
  | i::is, [], Some current_blk ->
    (* print_string ("pass #2\n"); *)
    let (blks, new_current_blk) = block_helper i [] (Some current_blk) in
    let next_blks = get_basic_blocks is [] (Some new_current_blk) in
      (* print_block_instrs new_current_blk; *)
      List.append blks next_blks
  | i::is, blks, Some current_blk ->
    (* print_string ("pass #3\n"); *)
    let (blks, new_current_blk) = block_helper i blks (Some current_blk) in
    let next_blks = get_basic_blocks is blks (Some new_current_blk) in
      (* print_block_instrs new_current_blk; *)
      List.append blks next_blks
  | _ -> failwith "#23: get basic blocks error"

(* TO DO >>>>> Add in/out links *)



(* ~~~~~~ PEEPHOLE ~~~~~~ *)

(* 1 -- Remove redundant loads and stores *)
(* input : 1 block *)
(* if only 1 instruction, return *)
(* go through each instruction. if one is load, check to see if the next is a store.
    if the store has the same info, rm the str instruction.
  if the str is the last instruction, update instrs out accordingly *)
let remove_redundant_ldr_str blk =
  let instrs = blk.instrs in
  if (List.length instrs) == 0 then
    blk (* DEFINITION SHOULD BE REMOVING THIS BLOCK :D *)
  else
    let rec helper instrs =
      let len = List.length instrs in
      match len, instrs with
      | 0, _ -> []
      | 1, _ -> instrs
      | _, i::is ->
        begin
        match i, (List.hd is) with
        | (ld1, LDR (_,_,rd1,addr_type1)), (ld2, STR (_,_,rd2,addr_type2)) ->
          begin
          match addr_type1, addr_type2 with
          | Reg r1, Reg r2 ->
            if (rd1 = r2) && (rd2 = r1) then
              List.append [i] (helper (List.tl is))
            else i :: helper is
          | _ -> i :: helper is
          end
        | (ld1, STR (cond,_,rd1,addr_type1)), (ld2, LDR (_,_,rd2,addr_type2)) ->
          begin
          match addr_type1, addr_type2 with
          | Reg r1, Reg r2 ->
            if (rd1 = r2) && (rd2 = r1) then
              List.append [i] (helper (List.tl is))
            else i :: helper is
          | RegPreIndexed (r1,off1,flag1), RegPreIndexed (r2,off2,flag2) ->
            begin
            if (rd1 = rd2) && (r1 = r2) && (off1 == off2) then
              helper (List.tl is)
            else
              if (r1 = r2) && (off1 == off2) then
                List.append [(ld1, MOV (cond, flag1, rd2, RegOp rd1))] (helper (List.tl is))
              else
                i :: helper is 
            end
          | RegPostIndexed (r1,off1), RegPostIndexed (r2,off2) ->
            begin
            if (rd1 = rd2) && (r1 = r2) && (off1 == off2) then
              helper (List.tl is)
            else
              if (r1 = r2) && (off1 == off2) then
                List.append [(ld1, MOV (cond, false, rd2, RegOp rd1))] (helper (List.tl is))
              else
                i :: helper is 
            end
          | _ -> i :: helper is
          end
        | (ld1, LDR (_,_,rd1,addr_type1)), (ld2, LDR (_,_,rd2,addr_type2)) ->
          if (rd1 = rd2) && (addr_type1 = addr_type2) then
            List.append [i] (helper (List.tl is))
          else i :: helper is 
        | (ld1, STR (_,_,rd1,addr_type1)), (ld2, STR (_,_,rd2,addr_type2)) ->
          if (rd1 = rd2) && (addr_type1 = addr_type2) then
            List.append [i] (helper (List.tl is))
          else i :: helper is 
        | _ -> i :: helper is
        end
      | _ -> failwith "#30: len and instrs mismatch"
    in blk.instrs <- helper instrs; blk

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





(* TESTS *)

let test_instrs = [Label "hello"];;
let test_instrs0 = Label "hello" :: ADD ("", false, "a1", "a0", RegOp ("v1")) :: ADD ("", false, "a1", "a0", RegOp ("v1")) :: Label "bye":: [];;
let test_instrs1 = Label "hello" :: ADD ("", false, "a1", "a0", RegOp ("v1")) :: LDR ("", "", "v1", Reg ("a1")) :: [];;
let test_instrs2 = Label "hello" 
  :: ADD ("", false, "a1", "a0", RegOp ("v1"))
  :: LDR ("", "", "v1", Reg ("a1"))
  :: STR ("", "", "a1", Reg ("v1"))
  :: STR ("", "", "v2", (RegPreIndexed ("fp", -3 , false)))
  :: LDR ("", "", "v2", (RegPreIndexed ("fp", -3 , false)))
  (* :: PseudoInstr "\n.L1exit:" *)
  :: SUB ("", false, "a1", "a0", ImmedOp "#0")
  :: SUB ("", false, "a1", "a0", ImmedOp "#0")
  :: AND ("", false, "a1", "a0", RegOp ("v1"))
  :: STR ("", "", "v1", (RegPreIndexed ("fp", -5 , false)))
  :: LDR ("", "", "v2", (RegPreIndexed ("fp", -5 , false)))
  :: ORR ("", false, "a1", "a0", RegOp ("v1"))
  :: BL ("", "printf(PLT)") (* only storing last instr *)
  :: Label "bye"
  :: B ("", ".1")
  :: [];;

(* let test_blocks = get_basic_blocks test_instrs [] None;; *)
(* let test_blocks0 = get_basic_blocks test_instrs0 [] None;; *)
(* let test_blocks1 = get_basic_blocks test_instrs1 [] None;; *)
let test_blocks2 = get_basic_blocks test_instrs2 [] None;;

(* print_string ("Test instrs set 0");;
print_string ("\n****Input instrs:" ^ (string_of_arm_prog test_instrs0) ^ "\n");;
print_string ("Num of blocks: " ^ (string_of_int (List.length (test_blocks0))));; (* flush std_out;; *)
print_len_block_instrs_head (test_blocks0);;
print_block_instrs (List.hd (test_blocks0));; *)

(* 
print_string ("\n\n~~~~~~ Test instrs set 1 ~~~~~~~");;
(* print_string ("\n****Input instrs:" ^ (string_of_arm_prog test_instrs1) ^ "\n");; *)
print_string ("Num of blocks: " ^ (string_of_int (List.length (test_blocks1))));; (* flush std_out;; *)
print_len_block_instrs_head (test_blocks1);;
print_block_instrs (List.hd (test_blocks1));;
 *)

print_string ("\n\n~~~~~~ Test instrs set 2 ~~~~~~~");;
(* print_string ("\n****Input instrs:" ^ (string_of_arm_prog test_instrs2) ^ "\n");; *)
print_string ("\nNum of blocks: " ^ (string_of_int (List.length (test_blocks2))));; (* flush std_out;; *)
print_all_block_instrs (test_blocks2);;
print_string ("     *******");;
(* print_len_block_instrs_head (test_blocks2);; *)
(* print_block_instrs (List.hd (test_blocks2));; *)
print_string ("\n\nPeephole: rm redundant load/store (on block1)\n");;
print_string ("\t~~ 1 - LD/STR ~~");;
let peephole1_block = remove_redundant_ldr_str (List.hd test_blocks2);;
print_block_instrs peephole1_block;;
print_string ("\t~~ 2 - ALGEBRAIC ~~");;
let peephole2_block = algebraic_simplification (List.hd test_blocks2);;
print_block_instrs peephole1_block;;



(* Peephole Optimizations *)
(*
let rec peephole_opt (instrs: arm_program) : arm_program = 
  let instrs_len = List.length instrs in 
    match instrs_len with
    | 0 -> []
    | 1 -> instrs
    | 2 ->
      let (instr1, instr2) = (List.nth instrs 0, List.nth instrs 1) in
        if (instr1 == instr2) then [instr1]
        else instrs
    | _ -> [] 
*)




(* Peephole: Unreachable code elimination *)



let optimize_arm (instructions : arm_program) =
  let blks = get_basic_blocks instructions [] None in
  print_all_block_instrs blks


(* EOF *)