
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

(* Print one block's instructions *)
let print_block_instrs blk =
  let blk_id = "\nBlock " ^ (string_of_int blk.id) ^ " instructions:\n" in
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

(* Make all basic blocks *)
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

    let instr_helper i : (block list * block) =
      let line_id = fresh_line () in
      let new_instruction = (line_id, i) in
      match current_blk with
      | Some curr_blk -> 
        let new_instr = (new_instruction, curr_blk.id, curr_blk.label) in
          curr_blk.instrs <- List.append curr_blk.instrs [new_instruction];
          curr_blk.instrs_out <- List.append curr_blk.instrs_out [new_instr];
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
          | Some current_blk  ->           (* Update current block and create new block *)
            let last_instr = last current_blk.instrs in  (* add label instruction to current_blk.instrs_out *)
             new_blk.instrs_in      <- (last_instr, current_blk.id, current_blk.label) :: []; 
             current_blk.instrs_out <- List.append current_blk.instrs_out [(label_instruction, blk_id, label)];
             (List.append blocks [current_blk], new_blk)     (* return list of blocks and new (current) block *)
          | None -> ([], new_blk)           (* No block exists, create new block *) (*thisfunction [new_block] new_blk *)
        end
      | PseudoInstr pseudo ->
        let line_id = fresh_line () in
        let p_instruction = (line_id, i) in
        begin
          match current_blk with
          | Some current_blk -> 
            current_blk.instrs <- List.append current_blk.instrs [p_instruction];
            let pseudo_instr = (p_instruction, current_blk.id, current_blk.label) in
              current_blk.instrs_out <- List.append current_blk.instrs_out [pseudo_instr];
              (blocks, current_blk)
          | None -> failwith "#6: Pseudo instrction block current block doesn't exist." 
              (* only temporary as isntructions can start with a pseudoinstr *)
        end 
      | B (cond, b_label) -> (* branch *)
        let line_id = fresh_line () in
        let b_instruction = (line_id, i) in
        begin
          match current_blk with
          | Some current_blk ->
            current_blk.instrs <- List.append current_blk.instrs [b_instruction];
            let branch_instr = (b_instruction, current_blk.id, current_blk.label) in 
              current_blk.instrs_out <- List.append current_blk.instrs_out [branch_instr];
              if current_blk.label == b_label then 
                begin
                  current_blk.instrs_in <- List.append current_blk.instrs_in [branch_instr];
                  (blocks, current_blk) 
                end
              else 
                let rec helper bs label = (* find block w/ matching label *)
                  match bs, label with 
                  | [], _ -> failwith "#5: Block with label wasn't found"
                  | b::bs, label ->
                    if b.label == label then
                      begin
                        b.instrs_in <- List.append b.instrs_in [branch_instr];
                        List.append [b] bs 
                      end
                    else List.append [b] (helper bs label)
                in (helper blocks b_label, current_blk) 
          | None -> failwith "#4: Branch - current block doesn't exist"
        end
      | BL (cond, b_label) -> (* branch *)
        let line_id = fresh_line () in
        let bl_instruction = (line_id, i) in
        begin
          match current_blk with
          | Some current_blk ->
            current_blk.instrs <- List.append current_blk.instrs [bl_instruction];
            let branch_instr = (bl_instruction, current_blk.id, current_blk.label) in 
              current_blk.instrs_out <- List.append current_blk.instrs_out [branch_instr];
              if current_blk.label == b_label then 
                begin
                  current_blk.instrs_in <- List.append current_blk.instrs_in [branch_instr];
                  (blocks, current_blk) 
                end
              else 
                let rec helper bs label = (* find block w/ matching label *)
                  match bs, label with 
                  | [], _ -> failwith "#5: Block with label wasn't found"
                  | b::bs, label ->
                    if b.label == label then
                      begin
                        b.instrs_in <- List.append b.instrs_in [branch_instr];
                        List.append [b] bs 
                      end
                    else List.append [b] (helper bs label)
                in (helper blocks b_label, current_blk) 
          | None -> failwith "#4: Branch - current block doesn't exist"
        end 
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
  | [], [], Some current_blk -> [current_blk]
  | [], blks, Some current_blk -> List.append blks [current_blk]
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



(* TESTS *)

let test_instrs = [Label "hello"];;
let test_instrs0 = Label "hello" :: ADD ("", false, "a1", "a0", RegOp ("v1")) :: ADD ("", false, "a1", "a0", RegOp ("v1")) :: Label "bye":: [];;
let test_instrs1 = Label "hello" :: ADD ("", false, "a1", "a0", RegOp ("v1")) :: [];;

(* let test_blocks = get_basic_blocks test_instrs [] None;; *)
let test_blocks0 = get_basic_blocks test_instrs0 [] None;;
(* let test_blocks1 = get_basic_blocks test_instrs1 [] None;; *)

print_string ("Test instrs set 0");;
print_string ("\n****Input instrs:" ^ (string_of_arm_prog test_instrs0) ^ "\n");;
print_string ("Num of blocks: " ^ (string_of_int (List.length (test_blocks0))));; (* flush std_out;; *)
print_len_block_instrs_head (test_blocks0);;
print_block_instrs (List.hd (test_blocks0));;

(* 
print_string ("\n\nTest instrs set 1");;
print_string ("\n****Input instrs:" ^ (string_of_arm_prog test_instrs1) ^ "\n");;
print_string ("Num of blocks: " ^ (string_of_int (List.length (test_blocks1))));; (* flush std_out;; *)
print_len_block_instrs_head (test_blocks1);;
print_block_instrs (List.hd (test_blocks1));; *)



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

(* Peephole: remove redundant loads and stores *)
(* input : 1 block *)
(* if only 1 instruction, return *)
(* go through each instruction. if one is load, check to see if the next is a store.
    if the store has the same info, rm the str instruction.
  if the str is the last instruction, update instrs out accordingly *)
let remove_redundant_ldr_str blk =
  let instrs = blk.instrs in
  if (List.length instrs) == 0 then
    blk
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
              match rd1, addr_type1, rd2, addr_type2 with
              | rd1, Reg r1, rd2, Reg r2 ->
                if (rd1 == r2) && (rd2 == r1) then
                  let rest_instrs = helper (List.tl is) in
                  List.append [i] rest_instrs
                else helper is 
              | _ -> helper is
            end
          | _ -> helper is
        end
      | _ -> failwith "#30: len and instrs mismatch"
    in blk.instrs <- helper instrs; blk


(* Peephole: Unreachable code elimination *)


(*
let optimize_arm (instructions : arm_program) : arm_program =
  let opt1 = peephole_opt instructions 
  in opt1 
*)