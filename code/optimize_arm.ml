
open Jlite_structs
open Ir3_structs
open Arm_structs

(* Optimization of ARM program *)

let linecount = ref 0
let fresh_line () = (linecount:=!linecount+1; (string_of_int !linecount))

let blkcount = ref 0
let fresh_blk () = (blkcount:=!blkcount+1; (string_of_int !blkcount))

let rec last ls =
  match ls with
  | [] -> []
  | l::[] -> l
  | l::ls -> last ls


(* get the basic blocks from the ARM instructions *)
(* (id, leader flag, instruction ) *)
type block_instr = int * bool * arm_instr 

(* returns in form:  *) 
(* Block: block id, label associated, instrs inside block, instrs leading out *)
type block = 
  {
    id: int;
    label: label;
    mutable blk_instrs: block_instr list;
    mutable instrs_in: (block_instr * label) list;
    mutable instrs_out: (block_instr * label) list
  }


let rec get_basic_blocks instrs (blocks: block list) : block list = []

(* returns updated list of blocks *)
let rec thisfunction 
  instrs 
  blocks
  current_blk = 
  match instrs with 
  | i::is ->
    match i with
    | Label label -> (*create new block*)
      let block_id = fresh_blk () in
      let line_id = fresh_line () in
      (* update current block *)
      begin
        let label_instr = (line_id, false, i) in
        let new_blk = Some {
          id = block_id;
          label = label;
          blk_instrs = [label_instr];
          instrs_in = []; 
          instrs_out = []
        } in
        match current_blk with
        (* No block exists, create new block *)
        | None -> [ new_blk ]
          (*thisfunction [new_block] new_blk *)
          (* or return the following: *)
          (* [new_blk] or new_blk *)

        (* Update current block and create new block *)
        | Some current_blk  ->
          (* add label instruction to current_blk.instrs_out *)
          let label_instr = (line_id, false, i) in
          let last_current_blk_instr = last current_blk.blk_instrs in 
          current_blk.instrs_out <- List.append current_blk.instrs_out label_instr;
          (* return list of blocks *)
          blocks :: [{ id = block_id; label = label; blk_instrs = [label_instr]; instrs_in = []; instrs_out = [] }]
          (* procede to make that blk - return blocks *)
      end



    | B (cond, label) -> 
      current_blk.blk_instrs
        blocks :: [{ id = id; label = label; blk_instrs = []; instrs_in = []; instrs_out = [] }]
    | BL (cond, label)
    | _ -> failwith "#1: instruction not implemented in block generation yet"
  | _ -> "wow"



(* Peephole Optimizations *)
let rec peephole_opt (instrs: arm_program) : arm_program = 
  let instrs_len = List.length instrs in 
    match instrs with
    | 0 -> []
    | 1 -> instrs
    | 2 ->
      let (instr1, instr2) = (List.nth instrs 0, List.nth instrs 1) in
        if (instr1 == instr2) then [instr1]
        else instrs
    | _ -> []

(* Peephole: remove redundant loads and stores *)
let remove_redundant_load_store instr1 instr2 =
  match instr1, instr2 with
  | LDR load_instr, STR store_instr ->
    begin
      let (reg_load, addr_type) = x,x  in
      match load_instr, store_instr with
      | (_,_, reg_ld, addr_type_ld), (_,_, reg_str, addr_type_str) ->
        if (reg_ld == reg_str) && (addr_type_ld == addr_type_str) then
          [instr1]
        else instr1 :: instr2 :: []
    end
  | _ -> instr1 :: instr2 :: []

(* Peephole: Unreachable code elimination *)




let optimize_arm (instructions : arm_program) : arm_program =
  let opt1 = peephole_opt instructions 
  in opt1 









































