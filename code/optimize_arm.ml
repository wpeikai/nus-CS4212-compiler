
open Jlite_structs
open Ir3_structs
open Arm_structs

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
    mutable instrs_in: (block_instr * int * label) list;
    mutable instrs_out: (block_instr * int * label) list
  }

(* -------- Optimization of ARM program --------- *)

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



let rec get_basic_blocks instrs (blocks: block list) : block list = []

(* returns updated list of blocks *)
let thisfunction 
  (instrs: arm_program)
  (blocks: block list)
  (current_blk: block option)
  : (block list * block) = 
  match instrs with 
  | [] -> failwith "#1: instruction not implemented in block generation yet"
  | i::is ->
    match i with
    | Label label -> (*create new block*)
      let blk_id = fresh_blk () in
      let line_id = fresh_line () in
      (* update current block *)
      let label_instruction = (line_id, false, i) in
      let new_blk = {
        id = blk_id;
        label = label;
        blk_instrs = [label_instruction];
        instrs_in = []; 
        instrs_out = []
      } in
      begin
        match current_blk with
        | Some current_blk  ->           (* Update current block and create new block *)
          let last_instr = last current_blk.blk_instrs in  (* add label instruction to current_blk.instrs_out *)
           new_blk.instrs_in      <- (last_instr, current_blk.id, current_blk.label) :: []; 
           current_blk.instrs_out <- List.append current_blk.instrs_out [(label_instruction, blk_id, label)];

           (List.append blocks [current_blk], new_blk)     (* return list of blocks and new (current) block *)

        | None -> ([], new_blk)           (* No block exists, create new block *) (*thisfunction [new_block] new_blk *)
      end

    | B (cond, b_label) -> (* add *)
      let line_id = fresh_line () in
      let b_instruction = (line_id, false, i) in
        begin
          match current_blk with
          | Some current_blk ->

            current_blk.blk_instrs <- List.append current_blk.blk_instrs [b_instruction];
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
(*      current.block = []*) 

(*      current_blk.blk_instrs 
        blocks :: [{ id = id; label = label; blk_instrs = []; instrs_in = []; instrs_out = [] }]
    | BL (cond, label) *)
    | _ -> failwith "#1: instruction not implemented in block generation"
  | _ -> failwith "#3: wow"


(*
(* Peephole Optimizations *)
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

(* Peephole: remove redundant loads and stores *)
let remove_redundant_load_store instr1 instr2 =
  match instr1, instr2 with
  | LDR load_instr, STR store_instr ->
    begin
      let (reg_load, addr_type) = load_instr  in
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

*)








































