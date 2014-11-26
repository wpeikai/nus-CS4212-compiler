
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
    mutable instrs: block_instr list;
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

let print_block_instrs blk =
  let rec helper blk_i =
    match blk_i with
    | [] -> []
    | (line_id, suffix, i)::is -> i :: (helper is)
  in print_string ("\n* Current block instr" ^ (string_of_arm_prog (helper (blk.instrs))) ^ "\n\n");;

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
      let new_instruction = (line_id, false, i) in
      match current_blk with
      | Some curr_blk -> 
        let new_instr = (new_instruction, curr_blk.id, curr_blk.label) in
          curr_blk.instrs <- List.append curr_blk.instrs [new_instruction];
          curr_blk.instrs_out <- List.append curr_blk.instrs_out [new_instr];
          (* print_block_instrs curr_blk; *)
          (blocks, curr_blk)
        | None -> failwith "#11: instrction block current block doesn't exist." 
    in
    begin
      match i with
      | Label label -> (*create new block*)
        let blk_id = fresh_blk () in
        let line_id = fresh_line () in
        (* update current block *)
        let label_instruction = (line_id, false, i) in
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
        let p_instruction = (line_id, false, i) in
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
        let b_instruction = (line_id, false, i) in
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
        let bl_instruction = (line_id, false, i) in
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
(*      | LDMFD reg_list ->
        let line_id = fresh_line () in
        let ldmfd_instruction = (line_id, false, i) in
        begin
          match current_blk with
          | Some current_blk -> 
            current_blk.instrs <- List.append current_blk.instrs [ldmfd_instruction];
            let ldmfd_instr = (ldmfd_instruction, current_blk.id, current_blk.label) in
              current_blk.instrs_out <- List.append current_blk.instrs_out [ldmfd_instr];
              (blocks, current_blk)
          | None -> failwith "#17: ldmfd instrction block current block doesn't exist." 
        end
      | STMFD reg_list ->
        let line_id = fresh_line () in
        let stmfd_instruction = (line_id, false, i) in
        begin
          match current_blk with
          | Some current_blk -> 
            current_blk.instrs <- List.append current_blk.instrs [stmfd_instruction];
            let stmfd_instr = (stmfd_instruction, current_blk.id, current_blk.label) in
              current_blk.instrs_out <- List.append current_blk.instrs_out [stmfd_instr];
              (blocks, current_blk)
          | None -> failwith "#17: ldmfd instrction block current block doesn't exist." 
        end
      | LDR (cond, wordtype, rd, address_type) -> 
        let line_id = fresh_line () in
        let ldr_instruction = (line_id, false, i) in
        begin
          match current_blk with
          | Some current_blk ->
            current_blk.instrs <- List.append current_blk.instrs [ldr_instruction];
            let ldr_instr = (ldr_instruction, current_blk.id, current_blk.label) in
              current_blk.instrs_out <- List.append current_blk.instrs_out [ldr_instr];
              (blocks, current_blk)
          | None -> failwith "#15: LDR instrction block current block doesn't exist." 
        end
      | STR (cond, wordtype, rd, address_type) -> 
        let line_id = fresh_line () in
        let str_instruction = (line_id, false, i) in
        begin
          match current_blk with
          | Some current_blk ->
            current_blk.instrs <- List.append current_blk.instrs [str_instruction];
            let str_instr = (str_instruction, current_blk.id, current_blk.label) in
              current_blk.instrs_out <- List.append current_blk.instrs_out [str_instr];
              (blocks, current_blk)
          | None -> failwith "#16: STR instrction block current block doesn't exist." 
        end
      | MOV (cond, suffix, rd, operand_type) -> (* not fully implemented *)
        let line_id = fresh_line () in
        let mov_instruction = (line_id, false, i) in
        begin
          match current_blk with
          | Some current_blk -> 
            current_blk.instrs <- List.append current_blk.instrs [mov_instruction];
            let mov_instr = (mov_instruction, current_blk.id, current_blk.label) in
              current_blk.instrs_out <- List.append current_blk.instrs_out [mov_instr];
              (blocks, current_blk)
          | None -> failwith "#14: MOV instrction block current block doesn't exist." 
        end
      | CMP (cond, rd, operand_type) -> (* not fully implemented *)
        let line_id = fresh_line () in
        let cmp_instruction = (line_id, false, i) in
        begin
          match current_blk with
          | Some current_blk -> 
            current_blk.instrs <- List.append current_blk.instrs [cmp_instruction];
            let cmp_instr = (cmp_instruction, current_blk.id, current_blk.label) in
              current_blk.instrs_out <- List.append current_blk.instrs_out [cmp_instr];
              (blocks, current_blk)
          | None -> failwith "#13: CMP instrction block current block doesn't exist." 
        end
       *)
      (* | ADD (cond, suffix, rd, rn, operand_type) -> *)
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
  (* | [], [], None -> failwith "#22: no instrs were given thus no blocks" *)
  (* | [], blks, None -> failwith "#25: there is no current_blk" *)
  | [], [], Some current_blk ->
      (* print_string("\npass #last w/ only curr blk // ret last\n"); *)
      (* print_block_instrs current_blk; *)
      [current_blk]
  | [], blks, Some current_blk -> 
      print_string("\npass #last\n");
      (* print_block_instrs current_blk; *)
      List.append blks [current_blk]
  | i::is, [], None -> 
    (* print_string("\npass #1 - [], None\n"); *)
    let (blks, new_current_blk) = block_helper i [] None in
    let next_blks = get_basic_blocks is [] (Some new_current_blk) in
    (* print_string("\npass #1 - [], None\n"); *)
      (* print_string ("ret #1"); *)
      List.append blks next_blks
  | i::is, [], Some current_blk -> 
    (* print_string("\npass #2 - [], Some __ \n"); *)
    let (blks, new_current_blk) = block_helper i [] (Some current_blk) in
    let next_blks = get_basic_blocks is [] (Some new_current_blk) in
    (* print_string("\npass #2 - [], Some __ \n"); *)
      (* print_string ("ret #2"); *)
      List.append blks next_blks
  | i::is, blks, Some current_blk ->
    (* print_string("\npass #3 -- blks, Some ___\n"); *)
    let (blks, new_current_blk) = block_helper i blks (Some current_blk) in
    let next_blks = get_basic_blocks is blks (Some new_current_blk) in
    (* print_string("\npass #3 -- blks, Some ___\n"); *)
      (* print_string ("ret #3"); *)
      List.append blks next_blks
        (* List.append blks (get_basic_blocks is blks (Some current_blk)) *)
  | _ -> failwith "#23: get basic blocks error"


let get_block_instrs_len blk = List.length (blk.instrs)

let print_len_block_instrs_head blks =
  let len = get_block_instrs_len (List.hd blks) in
  print_string ("\nhead instrs len:" ^ string_of_int len ^ "\n");;

let print_block_instrs_head blks =
  let blk = List.hd blks in
  let rec helper blk_i =
    match blk_i with
    | [] -> []
    | (line_id, suffix, i)::is -> i :: (helper is)
  in print_string ("**head block insts \n"^ (string_of_arm_prog (helper (blk.instrs))) ^ "\n\n");;

let test_instrs = [Label "hello"];;
(* let test_instrs0 = Label "hello" :: Label "bye" :: [];; *)
let test_instrs0 = Label "hello" :: ADD ("", false, "a1", "a0", RegOp ("v1")) :: ADD ("", false, "a1", "a0", RegOp ("v1")) :: Label "bye":: [];;
let test_instrs1 = Label "hello" :: ADD ("", false, "a1", "a0", RegOp ("v1")) :: [];;

(* let test_blocks = get_basic_blocks test_instrs [] None;; *)
let test_blocks0 = get_basic_blocks test_instrs0 [] None;;
let test_blocks1 = get_basic_blocks test_instrs1 [] None;;

(* print_string (string_of_arm_prog [Label "hello label"; Label "hello label"]); *)

print_string ("Test instrs 0");;
print_string ("\n****input instrs:" ^ (string_of_arm_prog test_instrs0) ^ "\n");;
print_string ("number of blocks: " ^ (string_of_int (List.length (test_blocks0))));; (* flush std_out;; *)
print_len_block_instrs_head (test_blocks0);;
print_block_instrs (List.hd (test_blocks0));;


(* print_string ("Test instrs 1");;
print_string ("\n****input instrs:" ^ (string_of_arm_prog test_instrs1) ^ "\n");;
print_string ("number of blocks: " ^ (string_of_int (List.length (test_blocks1))));; (* flush std_out;; *)
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
(*
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
*)

(* Peephole: Unreachable code elimination *)


(*
let optimize_arm (instructions : arm_program) : arm_program =
  let opt1 = peephole_opt instructions 
  in opt1 
*)