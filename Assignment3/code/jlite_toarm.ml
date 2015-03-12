
(* ===================================================== *)
(* ============== CS4212 Compiler Design =============== *)
(*   Transformation from intermediatery form ir3 to      *)
(*                   ARM machine code                    *)
(* ===================================================== *)

open Ir3_structs
open Arm_Structs

(* (register, variable) *)
(* (variable, mem loc where var can be found) *)
type memloc =
  | Reg of reg
  | Stack
  | Heap
type reg_desc = reg * id3
(* type addr_desc = id3 * (memloc list) *)
type addr_desc = id3 * (address_type list)


let labelcount = ref 0 
let fresh_label () = (labelcount:=!labelcount+1; !labelcount)

let varcount = ref 0 
let fresh_var () = (varcount:=!varcount+1; (string_of_int !varcount))

(* New stuff. *)

let rec init_reg (n: int) =
  match n with
  | 0 -> [("R0","")]
  | n ->
    let r = List.concat "R" (string_of_int n) in
      List.append [(r, "")] (init_reg n-1)

let rec is_in_reg (var: id3) (regs: reg_desc list) =
  match regs with
  | [] -> (None, "")
  | (r, v)::regs ->
    if (String.compare var v) then (Some r, v)
    else is_in_reg var regs

let rec find_empty_reg (regs: reg_desc list) = 
  match regs with
  | [] -> None
  | (r, v)::regs ->
    if (String.compare v "") then Some r
    else find_empty_reg regs

(* let get_reg_cmp
  (instr: arm_instr)
  (rd: reg_desc list) = 
  if 
  (* get registers *) *)

let rec get_reg
  (var: id3)
  (regs: reg_desc list)
  (ads: addr_desc list)
  : reg =
    (* try is in reg *)
  let (r1, v) = is_in_reg var regs in
  let v3 = find_empty_reg regs in
  match r1, v3 with
    | Some r, _ -> r 
    | _, Some r -> r
    | None, None -> r1 (* no free reg exists *)
(*   let (r, v) = is_in_reg var regs in
    match r with
    | None -> (* try is in reg *)
      let r = find_empty_reg regs in
      begin
        match r with
        | None -> (* no free reg exists *)
        | Some r -> r
      end
    | Some r -> r
 *)
  (* find ad entry for v. if the addr_type list has more than just reg, pick this one *)


    
let idc3_to_arm = "hi"

let binaryExp3_to_arm (op, v1, v2) =
  match op with
  | BooleanOp o -> 
    begin
      match o with
      | "&&" -> AND ("cond", false, "R0", "R1", ImmedOp "R0")
      | "||" -> ORR ("cond", false, "R0", "R1", ImmedOp "R0")
      | _ -> failwith "Error: boolean symbol not supported"
    end
  | ArithmeticOp o -> 
    begin
      match o with
      | "+" -> ADD ("cond", false, "R0", "R1", ImmedOp "R0")
      | "-" -> SUB ("cond", false, "R0", "R1", ImmedOp "R0")
      | "*" -> MUL ("cond", false, "R0", "R1", "v3")
      | _ -> failwith "Error: arithmetic symbol not supported"
    end
  | RelationalOp ->
   begin
      match o with
      | "==" -> CMP ("EQ", "R0", RegOp "R1")
      | "!=" -> CMP ("NE", "R0", RegOp "R1")
      | ">=" -> CMP ("GE", "R0", RegOp "R1")
      | "<=" -> CMP ("LE", "R0", RegOp "R1")
      | "<" -> CMP ("LT", "R0", RegOp "R1")
      | ">" -> CMP ("GT", "R0", RegOp "R1")
      | _ -> failwith "Error: relational symbol not supported"
    end

let ir3_expr_to_arm
  (p: ir3_program)
  (cl: cdata3)
  (exp: ir3_exp)
  : (hi) = 
  match exp with
    | BinaryExp3 be -> binaryExp3_to_arm be
    (* | BinaryExp3 (op, v1, v2) -> *)
    | UnaryExp3 (op, val) -> 
    | FieldAccess3 (v, mtd) ->
    | Idc3Expr v -> 
    | MdCall3 (mtd, vs) ->
    | ObjectCreate3 v -> 

(* let ir3_stmt_to_arm = *)


(* Inititalize program *)
let ir3_program_to_arm (p: ir3_program): arm_program =
  let ir3_classes_to_arm ((cname,cvars): cdata3) =
    (* whatever *)cname in
  let ir3_mtd_to_arm c = 5 in

  PseudoInstr ".data" :: 
  Label "L1" ::
  PseudoInstr ".asciz \"Hellokjljdkjbkbbkb World\"" ::
  PseudoInstr ".text" ::
  PseudoInstr ".global main" ::
  PseudoInstr ".type main, %function" ::
  Label "main" ::
  STMFD ("fp" :: "lr" :: "v1" :: "v2" :: "v3" :: "v4" :: "v5" :: []) ::
  ADD ("", true, "fp", "sp", ImmedOp "#24") ::
  SUB ("", true, "sp", "fp", ImmedOp "#32") :: (* 32 as a placeholder *)
  LDR ("", "", "a1", (Reg "=L1")) ::
  BL ("", "printf(PLT)") ::


  Label "L1exit" ::
  MOV ("", true, "a4", (ImmedOp "#0")) ::
  MOV ("", true, "a1", (RegOp "r3")) ::
  SUB ("", true, "sp", "fp", ImmedOp "#24") ::
  LDMFD ("fp" :: "pc" :: "v1" :: "v2" :: "v3" :: "v4" :: "v5" :: []) :: []
























