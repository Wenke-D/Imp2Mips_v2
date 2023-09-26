open Debug

let encode_boolean b = if b then 1 else 0

(**
   Simple translation from IMP to MIPS.

   Summary of MIPS architecture and assembly language.


   32-bits architecture comprising:
   - an arithmetic and logical unit
   - 32 registers
     * general purpose: $t0-$t9, $s0-$s7, $a0-$a3, $v0-$v1
     * special purpose: $ra, $sp, $fp, $gp, $zero
     (+3 reserved)
   - randomly addressable memory, containing code and data


   A MIPS program contains two parts :
   - instructions, introduced by the directive

       .text

   - data, introduced by the directive

       .data

   The data part contains statically allocated data, for instance the
   global variables of a program. Each piece of data is given a label
   that will be used for accessing the data.



   *** Arithmetic ***

   Arithmetic instructions in "three addresses" format, that apply to 
   values stored in registers, and also put the result in a register.
   Three addresses format:

     add  $r1, $r2, $r3

   means  $r1 <- $r2 + $r3 
   where $r1 is the register where the result is to be stored, and $r2 and
   $r3 are the registers containing the value to be summed.
   
   Many instructions in this format: 
   - arithmetic: add, sub, mul, div, rem, sll, srl
   - logic: and, or
   - comparisons: seq, sne, slt, sle, sgt, sge
   Some unary operations ask for only one operand:
   - not, neg, move

   A few arithmetic instructions take a number as second operand.
   For instance:

     addi  $r1, $r2, i

   for  $r1 <- $r2 + i


   Loading a value in a register:

     li  $r, i

   for  $r <- i


   *** Memory ***

   A memory address is identified by a base address contained in a
   register [$r], and an offset [o] relative to the base address. The
   offset is given directly as a number.
   Notation: o($r).
   Addresses are given in bytes. A 32-bits piece of data occupies 4 bytes.

   Read access

     lw  $r1, o($r2)

   for  $r1 <- *($r2+o)
   which means "$r1 takes the value stored at address $r2+o".

   Write access

     sw  $r1, o($r2)

   for  *($r2+o) <- $r1
   which means "store at address $r2+o the value of register $r1".


   Statically allocated data are, as everything else, stored in memory. 
   Retrieve the address associated to a label [lab] of statically allocated
   data with
   
     la  $r1, lab


   *** Branching instructions ***

   The running program is stored in memory: each instruction has an address.
   Special register [pc] contains the address of the next instruction.

   In most cases, execution of a given instruction is followed by the execution
   of its immediate successor in memory, which corresponds to
     pc <- pc + 4
   Jump or branch instructions drive execution towards other parts of the
   program, identified by one the following means:
   - a label written at some place in the assembly program,
   - an address that has been computed and stored in a register.

   Unconditional jumps with target given by a label 
   (two versions are subtly different, but we will ignore the difference).
     
     j  lab
     b  lab

   Unconditional jump with target address given by a register 

     jr  $r

   Two variants used for function calls, which store the value [pc+4] in the
   special purpose register $ra. This stored address identifies the instruction
   where execution should resume once the call is completed.

     jal   lab
     jalr  $r


   Conditional jumps to a label, depending on the result of a test.
   Example: jumps to the instruction with label [lab] si the values in 
   registers $r1 and $r2 are equal.

     beq  $r1, $r2, lab

   Available binary tests:
   - beq, bne, bgt, bge, blt, ble

   Particular cases with only one operand, equivalant to the previous 
   instructions with $r2=0

     beqz  $r1, lab

   Available tests-to-zero:
   - beqz, bnez, bgtz, bgez, bltz, blez


   *** System calls ***

   For this course we do not use actual MIPS hardware but a simulator.
   This simulator includes a few special operations that mimick some services
   that would otherwise be offered by the operating system, or a low-level
   library like the libc. These operations are triggered by the additional
   instruction
     
     syscall

   which is not part of the actual assembly language.
   Each service has a numeric code, that has to be put in register $v0.
   If an argument is needed, it is put in register $a0.

   Some services:
   - code 1: prints the integer contained in register $a0
   - code 10: halts the program
   - code 11: prints the character whose ASCII code is given by $a0
 *)

open Imp
(**
   Module Imp defines the abstract syntax for the source language.
   Module Mips defines caml functions for generating MIPS instructions.
 *)

open Mips

(* To save the (4-byte) value of a register [reg], decrement $sp by 4 bytes,
   then write the value at the new address pointed by $sp.
   Note: operator @@ concatenates MIPS fragments (defined in Mips module). *)

(**
   The upper part of the memory (the highest addresses) is used as a stack
   for storing various elements. Special purpose register $sp points to the
   top of the stack (more precisely: the address of the element at the top
   of the stack). The stack grows towards decreasing addresses.
   We define two auxiliary functions [push] and [pop] generating MIPS code
   for adding or removing an element at the top of the stack.
 *)
let push reg =
  explain (Printf.sprintf "push %s" reg)
  @@ subi sp sp 4 @@ sw reg 0 sp @@ explain "end_push"

(* Conversely, to retrieve and remove an element from the top of the stack,
   read the value at the address given by $sp, and increment $sp by 4 bytes. *)
let pop reg = lw reg 0 sp @@ addi sp sp 4
(* In both cases, the update of $sp guarantees that the next operation on the
   stack will take into account the fact that the stack grew or shrank. *)

(** Save state of t[0] - t[i] to stack. 
    t[0] will be pushed at last *)
let save_t i =
  if i < 0 then nop
  else
    let rec aux i code =
      match i with
      | 0 -> code @@ push (t 0)
      | i -> aux (i - 1) (code @@ push (t i))
    in
    aux i (explain (Printf.sprintf "save %s ~ %s" (t 0) (t i)))
    @@ explain "end_save"

(** Restore state of t.(0) - t.(i) from stack *)
let restore_t i =
  if i < 0 then nop
  else
    let rec aux j code =
      if j = i then code @@ pop (t i) else aux (j + 1) (code @@ pop (t j))
    in
    aux 0 (explain (Printf.sprintf "__restore %s ~ %s" (t 0) (t i)))
    @@ explain "__end_restore"

(**
   Function producing MIPS code for an IMP function. Function producing MIPS
   code for expressions and instructions will be defined inside.
 *)
let tr_function fdef =
  (*
     Initialization of a table for accessing local variables and function
     parameters. These data are laid around a base address given by the
     special purpose register $fp. This information is called the
     activation frame for the call. It is pictured below.
     
       +------+
       |  aN  |   <- last argument, address  $fp + 4N
       +------+
       |      |
       |  ..  |
       |      |
       +------+
       |  a2  |   <- second argument, address $fp + 8
       +------+
       |  a1  |   <- first argument, address $fp + 4
       +------+
       |      |   <- base address given by $fp
       +------+
       |      |
       +------+
       |  x1  |   <- first local variable, address $fp - 8
       +------+
       |  x2  |   <- second local variable, address $fp - 12
       +------+
       |      |
       |  ..  |
       |      |
       +------+
       |  xK  |   <- last local variable, address $fp - 4(K+1)
       +------+

     The table defined below associates each local variable or parameter
     name the offset at which it can be found relative to the base
     address $fp.
   *)
  let env = Hashtbl.create 16 in
  List.iteri (fun k id -> Hashtbl.add env id (4 * (k + 1))) fdef.params;
  List.iteri (fun k id -> Hashtbl.add env id (-4 * (k + 2))) fdef.locals;

  (*
     In the activation frame, addresses $fp and $fp-4 are used for two
     special information that the source program cannot access.
     - At the base address $fp, we store the base address of the activation
       frame of the caller. This implicitly creates a linked list of the
       activation frames of all the currently active calls, that starts
       with the most recent.
     - At address $fp-4, we store the return address of the call, that is
       the address in the code of the caller at which the execution will 
       resume after the call. This is the address that is put in $ra when
       the call is performed. It is stored in the activation frame so as
       not to be destroyed by the next call.
     - So that we can use $fp and $ra to store stack bottom address and
       stack top address for current function;
   *)

  (*
     Function that generate MIPS code for an IMP expression.
     The function takes a parameter an expression [e] and produces a
     sequence of MIPS instructions that evaluates [e] and put the
     obtained value in register $t.(0).
   *)
  let tr_expr e =
    let rec aux i e =
      (* Evaluate a expression and put the result at register $ti,
          i is an argument.
             If i is bigger than 7, abort.
         Return instruction list of the evaluation and number of register consumed.
      *)
      let ti = Mips.t i in
      match e with
      (* Case of a constant: load the value in the target register $t0. *)
      | Cst n -> (li ti n, 1)
      (* Boolean [true] is coded by 1 and boolean [false] by 0. *)
      | Bool b -> (li ti (encode_boolean b), 1)
      (* Case of a variable. Look up the identifier in the local environement
         [env] to know the offset, then read at the obtained offset from the
         base address $fp. *)
      | Var id -> (
          match Hashtbl.find_opt env id with
          | Some offset -> (lw ti offset fp, 1)
          (* In case the identifier does not appear in [env], we assume we are
             accessing a global variable. We use the identifier as a label and
             read at the corresponding address.
             $ti = &id;
             $ti = *(0+ti)
          *)
          | None -> (la ti id @@ lw ti 0 ti, 1))
      (* Binary operation: use the stack to store intermediate values waiting
         to be used. *)
      | Binop (bop, e1, e2) ->
          let op = match bop with Add -> add | Mul -> mul | Lt -> slt in
          (* Register Index for e2 *)
          let ri_e2 = i in
          (* Instruction List for e2, Consumed Register Number by e2 *)
          let il_e2, crn_e2 =
            (* Evaluate [e2] *)
            aux ri_e2 e2
          in
          (* Register Index for e1 *)
          let ri_e1 = ri_e2 + crn_e2 in
          (* Instruction List for e1, register count for e1 *)
          let il_e1, crn_e1 =
            (* Evaluate [e1] *)
            aux ri_e1 e1
          in
          (* Apply the binary operation
             1. concat all evaluation instructions for e1 & e2;
             2. apply bop, save value at $ti;
             3. this consumes the sum of e1 and e2 consumed registers numbers.
          *)
          (il_e2 @@ il_e1 @@ op ti (t ri_e2) (t ri_e1), crn_e2 + crn_e1)
      (* Function call.
         Before jumping to the function itself, evaluate all parameters and put
         their values on the stack, from last to first. *)
      | Call (f, params) ->
          (* Evaluate the arguments and pass them on the stack. *)
          let all_code, rcparams =
            List.fold_right
              (fun (* [code] for previous arguments
                       $t[i] register for this argument evaluation result *)
                     arg (previous_code, consumed_sum) ->
                (* Instruction List & Register Count for current argument *)
                (* index can begin from 0, because all $t will be saved before evaluate arguments *)
                let ri = consumed_sum in
                let il_arg, rc_arg = aux ri arg in
                ( (* codes for previous arguments *)
                  previous_code
                  (* code for this arg *)
                  @@ il_arg
                  (* push result to stack, result at $ti *)
                  @@ push (t ri),
                  (* all register count comsumed *)
                  consumed_sum + rc_arg ))
              params (nop, 0)
          in
          (* save previous $ti *)
          ( save_t (i - 1)
            (* codes for evaluate all arguments and put them on the stack  *)
            @@ all_code
            (* Jump to the funtion for execution,
               save current address to $ra for return *)
            @@ jal f
            (* After return, drop the stack for arguments  *)
            @@ addi sp sp (4 * List.length params)
            (* move return value to current $ti *)
            @@ move (t 0) ti
            (* restore previous $ti *)
            @@ restore_t (i - 1),
            rcparams )
    in
    let code, count = aux 0 e in
    code
  in
  (*
     Auxiliary function for producing unique labels, for use in the
     translation of control structures (if and while).
   *)
  let new_label =
    let cpt = ref (-1) in
    fun () ->
      incr cpt;
      Printf.sprintf "__%s_%i" fdef.name !cpt
  in

  (*
     Functions that generate MIPS code for an instruction or a sequence.
     Registers for evaluation is the same for each instruction.
   *)
  let rec tr_seq seq =
    match seq with
    | [] -> nop
    | [ i ] -> tr_instr i
    (* If an IMP sequence contains several instructions, concatenate the
       MIPS sequences for each in order. *)
    | i :: s -> tr_instr i @@ tr_seq s
  and tr_instr instr =
    match instr with
    (* Prints a char. *)
    | Putchar e ->
        let code =
          (* Evaluate expression [e] to $ti *)
          tr_expr e
        in
        code
        (* Move the value of [e] from $ti (where it has been produced)
           to $a0 (where syscall expects it). *)
        @@ move a0 (t 0)
        (* Syscall number 11: printing an ASCII character. *)
        @@ li v0 11
        @@ syscall
    (* Assignment.
       After evaluation of [e], its value is in $ti.
       Chose the right instruction to update memory depending on the
       local or global nature of the variable [id]. *)
    | Set (id, e) ->
        let save_code =
          match Hashtbl.find_opt env id with
          (* Local variable, save value in t0 into memory,
             address of memeory local variable is calculated by offset to frame stack *)
          | Some offset -> sw (t 0) offset fp
          (* Global variable,
             1. get address
              t1 = &id;
             2. save value
              *(t1+0) = t0
          *)
          | None -> la t1 id @@ sw (t 0) 0 t1
        in
        let eval_code = tr_expr e in
        eval_code @@ save_code
    (* Conditional *)
    | If (c, s1, s2) ->
        (* Create two labels that will serve as targets for jumps. *)
        let then_label = new_label () and end_label = new_label () in
        (* Evaluate the condition [c] *)
        let cond_code = tr_expr c in
        cond_code
        (* If we got a non-zero value, which is interpreted as [true], jump
           to the code fragment of the "then" branch... *)
        @@ bnez (t 0) then_label
        (* ... otherwise just fall to the next instruction.
           Hence, we put the code of the "else" branch just here. *)
        @@ tr_seq s2
        (* At the end of the "else" branch, jump to the instruction that follows
           the conditional. *)
        @@ b end_label
        (* Code for the "then" branch. *)
        @@ label then_label
        @@ tr_seq s1
        (* At the end of the "then" branch, there is no need to jump, since we
           are precisely at the end of the conditional. Just put here the
           final label, without any explicitly associated instruction (it will
           be associated to the instruction that immadiately follows the
           conditional). *)
        @@ label end_label
    (* Loop *)
    | While (c, s) ->
        let test_code = tr_expr c in
        (* Create two labels for jumps. *)
        let test_label = new_label () and code_label = new_label () in
        (* First instruction: jump to the code that evaluates the condition. *)
        b test_label
        (* Code for the loop body, introduced by its label. *)
        @@ label code_label
        @@ tr_seq s
        (* At the end of a pass through the loop, just fall to the evaluation of
           the condition, that determines whether the loop is executed again. *)
        @@ label test_label
        @@ test_code
        (* If the condition is non-zero, jumps back to the beginning of the loop
           body. *)
        @@ bnez (t 0) code_label
    (* Otherwise, fall to the next instruction, which in this case is the
       instruction that immediately follows the loop. *)
    (* Note: without the instruction [b test_label] at the beginning, we get
       the behaviour of a do-while loop instead of while. *)

    (* Function termination. *)
    | Return e ->
        let eval_code =
          (* Evaluate the value to be returned, in $t.(0) *)
          tr_expr e
        in
        eval_code
        (* Deallocate the part of the stack used for local variables.
           move sp to fp -4 to get saved $ra and $fp *)
        @@ addi sp fp (-4)
        (* Retrieve the return address *)
        @@ pop ra
        (* Restore the base pointer of the caller *)
        @@ pop fp
        (* Jumps back to the caller *)
        @@ jr ra
    (* Expression used as an instruction.
       Note that the produced MIPS code writes a value in $t0, but
       this value will not be used. *)
    | Expr e ->
        let code = tr_expr e in
        code
  in

  (*
     MIPS code produced for the function.
     Reminder: when this code is executed, the parameters of the call have
     already been evaluated and pushed on the stack. Half of the activation
     frame is already built, and we now build the second hald.
   *)
  (* Save the base address of the activation frame of the caller *)
  push fp
  (* Save return address of caller *)
  @@ push ra
  (* Definition of the base pointer of the new activation frame. *)
  @@ addi fp sp 4
  (* Allocate space on the stack for local variables, by just shifting the
     pointer that indicates where the top is. *)
  @@ addi sp sp (-4 * List.length fdef.locals)
  (* After this preamble, we can execute the actual code of the function. *)
  @@ tr_seq fdef.code
  (* If execution reaches the end of this sequence, it means that no [return]
     instruction has been met. We add a MIPS fragment equivalent to [return 0;] *)
  @@ li t0 0
  @@ addi sp fp (-4) @@ pop ra @@ pop fp @@ jr ra

(**
   Main function for translating a program.
 *)
let translate_program prog =
  (* MIPS fragment that will be placed at the beginning of the produced
     assembly code, for retrieving one optional integer command-line
     argument, then jumping to the "main" function. *)
  let init =
    (* At the beginning, $a0 contains the number of command-line arguments.
       if $a0 == 0; jump to label "init_end"
    *)
    beqz a0 "init_end"
    (* Otherwise, $a1 contains the address of an array of character strings
       containing the command-line arguments.
       assume argv.length = 1;
       assume $a1 = argv;
       1. $a0 = a1;
       2. $v0 = atoi($a0)
    *)
    @@ lw a0 0 a1
    @@ jal "atoi" @@ label "init_end"
    (* At the end, the obtained integer is in $v0. Push it on the stack to
       pass it as an argument to "main". *)
    @@ push v0
    @@ jal "main"
    (* After execution of the "main" function,
       call syscall 10 for terminate the program *)
    @@ li v0 10
    @@ syscall
  and built_ins =
    (* Conversion function string -> int, that iterates on the characters of
       the string.
       $v0 = atoi($a0)
        [$a0]: address of the string *)
    comment "built-in atoi"
    (* Label of the funtion *)
    @@ label "atoi"
    (* result value init as 0 *)
    @@ li v0 0
    (* Label for loop *)
    @@ label "atoi_loop"
    (* $t0 = $a0[] 1 byte of the string*)
    @@ lbu t0 0 a0
    (* if the string is zero break *)
    @@ beqz t0 "atoi_end"
    (* string ascii -= 48 *)
    @@ addi t0 t0 (-48)
    (* if $t0 < 48, invalid string *)
    @@ bltz t0 "atoi_error"
    (* if $t0 >= 10, invalid string *)
    @@ bgei t0 10 "atoi_error"
    (* $v0 = $v0 * 10 *)
    @@ muli v0 v0 10
    (* $v0 = v0 + $t0 *)
    @@ add v0 v0 t0
    (* $a0 += 1 byte, move to next byte (char) *)
    @@ addi a0 a0 1
    (* back to loop *)
    @@ b "atoi_loop"
    (* function error, exit the program by sys 10 *)
    @@ label "atoi_error"
    @@ li v0 10 @@ syscall
    (* function end *)
    @@ label "atoi_end"
    @@ jr ra
  in

  (*
     Main code for producing the MIPS assembly program corresponding to the
     source IMP program.
   *)
  let function_codes =
    List.fold_right
      (fun fdef code -> label fdef.name @@ tr_function fdef @@ code)
      prog.functions nop
  in

  (* Combine the initialization code seen above, we the code produced for each
     function of the source program. *)
  let text = init @@ function_codes @@ built_ins
  (* In the "data" part, introduce a label for each global variable,
     variable name is the label name.
     All global variable has 0 with a word width as initial value.
     For example:
     "" var global1; ""
     becomes
     "" global1:
          .word 0;  "" *)
  and data =
    List.fold_right
      (fun id code -> label id @@ dword [ 0 ] @@ code)
      prog.globals nop
  in

  { text; data }
