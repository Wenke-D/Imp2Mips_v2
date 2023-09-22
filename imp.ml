(**
   Abstract syntax for the IMP language.
   Definition of types and data structures to represent an IMP program
   as caml data.
 *)

(**
   Binary operators: +, *, <
 *)
type binop = Add | Mul | Lt

(**
   Data structure for expressions
 *)
type expression =
  (* Integer constant: 0, -1, 42, ... *)
  | Cst   of int
  (* Boolean constant: true, false *)
  | Bool  of bool
  (* Variable, identified by a name *)
  | Var   of string
  (* Binary operation, with an operator and two operands *)
  | Binop of binop * expression * expression
  (* Function call, with a function name and a list of parameters *)
  | Call  of string * expression list
(**
   An expression:
     (1 + x) * f(3, true)
 *)
let e = Binop(Mul,
              Binop(Add, Cst 1, Var "x"),
              Call("f", [Cst 3; Bool true]))

(**
   Data structure for instructions
*)
type instruction =
  (* Primitive operation for printing a char, given as ASCII code *)
  | Putchar of expression
  (* Assignment of a new value to a variable *)
  | Set     of string * expression
  (* Conditional *)
  | If      of expression * sequence * sequence
  (* Loop *)
  | While   of expression * sequence
  (* Function termination *)
  | Return  of expression
  (* Expression used as an instruction (typically function call) *)
  | Expr    of expression
(* Instruction sequence *)
and sequence = instruction list
(**
   An instruction:
     while (c < 58) {
       putchar(c);
       c = c+1;
     }
 *)
let i = While(Binop(Lt, Var "c", Cst 58),
              [ Putchar(Var "c");
                Set("c", Binop(Add, Var "x", Cst 1)) ]
          )

(** 
   Data structure for a function definition
 *)
type function_def = {
    (* Function name *)
    name: string;
    (* List of named parameters *)
    params: string list;
    (* List of named local variables *)
    locals: string list;
    (* The actual code *)
    code: sequence;
  }
(**
   A function:
     function digits(start) {
       var c;
       c = start + 48;
       while (c < 58) {
         putchar(c);
         c = c+1;
       }
     }
 *)
let f = {
    name = "digits";
    params = ["start"];
    locals = ["c"];
    code = [ Set("c", Binop(Add, Var "start", Cst 48)); i ]
  }

(**
   Data structure for a program
 *)
type program = {
    (* List of named global variables *)
    globals: string list;
    (* The functions defined by the program *)
    functions: function_def list;
  }
(**
   A programme:
     var zero;

     function main() {
       zero = 0;
       digit(zero);
     }

     function digits(start) {
       var c;
       c = start + 48;
       while (c < 58) {
         putchar(c);
         c = c+1;
       }
     }
 *)
let p = {
    globals = ["zero"];
    functions = [ f;
                  { name = "main";
                    params = [];
                    locals = [];
                    code = [ Set("zero", Cst 0);
                             Expr(Call("digits", [Var "zero"])) ] } ]
  }


(**
   An interpreter for IMP.
   The interpreter is given by functions that take as input abstract
   syntax fragments of IMP programs and compute the corresponding result
   or produce the intended effect.
 *)

(**
   Representation of values: integers, booleans, or [Undef] for an
   uninitialized value.
 *)
type value =
  | VInt of int
  | VBool of bool
  | Undef


(** 
   We will use an exception to terminate the interpretation of a function.
 *)
exception EReturn of value

(**
   We now define interpretation functions.

   Interpretation of a given instruction or expression is always
   relative to an environment that gives values to the variables.
   We use two hash tables:

     - a table [global_env] for global variables, used for the whole
       interpretation of the program

     - a table [local_env] for local variables and parameters of the
       current function call, that is created at the beginning of a
       call and discarded at its end.

   Functions [eval_expr] and [exec_instr] that interpret expressions and
   instructions depend on the global table and the local table. 
   The function [exec_call] that interprets a function call depens on
   the global table. To avoid explicitely and repeatedly passing the tables 
   as arguments, we nest the definitions of these functions.
 *)
                   
(**
   Main interpretation function, that takes as parameter the representation
   of a program and an integer argument. Other functions definitions will be
   nested inside this one.
 *)
let exec_prog prog arg =
  (* Global environment: hash table for global variables, initialized with [Undef]. *)
  let global_env = Hashtbl.create 16 in
  List.iter (fun id -> Hashtbl.add global_env id Undef) prog.globals;

  (**
     Function interpreting a function call. Takes as parameters the name of
     the function and a list of parameters. Since it is nested, it can access
     [global_env] and [prog].
   *)
  let rec exec_call f args =
    (* Find a function definition matching the called name *)
    let fdef = List.find (fun fdef -> fdef.name = f) prog.functions in
    (* Local environment for the call: hash table that contains
       - functions parameters (names and given values),
       - local variables, initialized with [Undef].
       A local variable with the same name as a formal parameter shadows
       the parameter. *)
    let local_env = Hashtbl.create 16 in
    List.iter2 (fun id arg -> Hashtbl.add local_env id arg) fdef.params args;
    List.iter (fun id -> Hashtbl.add local_env id Undef) fdef.locals;

    (** 
       Function interpreting an expression. Takes as parameter an expression
       and returns a value (represented in caml by the type [value]).
       This function can access [global_env] and [local_env]. It can also
       call [exec_call], which is recursive.
       The main function [eval_expr: expr -> value] is associated with two 
       auxiliary functions [eval_int: expr -> int] and [eval_bool: expr -> bool]
       which assume that the value is of a given type and extract its contents.
     *)
    let rec eval_int e = match eval_expr e with
      | VInt n -> n
      | _ -> failwith "not an int"
    and eval_bool e = match eval_expr e with
      | VBool b -> b
      | _ -> failwith "not a boolean"
    and eval_expr = function
      (* Integer and boolean constants: return the value, labeled with [VInt]
         of [VBool] according to its nature. *)
      | Cst n -> VInt n
      | Bool b -> VBool b
      (* Variable: first look up the local environment. If the variable name
         is not found there, then look up the global environment.
         Note: the call [Hashtbl.find global_env id] will fail if the variable
         name is not present in the global environment. *)
      | Var id -> begin
          match Hashtbl.find_opt local_env id with
          | Some v -> v
          | None -> Hashtbl.find global_env id
        end
      (* Binary operations: on évalue les deux opérandes [e1] et [e2],
         puis on combine leurs résultats avec une fonction correspondant
         à l'opérateur. *)
      | Binop(op, e1, e2) ->
         (* Note: the only binary operators we defined apply to integers.
            We assume here that the values obtained when evaluating [e1]
            and [e2] are indeed of the kind [VInt], which can be seen by
            the use of [eval_int]. If one of these expressions produce a
            value of the wrong kind ([VBool] of [Undef]), then the call to
            [eval_int] fails. *)
         let n1 = eval_int e1 in
         let n2 = eval_int e2 in
         begin match op with
           | Add -> VInt (n1 + n2)
           | Mul -> VInt (n1 * n2)
           | Lt  -> VBool (n1 < n2)
         end
      (* Function call: evaluate the arguments and trigger a new call to
         [exec_call]. *)
      | Call(f, args) ->
         exec_call f (List.map eval_expr args)
    in

    (**
       Functions interpreting instructions and sequences of instructions.
       These functions can access [global_env] and [local_env].
       They can also call the other local functions [eval_*].
     *)
    let rec exec_seq s =
      (* Interpretation of a sequence: interpret each instruction in
         order. Note that the interpretation of the sequence is interrupted
         in case an exception is raised, which will happen if a [Return]
         instruction is met. *)
      List.iter exec_instr s

    and exec_instr = function
      (* Prints a char. The expression is assumed to be of int type, hence
         the use of [eval_int]. *)
      | Putchar e -> print_char (char_of_int (eval_int e))
      (* Assignment: evaluates [e], then update the environment.
         As before, first look up the local environment, then the global
         environment if needed. *)
      | Set(id, e) ->
         let v = eval_expr e in
         if Hashtbl.mem local_env id then
           Hashtbl.replace local_env id v
         else
           Hashtbl.replace global_env id v
      (* Conditional, loop. 
         The condition [e] is expected to provide a boolean value. *)
      | If(e, s1, s2) ->
         if eval_bool e then
           exec_seq s1
         else
           exec_seq s2
      | While(e, s) as i ->
         if eval_bool e then
           (exec_seq s; exec_instr i)
      (* Call termination. Raise an exception that interrupts the 
         interpretation of the current sequences. The exception also
         carries the returned value. *)
      | Return e -> raise (EReturn(eval_expr e))
      (* Expression used as an instruction.
         The value is discarded. The typical usecase is for a function 
         call that produces effects (on memory or of the printed output) *)
      | Expr e -> ignore (eval_expr e)

    in

    (* Main code for the interpretation of a function call.
       Interpret the code of the function, and get ready to catch an
       exception [EReturn] carrying the result. In case no exception is
       raised, which happens is the interpretation reaches the end of the
       code of the function without meeting a [Return] instruction,
       return [Undef]. *)
    try
      exec_seq fdef.code; Undef
    with
      EReturn v -> v
    
  in

  (* Main code for interpreting a program.
     Execute a call to the function "main", with arg as unique parameter. *)
  exec_call "main" [arg]
  
(* Challenge: this interpreter has a subtle difference with the one given 
   in caml reinforcement session 2, regarding the actual range of local
   variables. Can you spot it? Which one implements the semantics you
   would expect? *)
