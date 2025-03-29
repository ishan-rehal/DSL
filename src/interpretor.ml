(* interpretor.ml *)

open Printf
open Ast        (* Provides: exptree, stmt, program *)
open Lexer
open Parser
open Typecheck

(*
   For our simple evaluator, we support only integer/float arithmetic and 
   variable assignments (i.e. the [Assign] statement). All other AST constructs 
   are not handled.
*)

(* ------------------------------------------------------------------------- *)
(* Runtime Values *)
type value =
  | VInt of int
  | VFloat of float

(* ------------------------------------------------------------------------- *)
(* Opcodes for the Stack Machine *)
type opcode =
  | PushInt of int        (* Push an integer constant onto the stack *)
  | PushFloat of float    (* Push a float constant onto the stack *)
  | Load of string        (* Load the value of a variable onto the stack *)
  | Store of string       (* Store the top of the stack into a variable *)
  | Add                   (* Pop two values, add them, push the result *)
  | Sub                   (* Pop two values, subtract, push the result *)
  | Mul                   (* Pop two values, multiply, push the result *)
  | Div                   (* Pop two values, divide, push the result *)

(* ------------------------------------------------------------------------- *)
(* Compiler: Convert an AST expression (exptree) into a list of opcodes *)
let rec compile_expr (e: exptree) : opcode list =
  match e with
  | Int n -> [PushInt n]
  | Float f -> [PushFloat f]
  | Var s -> [Load s]
  | Add (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [Add]
  | Sub (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [Sub]
  | Mul (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [Mul]
  | Div (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [Div]
  | _ -> failwith "Expression not supported in interpreter"

(* ------------------------------------------------------------------------- *)
(* Compile a statement into opcodes.
   We only support the [Assign] statement for now.
*)
let compile_stmt (s: Ast.stmt) : opcode list =
  match s with
  | Assign (var, expr)
  | DeclareInt (var, expr)
  | DeclareFloat (var, expr) ->
      compile_expr expr @ [Store var]
  | _ -> failwith "Statement not supported in interpreter"


(* ------------------------------------------------------------------------- *)
(* Compile a program (Program of stmt list) into a list of opcodes *)
let compile_program (prog: Ast.program) : opcode list =
  match prog with
  | Program stmts ->
      List.fold_left (fun acc s -> acc @ compile_stmt s) [] stmts

(* ------------------------------------------------------------------------- *)
(* Environment for variable bindings *)
let env : (string, value) Hashtbl.t = Hashtbl.create 10

(* ------------------------------------------------------------------------- *)
(* Tail Recursive Stack-Based Evaluator *)
let rec eval_opcodes (code: opcode list) (stack: value list) : value list =
  match code with
  | [] -> stack
  | op :: rest ->
      match op with
      | PushInt n ->
          eval_opcodes rest (VInt n :: stack)
      | PushFloat f ->
          eval_opcodes rest (VFloat f :: stack)
      | Load s ->
          let v =
            try Hashtbl.find env s
            with Not_found -> failwith ("Undefined variable: " ^ s)
          in
          eval_opcodes rest (v :: stack)
      | Store s ->
          (match stack with
           | v :: stack' ->
               Hashtbl.replace env s v;
               eval_opcodes rest stack'
           | _ -> failwith "Stack underflow during Store")
      | Add ->
          (match stack with
           | v2 :: v1 :: stack' ->
               let result =
                 match v1, v2 with
                 | VInt i1, VInt i2 -> VInt (i1 + i2)
                 | VFloat f1, VFloat f2 -> VFloat (f1 +. f2)
                 | VInt i, VFloat f -> VFloat ((float_of_int i) +. f)
                 | VFloat f, VInt i -> VFloat (f +. (float_of_int i))
               in
               eval_opcodes rest (result :: stack')
           | _ -> failwith "Stack underflow during Add")
      | Sub ->
          (match stack with
           | v2 :: v1 :: stack' ->
               let result =
                 match v1, v2 with
                 | VInt i1, VInt i2 -> VInt (i1 - i2)
                 | VFloat f1, VFloat f2 -> VFloat (f1 -. f2)
                 | VInt i, VFloat f -> VFloat ((float_of_int i) -. f)
                 | VFloat f, VInt i -> VFloat (f -. (float_of_int i))
               in
               eval_opcodes rest (result :: stack')
           | _ -> failwith "Stack underflow during Sub")
      | Mul ->
          (match stack with
           | v2 :: v1 :: stack' ->
               let result =
                 match v1, v2 with
                 | VInt i1, VInt i2 -> VInt (i1 * i2)
                 | VFloat f1, VFloat f2 -> VFloat (f1 *. f2)
                 | VInt i, VFloat f -> VFloat ((float_of_int i) *. f)
                 | VFloat f, VInt i -> VFloat (f *. (float_of_int i))
               in
               eval_opcodes rest (result :: stack')
           | _ -> failwith "Stack underflow during Mul")
      | Div ->
          (match stack with
           | v2 :: v1 :: stack' ->
               let result =
                 match v1, v2 with
                 | VInt i1, VInt i2 ->
                     if i2 = 0 then failwith "Division by zero" else VInt (i1 / i2)
                 | VFloat f1, VFloat f2 ->
                     if f2 = 0.0 then failwith "Division by zero" else VFloat (f1 /. f2)
                 | VInt i, VFloat f ->
                     if f = 0.0 then failwith "Division by zero" else VFloat ((float_of_int i) /. f)
                 | VFloat f, VInt i ->
                     if i = 0 then failwith "Division by zero" else VFloat (f /. (float_of_int i))
               in
               eval_opcodes rest (result :: stack')
           | _ -> failwith "Stack underflow during Div")

(* ------------------------------------------------------------------------- *)
(* Run the program: compile the AST and evaluate it *)
let run_program (prog: Ast.program) : unit =
  let opcodes = compile_program prog in
  ignore (eval_opcodes opcodes []);
  printf "Evaluation complete. Variable values:\n";
  Hashtbl.iter (fun k v ->
    match v with
    | VInt i -> printf "%s = %d\n" k i
    | VFloat f -> printf "%s = %f\n" k f
  ) env
