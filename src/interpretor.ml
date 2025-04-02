(* interpretor.ml *)

open Printf
open Ast        (* Provides: exptree, stmt, program *)
open Lexer
open Parser
open Typecheck

(*
   Extended interpreter to support integer, float, boolean, vector arithmetic,
   and additional functions: abs, angle, magnitude, dimension, scalar multiply,
   power, if-then-else, and a new unit type so that block expressions always yield a unit.
*)

(* ------------------------------------------------------------------------- *)
(* Runtime Values *)
type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VUnit                   (* New: Unit value *)
  | VVectorInt of int list
  | VVectorFloat of float list

(* ------------------------------------------------------------------------- *)
(* Opcodes for the Stack Machine *)
type opcode =
  | PushInt of int             (* Push an integer constant onto the stack *)
  | PushFloat of float         (* Push a float constant onto the stack *)
  | PushBool of bool           (* Push a boolean constant onto the stack *)
  | PushUnit                 (* New: Push the unit value *)
  | PushVectorInt of int list  (* Push an integer vector literal *)
  | PushVectorFloat of float list  (* Push a float vector literal *)
  | Load of string             (* Load the value of a variable onto the stack *)
  | Store of string            (* Store the top of the stack into a variable *)
  | Add                      (* Arithmetic addition *)
  | Sub                      (* Arithmetic subtraction *)
  | Mul                      (* Arithmetic multiplication *)
  | Div                      (* Arithmetic division *)
  | And                      (* Boolean AND *)
  | Or                       (* Boolean OR *)
  | Not                      (* Boolean NOT *)
  | Eq                       (* Equality comparison *)
  | Greater                  (* Greater-than comparison *)
  | GreaterEq                (* Greater-than or equal comparison *)
  | Less                     (* Less-than comparison *)
  | LessEq                   (* Less-than or equal comparison *)
  | PlusV                    (* Vector addition *)
  | MinusV                   (* Vector subtraction *)
  | DotProduct               (* Dot product of two vectors *)
  | AbsOp                    (* Absolute value *)
  | AngleOp                  (* Angle between two vectors *)
  | MagnitudeOp              (* Magnitude (Euclidean norm) of a vector *)
  | DimensionOp              (* Dimension (length) of a vector *)
  | ScalarMultiplyOp         (* Multiply a scalar with a vector *)
  | PowerOp                  (* Exponentiation operation *)
  | IfOp of opcode list * opcode list  (* If-then-else: then branch and else branch *)

(* ------------------------------------------------------------------------- *)
(* Helper: Efficient exponentiation by squaring for integers *)
let rec pow_int base exp =
  if exp < 0 then failwith "Negative exponent not supported for integers"
  else if exp = 0 then 1
  else if exp mod 2 = 0 then
    let half = pow_int base (exp / 2) in
    half * half
  else
    base * pow_int base (exp - 1)

(* ------------------------------------------------------------------------- *)
(* Compiler: Convert an AST expression (exptree) into a list of opcodes *)
let rec compile_expr (e: exptree) : opcode list =
  match e with
  | Int n -> [PushInt n]
  | Float f -> [PushFloat f]
  | Bool b -> [PushBool b]
  | Var s -> [Load s]
  | VectorInt (_, lst) -> [PushVectorInt lst]
  | VectorFloat (_, lst) -> [PushVectorFloat lst]
  | Add (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [Add]
  | Sub (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [Sub]
  | Mul (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [Mul]
  | Div (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [Div]
  | And (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [And]
  | Or (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [Or]
  | Not e -> compile_expr e @ [Not]
  | Eq (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [Eq]
  | Greater (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [Greater]
  | GreaterEq (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [GreaterEq]
  | Less (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [Less]
  | LessEq (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [LessEq]
  | PlusV (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [PlusV]
  | MinusV (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [MinusV]
  | DotProduct (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [DotProduct]
  | Abs e -> compile_expr e @ [AbsOp]
  | Angle (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [AngleOp]
  | Magnitude e -> compile_expr e @ [MagnitudeOp]
  | Dimension e -> compile_expr e @ [DimensionOp]
  | ScalarMultiply (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [ScalarMultiplyOp]
  | Power (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [PowerOp]
  | If (cond, then_expr, else_expr) ->
         compile_expr cond @ [IfOp (compile_expr then_expr, compile_expr else_expr)]
  | BlockExp stmts ->
         (* Compile each statement in the block, then yield unit *)
         let opcodes = List.fold_left (fun acc stmt -> acc @ compile_stmt stmt) [] stmts in
         opcodes @ [PushUnit]
  | _ -> failwith "Expression not supported in interpreter"

(* ------------------------------------------------------------------------- *)
(* Compile a statement into opcodes.
   We support assignment, declarations, and expression statements.
*)
and compile_stmt (s: Ast.stmt) : opcode list =
  match s with
  | Assign (var, expr)
  | DeclareInt (var, expr)
  | DeclareFloat (var, expr)
  | DeclareBool (var, expr)
  | DeclareVectorInt (var, expr)
  | DeclareVectorFloat (var, expr) ->
      compile_expr expr @ [Store var]
  | ExprStmt e -> compile_expr e   (* Now handles expression statements, including if expressions *)
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
      | PushInt n -> eval_opcodes rest (VInt n :: stack)
      | PushFloat f -> eval_opcodes rest (VFloat f :: stack)
      | PushBool b -> eval_opcodes rest (VBool b :: stack)
      | PushUnit -> eval_opcodes rest (VUnit :: stack)
      | PushVectorInt lst -> eval_opcodes rest (VVectorInt lst :: stack)
      | PushVectorFloat lst -> eval_opcodes rest (VVectorFloat lst :: stack)
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
                 | _ -> failwith "Type error in Add operation"
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
                 | _ -> failwith "Type error in Sub operation"
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
                 | _ -> failwith "Type error in Mul operation"
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
                 | _ -> failwith "Type error in Div operation"
               in
               eval_opcodes rest (result :: stack')
           | _ -> failwith "Stack underflow during Div")
      | And ->
          (match stack with
           | v2 :: v1 :: stack' ->
               let result =
                 match v1, v2 with
                 | VBool b1, VBool b2 -> VBool (b1 && b2)
                 | _ -> failwith "Type error in And operation"
               in
               eval_opcodes rest (result :: stack')
           | _ -> failwith "Stack underflow during And")
      | Or ->
          (match stack with
           | v2 :: v1 :: stack' ->
               let result =
                 match v1, v2 with
                 | VBool b1, VBool b2 -> VBool (b1 || b2)
                 | _ -> failwith "Type error in Or operation"
               in
               eval_opcodes rest (result :: stack')
           | _ -> failwith "Stack underflow during Or")
      | Not ->
          (match stack with
           | v :: stack' ->
               let result =
                 match v with
                 | VBool b -> VBool (not b)
                 | _ -> failwith "Type error in Not operation"
               in
               eval_opcodes rest (result :: stack')
           | _ -> failwith "Stack underflow during Not")
      | Eq ->
          (match stack with
           | v2 :: v1 :: stack' ->
               let result =
                 match v1, v2 with
                 | VInt i1, VInt i2 -> VBool (i1 = i2)
                 | VFloat f1, VFloat f2 -> VBool (f1 = f2)
                 | VInt i, VFloat f | VFloat f, VInt i -> VBool (float_of_int i = f)
                 | VBool b1, VBool b2 -> VBool (b1 = b2)
                 | _ -> failwith "Type error in Eq operation"
               in
               eval_opcodes rest (result :: stack')
           | _ -> failwith "Stack underflow during Eq")
      | Greater ->
          (match stack with
           | v2 :: v1 :: stack' ->
               let result =
                 match v1, v2 with
                 | VInt i1, VInt i2 -> VBool (i1 > i2)
                 | VFloat f1, VFloat f2 -> VBool (f1 > f2)
                 | VInt i, VFloat f | VFloat f, VInt i -> VBool (float_of_int i > f)
                 | _ -> failwith "Type error in Greater operation"
               in
               eval_opcodes rest (result :: stack')
           | _ -> failwith "Stack underflow during Greater")
      | GreaterEq ->
          (match stack with
           | v2 :: v1 :: stack' ->
               let result =
                 match v1, v2 with
                 | VInt i1, VInt i2 -> VBool (i1 >= i2)
                 | VFloat f1, VFloat f2 -> VBool (f1 >= f2)
                 | VInt i, VFloat f | VFloat f, VInt i -> VBool (float_of_int i >= f)
                 | _ -> failwith "Type error in GreaterEq operation"
               in
               eval_opcodes rest (result :: stack')
           | _ -> failwith "Stack underflow during GreaterEq")
      | Less ->
          (match stack with
           | v2 :: v1 :: stack' ->
               let result =
                 match v1, v2 with
                 | VInt i1, VInt i2 -> VBool (i1 < i2)
                 | VFloat f1, VFloat f2 -> VBool (f1 < f2)
                 | VInt i, VFloat f | VFloat f, VInt i -> VBool (float_of_int i < f)
                 | _ -> failwith "Type error in Less operation"
               in
               eval_opcodes rest (result :: stack')
           | _ -> failwith "Stack underflow during Less")
      | LessEq ->
          (match stack with
           | v2 :: v1 :: stack' ->
               let result =
                 match v1, v2 with
                 | VInt i1, VInt i2 -> VBool (i1 <= i2)
                 | VFloat f1, VFloat f2 -> VBool (f1 <= f2)
                 | VInt i, VFloat f | VFloat f, VInt i -> VBool (float_of_int i <= f)
                 | _ -> failwith "Type error in LessEq operation"
               in
               eval_opcodes rest (result :: stack')
           | _ -> failwith "Stack underflow during LessEq")
      | PlusV ->
          (match stack with
           | v2 :: v1 :: stack' ->
               let result =
                 match v1, v2 with
                 | VVectorInt lst1, VVectorInt lst2 ->
                     if List.length lst1 <> List.length lst2 then
                       failwith "Vector dimension mismatch in PlusV"
                     else VVectorInt (List.map2 ( + ) lst1 lst2)
                 | VVectorFloat lst1, VVectorFloat lst2 ->
                     if List.length lst1 <> List.length lst2 then
                       failwith "Vector dimension mismatch in PlusV"
                     else VVectorFloat (List.map2 ( +. ) lst1 lst2)
                 | VVectorInt lst1, VVectorFloat lst2 ->
                     if List.length lst1 <> List.length lst2 then
                       failwith "Vector dimension mismatch in PlusV"
                     else VVectorFloat (List.map2 (fun i f -> (float_of_int i) +. f) lst1 lst2)
                 | VVectorFloat lst1, VVectorInt lst2 ->
                     if List.length lst1 <> List.length lst2 then
                       failwith "Vector dimension mismatch in PlusV"
                     else VVectorFloat (List.map2 (fun f i -> f +. (float_of_int i)) lst1 lst2)
                 | _ -> failwith "Type error in PlusV operation"
               in
               eval_opcodes rest (result :: stack')
           | _ -> failwith "Stack underflow during PlusV")
      | MinusV ->
          (match stack with
           | v2 :: v1 :: stack' ->
               let result =
                 match v1, v2 with
                 | VVectorInt lst1, VVectorInt lst2 ->
                     if List.length lst1 <> List.length lst2 then
                       failwith "Vector dimension mismatch in MinusV"
                     else VVectorInt (List.map2 ( - ) lst1 lst2)
                 | VVectorFloat lst1, VVectorFloat lst2 ->
                     if List.length lst1 <> List.length lst2 then
                       failwith "Vector dimension mismatch in MinusV"
                     else VVectorFloat (List.map2 ( -. ) lst1 lst2)
                 | VVectorInt lst1, VVectorFloat lst2 ->
                     if List.length lst1 <> List.length lst2 then
                       failwith "Vector dimension mismatch in MinusV"
                     else VVectorFloat (List.map2 (fun i f -> (float_of_int i) -. f) lst1 lst2)
                 | VVectorFloat lst1, VVectorInt lst2 ->
                     if List.length lst1 <> List.length lst2 then
                       failwith "Vector dimension mismatch in MinusV"
                     else VVectorFloat (List.map2 (fun f i -> f -. (float_of_int i)) lst1 lst2)
                 | _ -> failwith "Type error in MinusV operation"
               in
               eval_opcodes rest (result :: stack')
           | _ -> failwith "Stack underflow during MinusV")
      | DotProduct ->
          (match stack with
           | v2 :: v1 :: stack' ->
               let result =
                 match v1, v2 with
                 | VVectorInt lst1, VVectorInt lst2 ->
                     if List.length lst1 <> List.length lst2 then
                       failwith "Vector dimension mismatch in DotProduct"
                     else VInt (List.fold_left (+) 0 (List.map2 ( * ) lst1 lst2))
                 | VVectorFloat lst1, VVectorFloat lst2 ->
                     if List.length lst1 <> List.length lst2 then
                       failwith "Vector dimension mismatch in DotProduct"
                     else VFloat (List.fold_left (+.) 0.0 (List.map2 ( *. ) lst1 lst2))
                 | VVectorInt lst1, VVectorFloat lst2 ->
                     if List.length lst1 <> List.length lst2 then
                       failwith "Vector dimension mismatch in DotProduct"
                     else VFloat (List.fold_left (+.) 0.0 (List.map2 (fun i f -> (float_of_int i) *. f) lst1 lst2))
                 | VVectorFloat lst1, VVectorInt lst2 ->
                     if List.length lst1 <> List.length lst2 then
                       failwith "Vector dimension mismatch in DotProduct"
                     else VFloat (List.fold_left (+.) 0.0 (List.map2 (fun f i -> f +. (float_of_int i)) lst1 lst2))
                 | _ -> failwith "Type error in DotProduct operation"
               in
               eval_opcodes rest (result :: stack')
           | _ -> failwith "Stack underflow during DotProduct")
      | AbsOp ->
          (match stack with
           | v :: stack' ->
               let result =
                 match v with
                 | VInt i -> VInt (abs i)
                 | VFloat f -> VFloat (abs_float f)
                 | VVectorInt lst -> VVectorInt (List.map abs lst)
                 | VVectorFloat lst -> VVectorFloat (List.map abs_float lst)
                 | _ -> failwith "Type error in Abs operation"
               in
               eval_opcodes rest (result :: stack')
           | _ -> failwith "Stack underflow during Abs")
      | AngleOp ->
          (match stack with
           | v2 :: v1 :: stack' ->
               let dot, mag1, mag2 =
                 match v1, v2 with
                 | VVectorInt lst1, VVectorInt lst2 ->
                     if List.length lst1 <> List.length lst2 then
                       failwith "Vector dimension mismatch in Angle"
                     else
                       ( List.fold_left (+.) 0.0 (List.map2 (fun i j -> (float_of_int i) *. (float_of_int j)) lst1 lst2),
                         sqrt (List.fold_left (fun acc i -> acc +. ((float_of_int i) ** 2.0)) 0.0 lst1),
                         sqrt (List.fold_left (fun acc i -> acc +. ((float_of_int i) ** 2.0)) 0.0 lst2) )
                 | VVectorFloat lst1, VVectorFloat lst2 ->
                     if List.length lst1 <> List.length lst2 then
                       failwith "Vector dimension mismatch in Angle"
                     else
                       ( List.fold_left (+.) 0.0 (List.map2 ( *. ) lst1 lst2),
                         sqrt (List.fold_left (fun acc f -> acc +. (f ** 2.0)) 0.0 lst1),
                         sqrt (List.fold_left (fun acc f -> acc +. (f ** 2.0)) 0.0 lst2) )
                 | VVectorInt lst1, VVectorFloat lst2 ->
                     if List.length lst1 <> List.length lst2 then
                       failwith "Vector dimension mismatch in Angle"
                     else
                       ( List.fold_left (+.) 0.0 (List.map2 (fun i f -> (float_of_int i) *. f) lst1 lst2),
                         sqrt (List.fold_left (fun acc i -> acc +. ((float_of_int i) ** 2.0)) 0.0 lst1),
                         sqrt (List.fold_left (fun acc f -> acc +. (f ** 2.0)) 0.0 lst2) )
                 | VVectorFloat lst1, VVectorInt lst2 ->
                     if List.length lst1 <> List.length lst2 then
                       failwith "Vector dimension mismatch in Angle"
                     else
                       ( List.fold_left (+.) 0.0 (List.map2 (fun f i -> f *. (float_of_int i)) lst1 lst2),
                         sqrt (List.fold_left (fun acc f -> acc +. (f ** 2.0)) 0.0 lst1),
                         sqrt (List.fold_left (fun acc i -> acc +. ((float_of_int i) ** 2.0)) 0.0 lst2) )
                 | _ -> failwith "Type error in Angle operation: expected vectors"
               in
               if mag1 *. mag2 = 0.0 then failwith "Zero magnitude vector in Angle operation"
               else
                 let cosine = dot /. (mag1 *. mag2) in
                 let angle = acos cosine in
                 eval_opcodes rest (VFloat angle :: stack')
           | _ -> failwith "Stack underflow during Angle")
      | MagnitudeOp ->
          (match stack with
           | v :: stack' ->
               let result =
                 match v with
                 | VVectorInt lst ->
                     VFloat (sqrt (List.fold_left (fun acc i -> acc +. ((float_of_int i) ** 2.0)) 0.0 lst))
                 | VVectorFloat lst ->
                     VFloat (sqrt (List.fold_left (fun acc f -> acc +. (f ** 2.0)) 0.0 lst))
                 | _ -> failwith "Type error in Magnitude operation: expected vector"
               in
               eval_opcodes rest (result :: stack')
           | _ -> failwith "Stack underflow during Magnitude")
      | DimensionOp ->
          (match stack with
           | v :: stack' ->
               let result =
                 match v with
                 | VVectorInt lst -> VInt (List.length lst)
                 | VVectorFloat lst -> VInt (List.length lst)
                 | _ -> failwith "Type error in Dimension operation: expected vector"
               in
               eval_opcodes rest (result :: stack')
           | _ -> failwith "Stack underflow during Dimension")
      | ScalarMultiplyOp ->
          (match stack with
           | v2 :: v1 :: stack' ->
               let result =
                 match v1, v2 with
                 | VInt i, VVectorInt lst ->
                     VVectorInt (List.map (fun x -> i * x) lst)
                 | VInt i, VVectorFloat lst ->
                     VVectorFloat (List.map (fun x -> (float_of_int i) *. x) lst)
                 | VFloat f, VVectorInt lst ->
                     VVectorFloat (List.map (fun x -> f *. (float_of_int x)) lst)
                 | VFloat f, VVectorFloat lst ->
                     VVectorFloat (List.map (fun x -> f *. x) lst)
                 | VVectorInt lst, VInt i ->
                     VVectorInt (List.map (fun x -> x * i) lst)
                 | VVectorInt lst, VFloat f ->
                     VVectorFloat (List.map (fun x -> (float_of_int x) *. f) lst)
                 | VVectorFloat lst, VInt i ->
                     VVectorFloat (List.map (fun x -> x *. (float_of_int i)) lst)
                 | VVectorFloat lst, VFloat f ->
                     VVectorFloat (List.map (fun x -> x *. f) lst)
                 | _ -> failwith "Type error in ScalarMultiply operation: expected scalar and vector"
               in
               eval_opcodes rest (result :: stack')
           | _ -> failwith "Stack underflow during ScalarMultiply")
      | PowerOp ->
          (match stack with
           | v2 :: v1 :: stack' ->
               let result =
                 match v1, v2 with
                 | VInt base, VInt exp ->
                     if exp < 0 then
                       VFloat ((float_of_int base) ** (float_of_int exp))
                     else
                       VInt (pow_int base exp)
                 | VInt base, VFloat exp ->
                     VFloat ((float_of_int base) ** exp)
                 | VFloat base, VInt exp ->
                     VFloat (base ** (float_of_int exp))
                 | VFloat base, VFloat exp ->
                     VFloat (base ** exp)
                 | _ -> failwith "Type error in Power operation"
               in
               eval_opcodes rest (result :: stack')
           | _ -> failwith "Stack underflow during Power")
      | IfOp (then_ops, else_ops) ->
          (match stack with
           | v :: stack' ->
               (match v with
                | VBool true ->
                    let branch_result = eval_opcodes then_ops [] in
                    (match branch_result with
                     | r :: [] -> eval_opcodes rest (r :: stack')
                     | _ -> failwith "Then branch did not yield a single result")
                | VBool false ->
                    let branch_result = eval_opcodes else_ops [] in
                    (match branch_result with
                     | r :: [] -> eval_opcodes rest (r :: stack')
                     | _ -> failwith "Else branch did not yield a single result")
                | _ -> failwith "If condition is not boolean")
           | _ -> failwith "Stack underflow during IfOp")
      
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
    | VBool b -> printf "%s = %b\n" k b
    | VUnit -> printf "%s = Unit\n" k
    | VVectorInt lst -> printf "%s = [%s]\n" k (String.concat "; " (List.map string_of_int lst))
    | VVectorFloat lst -> printf "%s = [%s]\n" k (String.concat "; " (List.map string_of_float lst))
  ) env
