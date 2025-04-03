(* interpretor.ml *)

open Printf
open Ast        (* Provides: exptree, stmt, program *)
open Lexer
open Parser
open Typecheck

  (*
    Extended interpreter to support integer, float, boolean, vector, and matrix arithmetic.
    Additional functions include: abs, angle, magnitude, dimension, scalar multiply,
    power, if-then-else, and a unit type so that block expressions always yield a unit.
  *)

(* ------------------------------------------------------------------------- *)
(* Runtime Values *)
type value =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VUnit                        (* Unit value *)
  | VVectorInt of int list
  | VVectorFloat of float list
  | VMatrixInt of ((int * int) * int list list)
  | VMatrixFloat of ((int * int) * float list list)

(* ------------------------------------------------------------------------- *)
(* Opcodes for the Stack Machine *)
type opcode =
  | PushInt of int             
  | PushFloat of float         
  | PushBool of bool           
  | PushUnit                 
  | PushVectorInt of int list  
  | PushVectorFloat of float list  
  | PushMatrixInt of ((int * int) * int list list)
  | PushMatrixFloat of ((int * int) * float list list)
  | Load of string             
  | Store of string
  | NegOp
  | PosOp
  | Add                      
  | Sub                      
  | Mul                      
  | Div 
  | ModOp                     
  | And                      
  | Or                       
  | Not                      
  | Eq                       
  | Greater                  
  | GreaterEq                
  | Less                     
  | LessEq                   
  | PlusV                    
  | MinusV                   
  | DotProduct               
  | AbsOp                    
  | AngleOp                  
  | MagnitudeOp              
  | DimensionOp              
  | ScalarMultiplyOp 
  | InverseOp   (* Matrix inverse *)        
  | PowerOp                  
  | IfOp of opcode list * opcode list
  | PlusM                    (* Matrix elementwise addition *)
  | MinusM                   (* Matrix elementwise subtraction *)
  | MultiplyM                (* Standard matrix multiplication *)
  | TransposeOp              (* Matrix transpose *)
  | DeterminantOp            (* Matrix determinant *)
  | VectorAccessOp of string
  | MatrixAccessOp of string
  | WhileOp of opcode list * opcode list
  | PrintOp of string option
  | InputOp of string


(* ------------------------------------------------------------------------- *)
(* Helper Functions for Matrix Operations *)

(* Convert an integer matrix to a float matrix *)
let to_float_matrix (((r, c), rows)) =
  ((r, c), List.map (List.map float_of_int) rows)

(* Elementwise addition for integer matrices *)
let matrix_add_int (((r1, c1), rows1)) (((r2, c2), rows2)) =
  if r1 <> r2 || c1 <> c2 then failwith "Matrix dimensions do not match for addition"
  else ((r1, c1), List.map2 (List.map2 ( + )) rows1 rows2)

(* Elementwise addition for float matrices *)
let matrix_add_float (((r1, c1), rows1)) (((r2, c2), rows2)) =
  if r1 <> r2 || c1 <> c2 then failwith "Matrix dimensions do not match for addition"
  else ((r1, c1), List.map2 (List.map2 ( +. )) rows1 rows2)

(* Elementwise subtraction for integer matrices *)
let matrix_sub_int (((r1, c1), rows1)) (((r2, c2), rows2)) =
  if r1 <> r2 || c1 <> c2 then failwith "Matrix dimensions do not match for subtraction"
  else ((r1, c1), List.map2 (List.map2 ( - )) rows1 rows2)

(* Elementwise subtraction for float matrices *)
let matrix_sub_float (((r1, c1), rows1)) (((r2, c2), rows2)) =
  if r1 <> r2 || c1 <> c2 then failwith "Matrix dimensions do not match for subtraction"
  else ((r1, c1), List.map2 (List.map2 ( -. )) rows1 rows2)

(* Standard matrix multiplication producing a float matrix *)
(* Multiply two integer matrices (element-wise multiplication using standard matrix multiplication) *)
let matrix_multiply_int (((r1, c1), rows1) : ((int * int) * int list list))
    (((r2, c2), rows2) : ((int * int) * int list list)) : ((int * int) * int list list) =
  if c1 <> r2 then failwith "Matrix dimensions do not match for multiplication"
  else
    let result_rows =
      List.init r1 (fun i ->
          List.init c2 (fun j ->
              let row = List.nth rows1 i in
              let col = List.map (fun row -> List.nth row j) rows2 in
              List.fold_left2 (fun acc a b -> acc + (a * b)) 0 row col
            )
        )
    in
    ((r1, c2), result_rows)

(* Multiply two matrices whose rows are already floats *)
let matrix_multiply_float (((r1, c1), rows1) : (int * int) * float list list)
    (((r2, c2), rows2) : (int * int) * float list list) =
  if c1 <> r2 then failwith "Matrix dimensions do not match for multiplication"
  else
    let result_rows =
      List.init r1 (fun i ->
          List.init c2 (fun j ->
              let row = List.nth rows1 i in
              let col = List.map (fun row -> List.nth row j) rows2 in
              List.fold_left2 (fun acc a b -> acc +. (a *. b)) 0.0 row col
            )
        )
    in
    ((r1, c2), result_rows)


(* Transpose a square matrix *)
let matrix_transpose (((r, c), rows) ) =
  if r <> c then failwith "Transpose is only supported for square matrices"
  else ((r, c), List.init r (fun i -> List.map (fun row -> List.nth row i) rows))

(* Optional: Define filteri if your OCaml version lacks it *)
  (*
  let filteri f lst =
    let rec aux i acc = function
      | [] -> List.rev acc
      | x :: xs -> aux (i + 1) (if f i x then x :: acc else acc) xs
    in
    aux 0 [] lst
  *)

(* Recursive determinant for a square matrix represented as a float list list *)
let rec determinant (matrix : float list list) : float =
  match matrix with
  | [] -> failwith "Empty matrix"
  | [x] -> List.hd x
  | m ->
    let rec aux i acc =
      if i >= List.length m then acc
      else
        let sign = if i mod 2 = 0 then 1.0 else -1.0 in
        let head = List.hd m in
        let factor = sign *. (List.nth head i) in
        let submatrix =
          List.map (fun row ->
              (* If filteri is not available, use the following:
                 let rec filter_not_i i = function
                 | [] -> []
                 | x :: xs -> if i = 0 then filter_not_i (i+1) xs else x :: filter_not_i (i+1) xs
                 in
                 filter_not_i 0 row
              *)
              List.filteri (fun j _ -> j <> i) row
            ) (List.tl m)
        in
        aux (i + 1) (acc +. (factor *. determinant submatrix))
    in
    aux 0 0.0

let rec drop n lst = if n <= 0 then lst else match lst with [] -> [] | _::xs -> drop (n-1) xs
let rec find_index p lst =
  let rec aux i = function
    | [] -> raise Not_found
    | x::xs -> if p x then i else aux (i+1) xs
  in aux 0 lst
let swap i j lst =
  let rec aux idx = function
    | [] -> []
    | x::xs ->
      if idx = i then List.nth lst j :: aux (idx+1) xs
      else if idx = j then List.nth lst i :: aux (idx+1) xs
      else x :: aux (idx+1) xs
  in aux 0 lst

(* Inverse of integer matrices - converts to float first *)
(* Inverse of integer matrices - computes with floats but rounds back to int *)
let inverse_matrix_int ((n, c), rows) =
  if n = 0 then failwith "Cannot invert empty matrix";
  if n <> c then failwith "Matrix must be square";
  let identity = List.init n (fun i -> List.init n (fun j -> if i = j then 1.0 else 0.0)) in
  let augmented = List.map2 ( @ ) (List.map (List.map float_of_int) rows) identity in
  let rec gauss_jordan m i =
    if i >= n then m
    else
      let pivot_row = 
        try find_index (fun row -> abs_float (List.nth row i) > 1e-10) (drop i m)
        with Not_found -> failwith "Matrix is singular"
      in
      let pivot_row = pivot_row + i in
      let m = swap i pivot_row m in
      let pivot = List.nth (List.nth m i) i in
      let pivot_row_normalized = List.map (fun x -> x /. pivot) (List.nth m i) in
      let m = List.mapi (fun r row ->
        if r = i then pivot_row_normalized
        else
          let factor = List.nth row i in
          List.map2 (fun a b -> a -. factor *. b) row pivot_row_normalized
      ) m in
      gauss_jordan m (i + 1)
  in
  let reduced = gauss_jordan augmented 0 in
  let inv_float = List.map (fun row -> drop n row) reduced in
  (* Round back to integers *)
  let inv_int = List.map (List.map (fun x -> int_of_float (Float.round x))) inv_float in
  ((n, n), inv_int)

(* Inverse of float matrices - no conversion needed *)
let inverse_matrix_float ((n, c), rows) =
  if n = 0 then failwith "Cannot invert empty matrix";
  if n <> c then failwith "Matrix must be square";
  let identity = List.init n (fun i -> List.init n (fun j -> if i = j then 1.0 else 0.0)) in
  let augmented = List.map2 ( @ ) rows identity in
  let rec gauss_jordan m i =
    if i >= n then m
    else
      let pivot_row = 
        try find_index (fun row -> abs_float (List.nth row i) > 1e-10) (drop i m)
        with Not_found -> failwith "Matrix is singular"
      in
      let pivot_row = pivot_row + i in
      let m = swap i pivot_row m in
      let pivot = List.nth (List.nth m i) i in
      let pivot_row_normalized = List.map (fun x -> x /. pivot) (List.nth m i) in
      let m = List.mapi (fun r row ->
        if r = i then pivot_row_normalized
        else
          let factor = List.nth row i in
          List.map2 (fun a b -> a -. factor *. b) row pivot_row_normalized
      ) m in
      gauss_jordan m (i + 1)
  in
  let reduced = gauss_jordan augmented 0 in
  let inv = List.map (fun row -> drop n row) reduced in
  ((n, n), inv)

(* ------------------------------------------------------------------------- *)
(* In the Compiler, add cases for matrix literals and matrix operations *)
(* MatrixInt and MatrixFloat are compiled to push the corresponding opcode. *)
(* PlusM, MinusM, MultiplyM, Transpose, Determinant are compiled similarly. *)

(* The rest of compile_expr and compile_stmt remains the same as in your code. *)

(* In the evaluator, new opcodes for matrices are handled as follows: *)

(* For PushMatrixInt and PushMatrixFloat: *)
(* They push a VMatrixInt or VMatrixFloat value onto the stack. *)

(* For PlusM and MinusM: *)
(* They pattern-match on VMatrixInt/VMatrixFloat operands and use matrix_add_int/float or matrix_sub_int/float. *)

(* For MultiplyM: *)
(* They always convert integer matrices to float matrices and then call matrix_multiply, yielding a VMatrixFloat. *)

(* For TransposeOp: *)
(* They call matrix_transpose on the matrix operand. *)

(* For DeterminantOp: *)
(* They convert an integer matrix to float (if needed) and compute the determinant via the determinant function. *)

(* ------------------------------------------------------------------------- *)
(* The rest of your interpreter code (compile_expr, compile_stmt, eval_opcodes, run_program) remains as provided. *)

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
  | MatrixInt m -> [PushMatrixInt m]
  | MatrixFloat m -> [PushMatrixFloat m]
  | Neg e -> compile_expr e @ [NegOp]
  | Pos e -> compile_expr e @ [PosOp]
  | Add (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [Add]
  | Sub (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [Sub]
  | Mul (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [Mul]
  | Div (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [Div]
  | Mod (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [ModOp]
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
  | PlusM (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [PlusM]
  | MinusM (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [MinusM]
  | MultiplyM (e1, e2) -> compile_expr e1 @ compile_expr e2 @ [MultiplyM]
  | Transpose e -> compile_expr e @ [TransposeOp]
  | Determinant e -> compile_expr e @ [DeterminantOp]
  | If (cond, then_expr, else_expr) ->
    compile_expr cond @ [IfOp (compile_expr then_expr, compile_expr else_expr)]
  | BlockExp stmts ->
    let ops = List.fold_left (fun acc stmt -> acc @ compile_stmt stmt) [] stmts in
    ops @ [PushUnit]
  | While (cond, body) -> [WhileOp (compile_expr cond, compile_expr body)]
  | For (init, cond, update, body) ->
    (* Transform a for-loop into:
        { init;
          while (cond) { body; update }
        }
        which yields unit.
    *)
    compile_expr (BlockExp ([init; WhileStmt (cond, [ExprStmt body; update])]))

  | VectorAcess (id, idx) ->
    (* First compile the index expression, then push the vector access opcode with the variable name *)
    compile_expr idx @ [VectorAccessOp id]

  | MatrixAcess (id, row, col) ->
    (* First compile the row index and then the column index, then push the matrix access opcode *)
    compile_expr row @ compile_expr col @ [MatrixAccessOp id]

  | Inverse e -> compile_expr e @ [InverseOp]


  | Input s -> [InputOp s]


  | _ -> failwith "Expression not supported in interpreter"

and compile_stmt (s: Ast.stmt) : opcode list =
  match s with
  | Assign (var, expr)
  | DeclareInt (var, expr)
  | DeclareFloat (var, expr)
  | DeclareBool (var, expr)
  | DeclareVectorInt (var, expr)
  | DeclareVectorFloat (var, expr)
  | DeclareMatrixInt (var, expr)
  | DeclareMatrixFloat (var, expr) ->
    compile_expr expr @ [Store var]
  | WhileStmt (cond, body) ->
    (* Convert a while statement into a while expression using BlockExp *)
    compile_expr (While (cond, BlockExp body))
  | ForStmt (init, cond, inc, body) ->
    (* Transform the for loop into:
        init;
        while (cond) { body; inc }
    *)
    let init_ops = compile_stmt init 
    in
    let while_ops = compile_expr (While (cond, BlockExp (body @ [inc])))
    in
    init_ops @ while_ops
  | ExprStmt e -> compile_expr e
  | PrintStmt (str) -> [PrintOp (str)]
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
    | PushMatrixInt m -> eval_opcodes rest (VMatrixInt m :: stack)
    | PushMatrixFloat m -> eval_opcodes rest (VMatrixFloat m :: stack)
    | Load s ->
      let v =
        try Hashtbl.find env s
        with Not_found -> failwith ("Undefined variable: " ^ s)
      in
      eval_opcodes rest (v :: stack)
    | Store s ->
      (match stack with
       | v :: stack' ->
         let v' =
           if Hashtbl.mem env s then
             let current = Hashtbl.find env s in
             match current, v with
             | VFloat _, VInt n -> VFloat (float_of_int n)
             | _ -> v
           else v
         in
         Hashtbl.replace env s v';
         eval_opcodes rest stack'
       | _ -> failwith "Stack underflow during Store")
    | InputOp s ->
      let input_string =
        if s = "" then
          (* Read from terminal *)
          read_line ()
        else
          (* Read entire file contents from file s *)
          let chan = open_in s in
          let rec read_all acc =
            try 
              let line = input_line chan in
              read_all (acc ^ "\n" ^ line)
            with End_of_file ->
              close_in chan;
              acc
          in
          read_all ""
      in
      (* Create a lexing buffer from the input string.
         Adjust the parser call if your Parser module requires a different signature. *)
      let lexbuf = Lexing.from_string input_string in
      let prog = Parser.program Lexer.token lexbuf in
      (* Typecheck the program (assumes typecheck_program is defined in your typecheck.ml) *)
      let _ = typecheck_program prog in
      (* Compile and evaluate the program *)
      let ops = compile_program prog in
      let result =
        match eval_opcodes ops [] with
        | r :: _ -> r
        | [] -> failwith "Input program did not yield a value"
      in
      eval_opcodes rest (result :: stack)
    | NegOp ->
      (match stack with
       | v :: stack' ->
         (match v with
          | VInt i -> eval_opcodes rest (VInt (-i) :: stack')
          | VFloat f -> eval_opcodes rest (VFloat (-. f) :: stack')
          | _ -> failwith "Type error in unary minus")
       | _ -> failwith "Stack underflow during NegOp")

    | PosOp ->
      (match stack with
       | v :: stack' ->
         (match v with
          | VInt i -> eval_opcodes rest (VInt i :: stack')
          | VFloat f -> eval_opcodes rest (VFloat f :: stack')
          | _ -> failwith "Type error in unary plus")
       | _ -> failwith "Stack underflow during PosOp")
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
    | ModOp ->
      (match stack with
       | v2 :: v1 :: stack' ->
         let result =
           (match v1, v2 with
            | VInt i1, VInt i2 ->
              if i2 = 0 then failwith "Modulo by zero" else VInt (i1 mod i2)
            | _ -> failwith "Type error in Mod operation")
         in
         eval_opcodes rest (result :: stack')
       | _ -> failwith "Stack underflow during Mod")
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
           (* --- Vector Cases --- *)
           | VInt s, VVectorInt vec ->
             VVectorInt (List.map (fun x -> s * x) vec)
           | VInt s, VVectorFloat vec ->
             VVectorFloat (List.map (fun x -> (float_of_int s) *. x) vec)
           | VFloat s, VVectorInt vec ->
             VVectorFloat (List.map (fun x -> s *. (float_of_int x)) vec)
           | VFloat s, VVectorFloat vec ->
             VVectorFloat (List.map (fun x -> s *. x) vec)
           | VVectorInt vec, VInt s ->
             VVectorInt (List.map (fun x -> x * s) vec)
           | VVectorInt vec, VFloat s ->
             VVectorFloat (List.map (fun x -> (float_of_int x) *. s) vec)
           | VVectorFloat vec, VInt s ->
             VVectorFloat (List.map (fun x -> x *. (float_of_int s)) vec)
           | VVectorFloat vec, VFloat s ->
             VVectorFloat (List.map (fun x -> x *. s) vec)
           (* --- Matrix Cases --- *)
           | VInt s, VMatrixInt mat ->
             let ((r, c), rows) = mat in
             VMatrixInt (((r, c), List.map (fun row -> List.map (fun x -> s * x) row) rows))
           | VInt s, VMatrixFloat mat ->
             let ((r, c), rows) = mat in
             VMatrixFloat (((r, c), List.map (fun row -> List.map (fun x -> (float_of_int s) *. x) row) rows))
           | VFloat s, VMatrixInt mat ->
             let ((r, c), rows) = mat in
             VMatrixFloat (((r, c), List.map (fun row -> List.map (fun x -> s *. (float_of_int x)) row) rows))
           | VFloat s, VMatrixFloat mat ->
             let ((r, c), rows) = mat in
             VMatrixFloat (((r, c), List.map (fun row -> List.map (fun x -> s *. x) row) rows))
           | VMatrixInt mat, VInt s ->
             let ((r, c), rows) = mat in
             VMatrixInt (((r, c), List.map (fun row -> List.map (fun x -> x * s) row) rows))
           | VMatrixInt mat, VFloat s ->
             let ((r, c), rows) = mat in
             VMatrixFloat (((r, c), List.map (fun row -> List.map (fun x -> (float_of_int x) *. s) row) rows))
           | VMatrixFloat mat, VInt s ->
             let ((r, c), rows) = mat in
             VMatrixFloat (((r, c), List.map (fun row -> List.map (fun x -> x *. (float_of_int s)) row) rows))
           | VMatrixFloat mat, VFloat s ->
             let ((r, c), rows) = mat in
             VMatrixFloat (((r, c), List.map (fun row -> List.map (fun x -> x *. s) row) rows))
           | _ -> failwith "Type error in ScalarMultiply operation: expected scalar and vector/matrix"
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
    | PlusM ->
      (match stack with
       | v2 :: v1 :: stack' ->
         let result =
           match v1, v2 with
           | VMatrixInt m1, VMatrixInt m2 ->
             VMatrixInt (matrix_add_int m1 m2)
           | VMatrixInt m1, VMatrixFloat m2 ->
             VMatrixFloat (matrix_add_float (to_float_matrix m1) m2)
           | VMatrixFloat m1, VMatrixInt m2 ->
             VMatrixFloat (matrix_add_float m1 (to_float_matrix m2))
           | VMatrixFloat m1, VMatrixFloat m2 ->
             VMatrixFloat (matrix_add_float m1 m2)
           | _ -> failwith "Type error in PlusM operation"
         in
         eval_opcodes rest (result :: stack')
       | _ -> failwith "Stack underflow during PlusM")
    | MinusM ->
      (match stack with
       | v2 :: v1 :: stack' ->
         let result =
           match v1, v2 with
           | VMatrixInt m1, VMatrixInt m2 ->
             VMatrixInt (matrix_sub_int m1 m2)
           | VMatrixInt m1, VMatrixFloat m2 ->
             VMatrixFloat (matrix_sub_float (to_float_matrix m1) m2)
           | VMatrixFloat m1, VMatrixInt m2 ->
             VMatrixFloat (matrix_sub_float m1 (to_float_matrix m2))
           | VMatrixFloat m1, VMatrixFloat m2 ->
             VMatrixFloat (matrix_sub_float m1 m2)
           | _ -> failwith "Type error in MinusM operation"
         in
         eval_opcodes rest (result :: stack')
       | _ -> failwith "Stack underflow during MinusM")
    | MultiplyM ->
      (match stack with
       | v2 :: v1 :: stack' ->
         let result =
           match v1, v2 with
           | VMatrixInt m1, VMatrixInt m2 ->
             VMatrixInt (matrix_multiply_int (m1) (m2))
           | VMatrixInt m1, VMatrixFloat m2 ->
             VMatrixFloat (matrix_multiply_float (to_float_matrix m1) m2)
           | VMatrixFloat m1, VMatrixInt m2 ->
             VMatrixFloat (matrix_multiply_float m1 (to_float_matrix m2))
           | VMatrixFloat m1, VMatrixFloat m2 ->
             VMatrixFloat (matrix_multiply_float m1 m2)
           | _ -> failwith "Type error in MultiplyM operation"
         in
         eval_opcodes rest (result :: stack')
       | _ -> failwith "Stack underflow during MultiplyM")

    | InverseOp ->
      (match stack with
        | v :: stack' ->
          let result =
            match v with
            | VMatrixInt m -> VMatrixInt (inverse_matrix_int m)
            | VMatrixFloat m -> VMatrixFloat (inverse_matrix_float m)
            | _ -> failwith "Type error in Inverse operation: expected matrix"
          in
          eval_opcodes rest (result :: stack')
       | _ -> failwith "Stack underflow during Inverse")

    | TransposeOp ->
      (match stack with
       | v :: stack' ->
         let result =
           match v with
           | VMatrixInt m -> VMatrixInt (matrix_transpose m)
           | VMatrixFloat m -> VMatrixFloat (matrix_transpose m)
           | _ -> failwith "Type error in Transpose operation: expected matrix"
         in
         eval_opcodes rest (result :: stack')
       | _ -> failwith "Stack underflow during Transpose")
    | DeterminantOp ->
      (match stack with
       | v :: stack' ->
         let result =
           match v with
           | VMatrixInt m ->
             VFloat (determinant (snd (to_float_matrix m)))
           | VMatrixFloat m ->
             VFloat (determinant (snd m))
           | _ -> failwith "Type error in Determinant operation: expected matrix"
         in
         eval_opcodes rest (result :: stack')
       | _ -> failwith "Stack underflow during Determinant")
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
    | WhileOp (cond_code, body_code) ->
      let cond_stack = eval_opcodes cond_code [] in
      (match cond_stack with
       | [VBool true] ->
         let _ = eval_opcodes body_code [] in
         eval_opcodes ([WhileOp (cond_code, body_code)] @ rest) stack
       | [VBool false] ->
         eval_opcodes rest (VUnit :: stack)
       | _ -> failwith "While condition did not yield a boolean")
    | VectorAccessOp id ->
      (match stack with
       | v_idx :: stack' ->
         (match v_idx with
          | VInt i ->
            let v =
              try Hashtbl.find env id
              with Not_found -> failwith ("Undefined vector: " ^ id)
            in
            (match v with
             | VVectorInt lst ->
               if i < 0 || i >= List.length lst then 
                 failwith "Vector index out of bounds"
               else eval_opcodes rest ((VInt (List.nth lst i)) :: stack')
             | VVectorFloat lst ->
               if i < 0 || i >= List.length lst then 
                 failwith "Vector index out of bounds"
               else eval_opcodes rest ((VFloat (List.nth lst i)) :: stack')
             | _ -> failwith (id ^ " is not a vector"))
          | _ -> failwith "Vector index is not an integer")
       | _ -> failwith "Stack underflow during vector access")

    | MatrixAccessOp id ->
      (match stack with
       | v_col :: v_row :: stack' ->
         (match v_row, v_col with
          | VInt i, VInt j ->
            let v =
              try Hashtbl.find env id
              with Not_found -> failwith ("Undefined matrix: " ^ id)
            in
            (match v with
             | VMatrixInt ((r, c), rows) ->
               if i < 0 || i >= r || j < 0 || j >= c then 
                 failwith "Matrix indices out of bounds"
               else eval_opcodes rest ((VInt (List.nth (List.nth rows i) j)) :: stack')
             | VMatrixFloat ((r, c), rows) ->
               if i < 0 || i >= r || j < 0 || j >= c then 
                 failwith "Matrix indices out of bounds"
               else eval_opcodes rest ((VFloat (List.nth (List.nth rows i) j)) :: stack')
             | _ -> failwith (id ^ " is not a matrix"))
          | _ -> failwith "Matrix indices must be integers")
       | _ -> failwith "Stack underflow during matrix access")
    | PrintOp opt ->
      (match opt with
       | None ->
         (* Print all variables in the environment *)
         Hashtbl.iter (fun k v ->
             match v with
             | VInt i -> printf "%s = %d\n" k i
             | VFloat f -> printf "%s = %f\n" k f
             | VBool b -> printf "%s = %b\n" k b
             | VUnit -> printf "%s = Unit\n" k
             | VVectorInt lst -> printf "%s = [%s]\n" k (String.concat "; " (List.map string_of_int lst))
             | VVectorFloat lst -> printf "%s = [%s]\n" k (String.concat "; " (List.map string_of_float lst))
             | VMatrixInt ((r, c), rows) ->
               printf "%s = MatrixInt (%d x %d):\n%s\n" k r c
                 (String.concat "\n" (List.map (fun row -> String.concat "; " (List.map string_of_int row)) rows))
             | VMatrixFloat ((r, c), rows) ->
               printf "%s = MatrixFloat (%d x %d):\n%s\n" k r c
                 (String.concat "\n" (List.map (fun row -> String.concat "; " (List.map string_of_float row)) rows))
           ) env;
         eval_opcodes rest stack
       | Some id ->
         (try 
            let v = Hashtbl.find env id in
            (match v with
             | VInt i -> printf "%s = %d\n" id i
             | VFloat f -> printf "%s = %f\n" id f
             | VBool b -> printf "%s = %b\n" id b
             | VUnit -> printf "%s = Unit\n" id
             | VVectorInt lst -> printf "%s = [%s]\n" id (String.concat "; " (List.map string_of_int lst))
             | VVectorFloat lst -> printf "%s = [%s]\n" id (String.concat "; " (List.map string_of_float lst))
             | VMatrixInt ((r, c), rows) ->
               printf "%s = MatrixInt (%d x %d):\n%s\n" id r c
                 (String.concat "\n" (List.map (fun row -> String.concat "; " (List.map string_of_int row)) rows))
             | VMatrixFloat ((r, c), rows) ->
               printf "%s = MatrixFloat (%d x %d):\n%s\n" id r c
                 (String.concat "\n" (List.map (fun row -> String.concat "; " (List.map string_of_float row)) rows))
            );
            eval_opcodes rest stack
          with Not_found ->
            printf "Variable %s not found\n" id;
            eval_opcodes rest stack))
    | _ -> failwith "Opcode not supported"


(* ------------------------------------------------------------------------- *)
(* Run the program: compile the AST and evaluate it *)(* A helper to print both to console and out_chan *)
(* A helper for "print to file, optionally also to console." *)
let print_both ~print_to_console out_chan str =
  if print_to_console then
    print_endline str;               (* Terminal output if enabled *)
  Printf.fprintf out_chan "%s\n" str  (* Always write to file *)

let run_program (out_chan: out_channel) ~(print_to_console: bool) (prog: Ast.program) : unit =
  (* 1) Compile the AST to opcodes *)
  let opcodes = compile_program prog in

  (* 2) Evaluate the opcodes, ignoring the final stack result *)
  ignore (eval_opcodes opcodes []);

  (* 3) Print results to both console (optionally) and file *)
  print_both ~print_to_console out_chan "Evaluation complete. Variable values:";
  Hashtbl.iter (fun k v ->
    match v with
    | VInt i ->
      print_both ~print_to_console out_chan (Printf.sprintf "%s = %d" k i)
    | VFloat f ->
      print_both ~print_to_console out_chan (Printf.sprintf "%s = %f" k f)
    | VBool b ->
      print_both ~print_to_console out_chan (Printf.sprintf "%s = %b" k b)
    | VUnit ->
      print_both ~print_to_console out_chan (Printf.sprintf "%s = Unit" k)
    | VVectorInt lst ->
      print_both ~print_to_console out_chan
        (Printf.sprintf "%s = [%s]" k (String.concat "; " (List.map string_of_int lst)))
    | VVectorFloat lst ->
      print_both ~print_to_console out_chan
        (Printf.sprintf "%s = [%s]" k (String.concat "; " (List.map string_of_float lst)))
    | VMatrixInt ((r, c), rows) ->
      let matrix_str =
        String.concat "\n"
          (List.map
             (fun row -> String.concat "; " (List.map string_of_int row))
             rows)
      in
      print_both ~print_to_console out_chan
        (Printf.sprintf "%s = MatrixInt (%d x %d):\n%s" k r c matrix_str)
    | VMatrixFloat ((r, c), rows) ->
      let matrix_str =
        String.concat "\n"
          (List.map
             (fun row -> String.concat "; " (List.map string_of_float row))
             rows)
      in
      print_both ~print_to_console out_chan
        (Printf.sprintf "%s = MatrixFloat (%d x %d):\n%s" k r c matrix_str)
  ) env
