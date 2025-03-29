(* typecheck.ml *)
open Ast

(* Custom exception for type errors *)
exception TypecheckError of string

(* Define the types in our language *)
type data_type =
  | TInt
  | TFloat
  | TBool
  | TUnit  (* For statements that don't return a value *)
  | TVectorInt of int  (* dimension *)
  | TVectorFloat of int  (* dimension *)
  | TMatrixInt of int * int  (* rows * cols *)
  | TMatrixFloat of int * int  (* rows * cols *)
  | TError of string  (* For type errors with message *)

(* Type environment to track variable types *)
type type_env = (string * data_type) list

(* Convert type to string for error reporting *)
let rec string_of_type = function
  | TInt -> "int"
  | TFloat -> "float"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TVectorInt dim -> "vector<int>[" ^ string_of_int dim ^ "]"
  | TVectorFloat dim -> "vector<float>[" ^ string_of_int dim ^ "]"
  | TMatrixInt (r, c) -> "matrix<int>[" ^ string_of_int r ^ "," ^ string_of_int c ^ "]"
  | TMatrixFloat (r, c) -> "matrix<float>[" ^ string_of_int r ^ "," ^ string_of_int c ^ "]"
  | TError msg -> "error: " ^ msg

(* Helper for raising type errors *)
let type_error msg =
  raise (TypecheckError msg)

(* Check if two types are compatible (for assignment) *)
let compatible t1 t2 =
  match t1, t2 with
  | TInt, TInt -> true
  | TFloat, TFloat -> true
  | TFloat, TInt -> true  (* Type promotion *)
  | TBool, TBool -> true
  | TUnit, TUnit -> true
  | TVectorInt d1, TVectorInt d2 -> d1 = d2
  | TVectorFloat d1, TVectorFloat d2 -> d1 = d2
  | TMatrixInt (r1, c1), TMatrixInt (r2, c2) -> r1 = r2 && c1 = c2
  | TMatrixFloat (r1, c1), TMatrixFloat (r2, c2) -> r1 = r2 && c1 = c2
  | _, _ -> false

(* Helper functions for common type checking patterns that take type_of_expr as parameter *)
let check_binary_numeric_op type_of_expr env e1 e2 =
  match type_of_expr env e1, type_of_expr env e2 with
  | TInt, TInt -> TInt
  | TFloat, TFloat -> TFloat
  | TInt, TFloat | TFloat, TInt -> TFloat  (* Type promotion *)
  | t1, t2 -> type_error ("Binary operation requires numeric types, got: " ^ 
                          string_of_type t1 ^ " and " ^ string_of_type t2)

let check_binary_int_op type_of_expr env e1 e2 =
  match type_of_expr env e1, type_of_expr env e2 with
  | TInt, TInt -> TInt
  | t1, t2 -> type_error ("Integer operation requires int types, got: " ^ 
                          string_of_type t1 ^ " and " ^ string_of_type t2)

let check_binary_bool_op type_of_expr env e1 e2 =
  match type_of_expr env e1, type_of_expr env e2 with
  | TBool, TBool -> TBool
  | t1, t2 -> type_error ("Boolean operation requires bool types, got: " ^ 
                          string_of_type t1 ^ " and " ^ string_of_type t2)

let check_vector_binary_op type_of_expr env v1 v2 =
  match type_of_expr env v1, type_of_expr env v2 with
  | TVectorInt d1, TVectorInt d2 when d1 = d2 -> TVectorInt d1
  | TVectorFloat d1, TVectorFloat d2 when d1 = d2 -> TVectorFloat d1
  | TVectorInt d1, TVectorFloat d2 when d1 = d2 -> TVectorFloat d1
  | TVectorFloat d1, TVectorInt d2 when d1 = d2 -> TVectorFloat d1
  | TVectorInt _, TVectorInt _ | TVectorFloat _, TVectorFloat _ ->
    type_error "Vectors must have same dimension for vector operations"
  | t1, t2 -> 
    type_error ("Vector operations require vectors, got: " ^ 
                string_of_type t1 ^ " and " ^ string_of_type t2)

let check_matrix_add_op type_of_expr env m1 m2 =
  match type_of_expr env m1, type_of_expr env m2 with
  | TMatrixInt (r1, c1), TMatrixInt (r2, c2) when r1 = r2 && c1 = c2 -> 
    TMatrixInt (r1, c1)
  | TMatrixFloat (r1, c1), TMatrixFloat (r2, c2) when r1 = r2 && c1 = c2 -> 
    TMatrixFloat (r1, c1)
  | TMatrixInt (r1, c1), TMatrixFloat (r2, c2) when r1 = r2 && c1 = c2 -> 
    TMatrixFloat (r1, c1)
  | TMatrixFloat (r1, c1), TMatrixInt (r2, c2) when r1 = r2 && c1 = c2 -> 
    TMatrixFloat (r1, c1)
  | TMatrixInt _, TMatrixInt _ | TMatrixFloat _, TMatrixFloat _ ->
    type_error "Matrices must have same dimensions for addition/subtraction"
  | t1, t2 -> 
    type_error ("Matrix operations require matrices, got: " ^ 
                string_of_type t1 ^ " and " ^ string_of_type t2)

let check_matrix_multiply_op type_of_expr env m1 m2 =
  match type_of_expr env m1, type_of_expr env m2 with
  | TMatrixInt (r1, c1), TMatrixInt (r2, c2) when c1 = r2 -> 
    TMatrixInt (r1, c2)
  | TMatrixFloat (r1, c1), TMatrixFloat (r2, c2) when c1 = r2 -> 
    TMatrixFloat (r1, c2)
  | TMatrixInt (r1, c1), TMatrixFloat (r2, c2) when c1 = r2 -> 
    TMatrixFloat (r1, c2)
  | TMatrixFloat (r1, c1), TMatrixInt (r2, c2) when c1 = r2 -> 
    TMatrixFloat (r1, c2)
  | TMatrixInt _, TMatrixInt _ | TMatrixFloat _, TMatrixFloat _ ->
    type_error "Matrix multiplication requires compatible dimensions (cols of first = rows of second)"
  | t1, t2 -> 
    type_error ("Matrix multiplication requires matrices, got: " ^ 
                string_of_type t1 ^ " and " ^ string_of_type t2)

(* Define the main functions once, with mutually recursive definitions *)
let rec type_of_expr env expr =
  match expr with
  | Int _ -> TInt
  | Float _ -> TFloat
  | Bool _ -> TBool
  | Var id -> 
    (try List.assoc id env 
     with Not_found -> type_error ("Undefined variable: " ^ id))
  | VectorInt (dim, _) -> TVectorInt dim
  | VectorFloat (dim, _) -> TVectorFloat dim
  | MatrixInt ((r, c), _) -> TMatrixInt (r, c)
  | MatrixFloat ((r, c), _) -> TMatrixFloat (r, c)
  | VectorAcess (id, idx) ->
    let id_type = 
      try List.assoc id env
      with Not_found -> type_error ("Undefined vector: " ^ id)
    in
    let idx_type = type_of_expr env idx in

    if idx_type <> TInt then
      type_error ("Vector index must be an integer, got: " ^ string_of_type idx_type)
    else
      (match id_type with
       | TVectorInt _ -> TInt
       | TVectorFloat _ -> TFloat
       | _ -> type_error (id ^ " is not a vector"))

  | MatrixAcess (id, row, col) ->
    let id_type = 
      try List.assoc id env
      with Not_found -> type_error ("Undefined matrix: " ^ id)
    in
    let row_type = type_of_expr env row in
    let col_type = type_of_expr env col in

    if row_type <> TInt || col_type <> TInt then
      type_error "Matrix indices must be integers"
    else
      (match id_type with
       | TMatrixInt _ -> TInt
       | TMatrixFloat _ -> TFloat
       | _ -> type_error (id ^ " is not a matrix"))

  | Abs e -> 
    (match type_of_expr env e with
     | TInt -> TInt
     | TFloat -> TFloat
     | t -> type_error ("abs requires numeric type, got: " ^ string_of_type t))
  
     | Pos e ->
      let t = type_of_expr env e in
      if t = TInt || t = TFloat then t
      else type_error ("Unary plus expects numeric type, got: " ^ string_of_type t)
  


  | Neg e ->
    (match type_of_expr env e with
     | TInt -> TInt
     | TFloat -> TFloat
     | t -> type_error ("negation requires numeric type, got: " ^ string_of_type t))

  | Add (e1, e2) -> check_binary_numeric_op type_of_expr env e1 e2
  | Sub (e1, e2) -> check_binary_numeric_op type_of_expr env e1 e2
  | Mul (e1, e2) -> check_binary_numeric_op type_of_expr env e1 e2
  | Div (e1, e2) -> check_binary_numeric_op type_of_expr env e1 e2

  | Mod (e1, e2) ->
    check_binary_int_op type_of_expr env e1 e2

  | And (e1, e2) -> check_binary_bool_op type_of_expr env e1 e2
  | Or (e1, e2) -> check_binary_bool_op type_of_expr env e1 e2

  | Not e ->
    (match type_of_expr env e with
     | TBool -> TBool
     | t -> type_error ("not requires boolean type, got: " ^ string_of_type t))

  | Eq (e1, e2) | Greater (e1, e2) | GreaterEq (e1, e2) 
  | Less (e1, e2) | LessEq (e1, e2) ->
    let t1 = type_of_expr env e1 in
    let t2 = type_of_expr env e2 in

    if compatible t1 t2 then TBool
    else type_error ("Comparison requires compatible types, got: " ^ 
                     string_of_type t1 ^ " and " ^ string_of_type t2)

  | If (cond, then_expr, else_expr) ->
    let cond_type = type_of_expr env cond in
    if cond_type <> TBool then
      type_error ("If condition must be boolean, got: " ^ string_of_type cond_type)
    else
      let then_type = type_of_expr env then_expr in
      let else_type = type_of_expr env else_expr in

      if compatible then_type else_type then then_type
      else type_error ("If branches have incompatible types: " ^ 
                       string_of_type then_type ^ " and " ^ string_of_type else_type)

  | While (cond, body) ->
    let cond_type = type_of_expr env cond in
    if cond_type <> TBool then
      type_error ("While condition must be boolean, got: " ^ string_of_type cond_type)
    else
      type_of_expr env body

  | For (init, cond, update, body) ->
    let env' = typecheck_stmt env init in
    let cond_type = type_of_expr env' cond in

    if cond_type <> TBool then
      type_error ("For condition must be boolean, got: " ^ string_of_type cond_type)
    else
      let _ = typecheck_stmt env' update in
      type_of_expr env' body

  | Angle (e1, e2) ->
        (match type_of_expr env e1, type_of_expr env e2 with
         | (TVectorInt d1, TVectorInt d2) when d1 = d2 -> TFloat
         | (TVectorFloat d1, TVectorFloat d2) when d1 = d2 -> TFloat
         | (TVectorInt d1, TVectorFloat d2) when d1 = d2 -> TFloat
         | (TVectorFloat d1, TVectorInt d2) when d1 = d2 -> TFloat
         | (TVectorInt _, TVectorInt _) | (TVectorFloat _, TVectorFloat _) -> 
             type_error "Vectors must have same dimension for angle calculation"
         | (t1, _) when not (match t1 with TVectorInt _ | TVectorFloat _ -> true | _ -> false) -> 
             type_error ("First argument to angle must be a vector, got: " ^ string_of_type t1)
         | (_, t2) -> 
             type_error ("Second argument to angle must be a vector, got: " ^ string_of_type t2))

  | Magnitude e ->
    (match type_of_expr env e with
     | TVectorInt _ | TVectorFloat _ -> TFloat
     | t -> type_error ("magnitude requires vector, got: " ^ string_of_type t))

  | Dimension e ->
    (match type_of_expr env e with
     | TVectorInt _ | TVectorFloat _ -> TInt
     | t -> type_error ("dimension requires vector, got: " ^ string_of_type t))

  | DotProduct (e1, e2) ->
    (match type_of_expr env e1, type_of_expr env e2 with
     | TVectorInt d1, TVectorInt d2 when d1 = d2 -> TInt
     | TVectorFloat d1, TVectorFloat d2 when d1 = d2 -> TFloat
     | TVectorInt d1, TVectorFloat d2 when d1 = d2 -> TFloat  (* Type promotion *)
     | TVectorFloat d1, TVectorInt d2 when d1 = d2 -> TFloat  (* Type promotion *)
     | TVectorInt _, TVectorInt _ | TVectorFloat _, TVectorFloat _ ->
       type_error "Vectors must have same dimension for dot product"
     | t1, t2 -> type_error ("dot_product requires vectors, got: " ^ 
                             string_of_type t1 ^ " and " ^ string_of_type t2))

  | Transpose e ->
    (match type_of_expr env e with
     | TMatrixInt (r, c) -> TMatrixInt (c, r)
     | TMatrixFloat (r, c) -> TMatrixFloat (c, r)
     | t -> type_error ("transpose requires matrix, got: " ^ string_of_type t))

  | Determinant e ->
    (match type_of_expr env e with
     | TMatrixInt (r, c) when r = c -> TInt
     | TMatrixFloat (r, c) when r = c -> TFloat
     | TMatrixInt _ | TMatrixFloat _ -> 
       type_error "Determinant requires a square matrix"
     | t -> type_error ("determinant requires matrix, got: " ^ string_of_type t))
  (* Add this to the type_of_expr function *)

  | ScalarMultiply (scalar, expr) ->
    let scalar_t = type_of_expr env scalar in
    let expr_t = type_of_expr env expr in
    (match scalar_t, expr_t with
     (* Vector cases *)
     | TInt, TVectorInt d -> TVectorInt d
     | TFloat, TVectorFloat d -> TVectorFloat d
     | TInt, TVectorFloat d | TFloat, TVectorInt d -> TVectorFloat d
     
     (* Matrix cases - add these *)
     | TInt, TMatrixInt (r, c) -> TMatrixInt (r, c)
     | TFloat, TMatrixFloat (r, c) -> TMatrixFloat (r, c)
     | TInt, TMatrixFloat (r, c) | TFloat, TMatrixInt (r, c) -> TMatrixFloat (r, c)
     
     (* Error cases *)
     | _, (TVectorInt _ | TVectorFloat _ | TMatrixInt _ | TMatrixFloat _) -> 
       type_error ("First arg to scalar_multiply must be numeric, got: " ^ string_of_type scalar_t)
     | _, _ -> type_error ("Second arg to scalar_multiply must be vector or matrix, got: " ^ string_of_type expr_t)
    )
  | Power (base, exp) ->
    let base_type = type_of_expr env base in
    let exp_type = type_of_expr env exp in
    (match base_type, exp_type with
     | (TInt | TFloat), (TInt | TFloat) -> TFloat
     | _, (TInt | TFloat) -> 
       type_error ("First argument to power must be numeric, got: " ^ string_of_type base_type)
     | _, _ -> 
       type_error ("Second argument to power must be numeric, got: " ^ string_of_type exp_type)
    )

  | PlusV (v1, v2) -> check_vector_binary_op type_of_expr env v1 v2
  | MinusV (v1, v2) -> check_vector_binary_op type_of_expr env v1 v2

  | PlusM (m1, m2) -> check_matrix_add_op type_of_expr env m1 m2
  | MinusM (m1, m2) -> check_matrix_add_op type_of_expr env m1 m2
  | MultiplyM (m1, m2) -> check_matrix_multiply_op type_of_expr env m1 m2

  | BlockExp stmts ->
    let _ = List.fold_left typecheck_stmt env stmts in
    TUnit  (* Would require adding a TUnit constructor to data_type *)

  | Input _ | Print _ -> TInt  (* Input/output operations return success code *)


(* Type check a statement and return the updated environment *)
and typecheck_stmt env stmt =
  match stmt with
  | DeclareInt (id, e) ->
    let t = type_of_expr env e in
    if t = TInt then
      (id, TInt) :: env
    else
      type_error ("Expected int but got " ^ string_of_type t ^ " in declaration of " ^ id)

(* Update DeclareFloat pattern *)
| DeclareFloat (id, e) ->
  let t = type_of_expr env e in
  if t = TFloat || t = TInt then  (* Allow int promotion *)
    (id, TFloat) :: env
  else
    type_error ("Expected float or int but got " ^ string_of_type t ^ " in declaration of " ^ id)


  | DeclareBool (id, e) ->
    let t = type_of_expr env e in
    if t = TBool then
      (id, TBool) :: env
    else
      type_error ("Expected bool but got " ^ string_of_type t ^ " in declaration of " ^ id)

  | DeclareVectorInt (id, e) ->
    let t = type_of_expr env e in
    (match t with
     | TVectorInt dim -> (id, TVectorInt dim) :: env
     | _ -> type_error ("Expected vector<int> but got " ^ string_of_type t ^ " in declaration of " ^ id))


(* Update DeclareVectorFloat pattern *)
| DeclareVectorFloat (id, e) ->
  let t = type_of_expr env e in
  (match t with
   | TVectorFloat dim -> (id, TVectorFloat dim) :: env
   | TVectorInt dim -> (id, TVectorFloat dim) :: env  (* Allow int->float vector promotion *)
   | _ -> type_error ("Expected vector<float> but got " ^ string_of_type t ^ " in declaration of " ^ id))

  | DeclareMatrixInt (id, e) ->
    let t = type_of_expr env e in
    (match t with
     | TMatrixInt (r, c) -> (id, TMatrixInt (r, c)) :: env
     | _ -> type_error ("Expected matrix<int> but got " ^ string_of_type t ^ " in declaration of " ^ id))

(* Update DeclareMatrixFloat pattern *)
| DeclareMatrixFloat (id, e) ->
  let t = type_of_expr env e in
  (match t with
   | TMatrixFloat (r, c) -> (id, TMatrixFloat (r, c)) :: env
   | TMatrixInt (r, c) -> (id, TMatrixFloat (r, c)) :: env  (* Allow int->float matrix promotion *)
   | _ -> type_error ("Expected matrix<float> but got " ^ string_of_type t ^ " in declaration of " ^ id))

  | Assign (id, e) ->
    let var_type = 
      try List.assoc id env 
      with Not_found -> type_error ("Variable " ^ id ^ " not declared before assignment")
    in
    let expr_type = type_of_expr env e in

    if compatible var_type expr_type then env
    else type_error ("Cannot assign " ^ string_of_type expr_type ^ " to " ^ id ^ 
                     " (variable type: " ^ string_of_type var_type ^ ")")

(* Update vector element assignment handling *)
| AssignVector (id, idx, val_expr) ->
  let vec_type = 
    try List.assoc id env 
    with Not_found -> type_error ("Vector " ^ id ^ " not declared before assignment")
  in
  let idx_type = type_of_expr env idx in
  if idx_type <> TInt then
    type_error ("Vector index must be an integer, got: " ^ string_of_type idx_type)
  else
   ( match vec_type with
    | TVectorInt _ -> 
        let val_type = type_of_expr env val_expr in
        if val_type <> TInt then
          type_error ("Cannot assign " ^ string_of_type val_type ^ " to element of vector<int>")
        else env
    | TVectorFloat _ ->
        let val_type = type_of_expr env val_expr in
        if not (compatible TFloat val_type) then  (* This line now allows int elements for float vectors *)
          type_error ("Cannot assign " ^ string_of_type val_type ^ " to element of vector<float>")
        else env
    | _ -> type_error (id ^ " is not a vector"))

(* Update matrix element assignment handling *)
| AssignMatrix (id, row_idx, col_idx, val_expr) ->
  let mat_type = 
    try List.assoc id env 
    with Not_found -> type_error ("Matrix " ^ id ^ " not declared before assignment")
  in
  let row_type = type_of_expr env row_idx in
  let col_type = type_of_expr env col_idx in
  
  if row_type <> TInt || col_type <> TInt then
    type_error "Matrix indices must be integers"
  else
    (match mat_type with
    | TMatrixInt _ -> 
        let val_type = type_of_expr env val_expr in
        if val_type <> TInt then
          type_error ("Cannot assign " ^ string_of_type val_type ^ " to element of matrix<int>")
        else env
    | TMatrixFloat _ ->
        let val_type = type_of_expr env val_expr in
        if not (compatible TFloat val_type) then  (* This line now allows int elements for float matrices *)
          type_error ("Cannot assign " ^ string_of_type val_type ^ " to element of matrix<float>")
        else env
    | _ -> type_error (id ^ " is not a matrix"))

  | InputStmt _  ->
    env  (* These don't affect the type environment *)

  | PrintStmt opt_id ->
      (match opt_id with
       | None -> env  (* Empty print statement is valid *)
       | Some id -> 
           try 
             let _ = List.assoc id env in  (* Check if identifier exists *)
             env  (* Return unchanged environment if identifier exists *)
           with Not_found -> 
             type_error ("Print statement refers to undefined variable: " ^ id))


  | ExprStmt e ->
    let _ = type_of_expr env e in
    env

  | Block stmts ->
    List.fold_left typecheck_stmt env stmts

  | WhileStmt (cond, body) ->
    let cond_type = type_of_expr env cond in
    if cond_type <> TBool then
      type_error ("While condition must be boolean, got: " ^ string_of_type cond_type)
    else
      List.fold_left typecheck_stmt env body

  | ForStmt (init, cond, update, body) ->
    let env' = typecheck_stmt env init in
    let cond_type = type_of_expr env' cond in

    if cond_type <> TBool then
      type_error ("For condition must be boolean, got: " ^ string_of_type cond_type)
    else
      let env'' = typecheck_stmt env' update in
      List.fold_left typecheck_stmt env'' body

(* Type check an entire program *)
let typecheck_program (Program stmts) =
  let env = [] in  (* Start with empty environment *)
  let final_env = List.fold_left typecheck_stmt env stmts in
  final_env  (* Return the final environment *)