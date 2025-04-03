(* ast.ml *)

type exptree =
  | Int of int
  | Float of float
  | Bool of bool
  | Var of string
  | VectorInt of (int * int list)
  | VectorFloat of (int * float list)
  | MatrixInt of ((int * int) * int list list)
  | MatrixFloat of ((int * int) * float list list)
  | VectorAcess of string * exptree
  | MatrixAcess of string * exptree * exptree
  | Abs of exptree
  | Neg of exptree
  | Pos of exptree
  | Add of exptree * exptree
  | Sub of exptree * exptree
  | Mul of exptree * exptree
  | Div of exptree * exptree
  | Mod of exptree * exptree
  | And of exptree * exptree
  | Or of exptree * exptree
  | Power of exptree * exptree
  | Not of exptree
  | Eq of exptree * exptree
  | Greater of exptree * exptree
  | GreaterEq of exptree * exptree
  | Less of exptree * exptree
  | LessEq of exptree * exptree
  | If of exptree * exptree * exptree
  | While of exptree * exptree       (* condition, block_expr body *)
  | For of stmt * exptree * stmt * exptree  (* init, condition, increment, block_expr body *)
  | Angle of exptree * exptree  (* Two vectors *)
  | Magnitude of exptree
  | Dimension of exptree
  | DotProduct of exptree * exptree
  | Transpose of exptree
  | Determinant of exptree
  | ScalarMultiply of exptree * exptree
  | Input of string
  | Print of string option
  | PlusV of exptree * exptree
  | MinusV of exptree * exptree
  | PlusM of exptree * exptree
  | MinusM of exptree * exptree
  | MultiplyM of exptree * exptree
  | BlockExp of stmt list
  | Inverse of exptree

and stmt =
  | DeclareInt of string * exptree
  | DeclareFloat of string * exptree
  | DeclareBool of string * exptree
  | DeclareVectorInt of string * exptree
  | DeclareVectorFloat of string * exptree
  | DeclareMatrixInt of string * exptree
  | DeclareMatrixFloat of string * exptree
  | Assign of string * exptree
  | AssignVector of string * exptree * exptree  (* vector_name, index, value *)
  | AssignMatrix of string * exptree * exptree * exptree  (* matrix_name, row, col, value *)
  | InputStmt of string
  | PrintStmt of string option
  | ExprStmt of exptree
  | Block of stmt list
  | WhileStmt of exptree * stmt list   (* condition, body *)
  | ForStmt of stmt * exptree * stmt * stmt list  (* init, condition, increment, body *)

type program =
  | Program of stmt list