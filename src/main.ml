(* main.ml *)
open Ast
open Parser
open Typecheck

(* Function to print both to console and to a file *)
let print_both out_chan str =
  print_endline str;
  Printf.fprintf out_chan "%s\n" str

let rec string_of_exptree expr =
  match expr with
  | Int n -> string_of_int n
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | Var id -> id
  | VectorInt (dim, lst) ->
    "VectorInt(" ^ string_of_int dim ^ ", [" ^
    (String.concat ", " (List.map string_of_int lst)) ^ "])"
  | VectorFloat (dim, lst) ->
    "VectorFloat(" ^ string_of_int dim ^ ", [" ^
    (String.concat ", " (List.map string_of_float lst)) ^ "])"
  | MatrixInt ((r, c), _) ->
    "MatrixInt((" ^ string_of_int r ^ ", " ^ string_of_int c ^ "), ...)"
  | MatrixFloat ((r, c), _) ->
    "MatrixFloat((" ^ string_of_int r ^ ", " ^ string_of_int c ^ "), ...)"
  | Abs e -> "Abs(" ^ string_of_exptree e ^ ")"
  | Pos e -> "Pos(" ^ string_of_exptree e ^ ")"
  | Neg e -> "Neg(" ^ string_of_exptree e ^ ")"
  | Add (e1, e2) -> "Add(" ^ string_of_exptree e1 ^ ", " ^ string_of_exptree e2 ^ ")"
  | Sub (e1, e2) -> "Sub(" ^ string_of_exptree e1 ^ ", " ^ string_of_exptree e2 ^ ")"
  | Mul (e1, e2) -> "Mul(" ^ string_of_exptree e1 ^ ", " ^ string_of_exptree e2 ^ ")"
  | Div (e1, e2) -> "Div(" ^ string_of_exptree e1 ^ ", " ^ string_of_exptree e2 ^ ")"
  | Mod (e1, e2) -> "Mod(" ^ string_of_exptree e1 ^ ", " ^ string_of_exptree e2 ^ ")"
  | And (e1, e2) -> "And(" ^ string_of_exptree e1 ^ ", " ^ string_of_exptree e2 ^ ")"
  | Or (e1, e2) -> "Or(" ^ string_of_exptree e1 ^ ", " ^ string_of_exptree e2 ^ ")"
  | Not e -> "Not(" ^ string_of_exptree e ^ ")"
  | Eq (e1, e2) -> "Eq(" ^ string_of_exptree e1 ^ ", " ^ string_of_exptree e2 ^ ")"
  | Greater (e1, e2) -> "Greater(" ^ string_of_exptree e1 ^ ", " ^ string_of_exptree e2 ^ ")"
  | GreaterEq (e1, e2) -> "GreaterEq(" ^ string_of_exptree e1 ^ ", " ^ string_of_exptree e2 ^ ")"
  | Less (e1, e2) -> "Less(" ^ string_of_exptree e1 ^ ", " ^ string_of_exptree e2 ^ ")"
  | LessEq (e1, e2) -> "LessEq(" ^ string_of_exptree e1 ^ ", " ^ string_of_exptree e2 ^ ")"
  | If (e1, e2, e3) ->
    "If(" ^ string_of_exptree e1 ^ ", " ^
    string_of_exptree e2 ^ ", " ^ string_of_exptree e3 ^ ")"
  | While (cond, body) ->
    "While(" ^ string_of_exptree cond ^ ", " ^ string_of_exptree body ^ ")"
  | For (init, cond, update, body) ->
    "For(" ^ string_of_stmt init ^ ", " ^ string_of_exptree cond ^ ", " ^ 
    string_of_stmt update ^ ", " ^ string_of_exptree body ^ ")"
  | Angle (e1, e2) -> "Angle(" ^ string_of_exptree e1 ^ ", " ^ string_of_exptree e2 ^ ")"
  | Magnitude e -> "Magnitude(" ^ string_of_exptree e ^ ")"
  | Dimension e -> "Dimension(" ^ string_of_exptree e ^ ")"
  | DotProduct (e1, e2) -> "DotProduct(" ^ string_of_exptree e1 ^ ", " ^ string_of_exptree e2 ^ ")"
  | Transpose e -> "Transpose(" ^ string_of_exptree e ^ ")"
  | Determinant e -> "Determinant(" ^ string_of_exptree e ^ ")"
  | ScalarMultiply (e1, e2) -> "ScalarMultiply(" ^ string_of_exptree e1 ^ ", " ^ string_of_exptree e2 ^ ")"
  | PlusV (e1, e2) -> "PlusV(" ^ string_of_exptree e1 ^ ", " ^ string_of_exptree e2 ^ ")"
  | MinusV (e1, e2) -> "MinusV(" ^ string_of_exptree e1 ^ ", " ^ string_of_exptree e2 ^ ")"
  | PlusM (e1, e2) -> "PlusM(" ^ string_of_exptree e1 ^ ", " ^ string_of_exptree e2 ^ ")"
  | MinusM (e1, e2) -> "MinusM(" ^ string_of_exptree e1 ^ ", " ^ string_of_exptree e2 ^ ")"
  | MultiplyM (e1, e2) -> "MultiplyM(" ^ string_of_exptree e1 ^ ", " ^ string_of_exptree e2 ^ ")"
  | BlockExp stmts -> "Block(" ^ (String.concat "; " (List.map string_of_stmt stmts)) ^ ")"
  | Input s -> "Input(" ^ s ^ ")"
  | Print opt_id -> 
      "Print(" ^ (match opt_id with Some id -> id | None -> "") ^ ")"
  | VectorAcess (id, e) -> id ^ "[" ^ string_of_exptree e ^ "]"
  | MatrixAcess (id, e1, e2) -> id ^ "[" ^ string_of_exptree e1 ^ "][" ^ string_of_exptree e2 ^ "]"
  | Power (base, exp) -> "Power(" ^ string_of_exptree base ^ ", " ^ string_of_exptree exp ^ ")"
and string_of_stmt stmt =
  match stmt with
  | DeclareInt (id, e) ->
    "int " ^ id ^ " := " ^ string_of_exptree e
  | DeclareFloat (id, e) ->
    "float " ^ id ^ " := " ^ string_of_exptree e
  | DeclareBool (id, e) ->
    "bool " ^ id ^ " := " ^ string_of_exptree e
  | DeclareVectorInt (id, e) ->
    "vector int " ^ id ^ " := " ^ string_of_exptree e
  | DeclareVectorFloat (id, e) ->
    "vector float " ^ id ^ " := " ^ string_of_exptree e
  | DeclareMatrixInt (id, e) ->
    "matrix int " ^ id ^ " := " ^ string_of_exptree e
  | DeclareMatrixFloat (id, e) ->
    "matrix float " ^ id ^ " := " ^ string_of_exptree e
  | Assign (id, e) ->
    id ^ " := " ^ string_of_exptree e

  | AssignVector (id, idx, val_expr) ->
      id ^ "[" ^ string_of_exptree idx ^ "] := " ^ string_of_exptree val_expr
  
  | AssignMatrix (id, row, col, val_expr) ->
      id ^ "[" ^ string_of_exptree row ^ "][" ^ string_of_exptree col ^ "] := " ^ 
      string_of_exptree val_expr

  | InputStmt s ->
    "Input(" ^ s ^ ")"
  | PrintStmt opt_id ->
      "print(" ^ (match opt_id with Some id -> id | None -> "") ^ ")"
  | ExprStmt e ->
    string_of_exptree e
  | Block stmts ->
    "{ " ^ (String.concat "; " (List.map string_of_stmt stmts)) ^ " }"
  | WhileStmt (cond, body) ->
    "while " ^ string_of_exptree cond ^ " { " ^ 
    (String.concat "; " (List.map string_of_stmt body)) ^ " }"
  | ForStmt (init, cond, update, body) ->
    "for(" ^ string_of_stmt init ^ "; " ^ string_of_exptree cond ^ "; " ^ 
    string_of_stmt update ^ ") { " ^ 
    (String.concat "; " (List.map string_of_stmt body)) ^ " }"

(* Check if the program is well-typed *)
let check_types prog out_chan =
  print_both out_chan "Running type checking...";
  try
    let final_env = typecheck_program prog in
    print_both out_chan "Type checking successful!";

    (* Optional: Print the final type environment *)
    print_both out_chan "\nFinal type environment:";
    List.iter (fun (var, ty) -> 
        let str = Printf.sprintf "  %s : %s" var (string_of_type ty) in
        print_string str;
        Printf.fprintf out_chan "%s" str;
        print_newline();
        output_char out_chan '\n';
      ) final_env;

    true (* Type checking succeeded *)
  with
  | TypecheckError message ->
    let err_msg = "Type Error: " ^ message in
    prerr_endline err_msg;
    Printf.fprintf out_chan "%s\n" err_msg;
    false (* Type checking failed *)
  | e ->
    let err_msg = "Unexpected error during type checking: " ^ Printexc.to_string e in
    prerr_endline err_msg;
    Printf.fprintf out_chan "%s\n" err_msg;
    false (* Type checking failed with unknown error *)

(* Pretty-print a whole program (list of statements) *)
let string_of_program (Program stmts) =
  String.concat ";\n" (List.map string_of_stmt stmts)

let () =
  print_endline "Reading from input.txt...";
  let in_chan = open_in "input.txt" in
  let out_chan = open_out "output.txt" in
  
  (* Use a try-with block with cleanup in both success and exception cases *)
  try
    let lexbuf = Lexing.from_channel in_chan in
    try
      let prog = Parser.program Lexer.token lexbuf in
      let ast_str = "\nParsed AST:\n" ^ (string_of_program prog) in
      print_endline ast_str;
      Printf.fprintf out_chan "%s\n" ast_str;

      (* Type check the program *)
      if check_types prog out_chan then
        print_both out_chan "\nProgram is well-typed and ready for execution."
      else begin
        let err_msg = "\nProgram contains type errors. Fix them before running." in
        prerr_endline err_msg;
        Printf.fprintf out_chan "%s\n" err_msg
      end
    with
    | Lexer.Lexing_error msg ->
        let err_msg = "Lexing error: " ^ msg in
        prerr_endline err_msg;
        Printf.fprintf out_chan "%s\n" err_msg
    | Parsing.Parse_error ->
        let err_msg = "Parsing error at position " ^ string_of_int (Lexing.lexeme_start lexbuf) in
        prerr_endline err_msg;
        Printf.fprintf out_chan "%s\n" err_msg
    | e ->
        let err_msg = "Unexpected error: " ^ Printexc.to_string e in
        prerr_endline err_msg;
        Printf.fprintf out_chan "%s\n" err_msg
  with e ->
    (* Handle exceptions from file opening *)
    prerr_endline ("File error: " ^ Printexc.to_string e)
  ;
  (* Ensure files are always closed *)
  (try close_in in_chan with _ -> ());
  (try close_out out_chan with _ -> ())