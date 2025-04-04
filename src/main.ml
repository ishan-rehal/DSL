(* main.ml *)

open Ast
open Parser
open Typecheck
open Printf
open Interpretor   (* Access the run_program function *)

(* Function to print both to console and to a file *)
let print_both out_chan str =
  print_endline str;
  fprintf out_chan "%s\n" str

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
      "If(" ^ string_of_exptree e1 ^ ", " ^ string_of_exptree e2 ^ ", " ^
      string_of_exptree e3 ^ ")"
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
  | MatrixAcess (id, e1, e2) ->
      id ^ "[" ^ string_of_exptree e1 ^ "][" ^ string_of_exptree e2 ^ "]"
  | Power (base, exp) ->
      "Power(" ^ string_of_exptree base ^ ", " ^ string_of_exptree exp ^ ")"
      | Inverse e -> "Inverse(" ^ string_of_exptree e ^ ")"
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
      string_of_stmt update ^ ") { " ^ (String.concat "; " (List.map string_of_stmt body)) ^ " }"

      let string_of_program (Program stmts) =
        String.concat ";\n" (List.map string_of_stmt stmts)
      
      (* Check if the program is well-typed *)
      let check_types ~print_to_console prog out_chan =
        (* Helper for "write to out_chan, optionally also to console." *)
        let print_both str =
          if print_to_console then
            print_endline str;            (* Print to console if enabled *)
          Printf.fprintf out_chan "%s\n" str  (* Always write to file *)
        in
      
        print_both "Running type checking...";
        try
          let final_env = typecheck_program prog in
      
          print_both "Type checking successful!";
          print_both "\nFinal type environment:";
      
          List.iter (fun (var, ty) ->
            let str = sprintf "  %s : %s" var (string_of_type ty) in
            print_both str
          ) final_env;
      
          true
        with
        | TypecheckError message ->
            let err_msg = "Type Error: " ^ message in
            prerr_endline err_msg;               (* Typically print errors to stderr *)
            Printf.fprintf out_chan "%s\n" err_msg;
            false
        | e ->
            let err_msg = "Unexpected error during type checking: " ^ Printexc.to_string e in
            prerr_endline err_msg;
            Printf.fprintf out_chan "%s\n" err_msg;
            false
      
      (* A helper for "print to file, optionally also to console." *)
      let print_both ~print_to_console out_chan str =
        if print_to_console then
          print_endline str;        (* Print to console if desired *)
        fprintf out_chan "%s\n" str (* Always print to file *)
      
      let () =
        (* Toggle console printing here: set to false to suppress console output,
           or true to allow console prints. *)
        let print_to_console = false in
      
        if print_to_console then
          print_endline "Reading from input.txt...";
      
        let in_chan = open_in "input.txt" in
        let out_chan = open_out "output.txt" in
        let lexbuf = Lexing.from_channel in_chan in
      
        try
          (* 1) Parse the program. *)
          let prog = Parser.program Lexer.token lexbuf in
          close_in in_chan;
      
          (* 2) Print the parsed AST (optionally to console, always to file). *)
          let ast_str = "\nParsed AST:\n" ^ string_of_program prog in
          if print_to_console then
            print_endline ast_str;
          fprintf out_chan "%s\n" ast_str;
      
          (* 3) Type-check the program, with optional console printing. *)
          if check_types ~print_to_console prog out_chan then begin
            (* If the program is well-typed, proceed to execution. *)
            print_both ~print_to_console out_chan "\nProgram is well-typed and ready for execution.";
      
            (* 4) Run the program, passing the same toggle for console printing. *)
            run_program out_chan ~print_to_console prog
          end else
            print_both ~print_to_console out_chan
              "\nProgram contains type errors. Fix them before running."
      
        with
        | Lexer.Lexing_error msg ->
            let err_msg = "Lexing error: " ^ msg in
            prerr_endline err_msg;
            fprintf out_chan "%s\n" err_msg
        | Parsing.Parse_error ->
            let err_msg = "Parsing error at position "
                          ^ string_of_int (Lexing.lexeme_start lexbuf) in
            prerr_endline err_msg;
            fprintf out_chan "%s\n" err_msg
        | e ->
            let err_msg = "Unexpected error: " ^ Printexc.to_string e in
            prerr_endline err_msg;
            fprintf out_chan "%s\n" err_msg;
      
        (* Clean up channels. *)
        try close_in in_chan with _ -> ();
        try close_out out_chan with _ -> ();
        ()
        (* End of main.ml *)
        