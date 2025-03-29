{
open Parser
(* Alias the token type so that Lexer.token is the same as Parser.token *)
type token = Parser.token

(* --------------------------------------------------------------------- *)
(* ORIGINAL CODE: definitions, exceptions, helper functions              *)
(* --------------------------------------------------------------------- *)

exception Lexing_error of string

let int_of_string s = int_of_string s
let float_of_string s = float_of_string s

let split_on_string sep s =
  let sep_len = String.length sep in
  let rec aux i acc =
    if i >= String.length s then List.rev acc
    else
      try
        let rec find_index k =
          if k > String.length s - sep_len then raise Not_found
          else if String.sub s k sep_len = sep then k
          else find_index (k + 1)
        in
        let j = find_index i in
        let part = String.sub s i (j - i) in
        aux (j + sep_len) (part :: acc)
      with Not_found ->
        let part = String.sub s i (String.length s - i) in
        List.rev (part :: acc)
  in
  aux 0 []

let parse_int_vector lexeme =
  let len = String.length lexeme in
  if len < 2 then raise (Lexing_error ("Invalid vector literal: " ^ lexeme))
  else
    let inner = String.sub lexeme 1 (len - 2) in
    let parts = split_on_string "," inner in
    List.map (fun part -> int_of_string (String.trim part)) parts

let parse_float_vector lexeme =
  let len = String.length lexeme in
  if len < 2 then raise (Lexing_error ("Invalid vector literal: " ^ lexeme))
  else
    let inner = String.sub lexeme 1 (len - 2) in
    let parts = split_on_string "," inner in
    List.map (fun part -> float_of_string (String.trim part)) parts

(* Add this at the beginning of the file *)
open Str

let parse_int_matrix s =
  let len = String.length s in
  if len < 4 then raise (Lexing_error ("Invalid matrix literal: " ^ s))
  else if not (String.sub s 0 2 = "[[" && String.sub s (len-2) 2 = "]]") then
    raise (Lexing_error ("Matrix literal must start with \"[[\" and end with \"]]\": " ^ s))
  else
    let inner = String.sub s 2 (len - 4) in
    (* Improved regex to handle spaces BOTH before and after commas *)
    let rows = Str.split (Str.regexp "\\][ \t]*,[ \t]*\\[") inner in
    List.map (fun r -> parse_int_vector ("[" ^ r ^ "]")) rows

let parse_float_matrix s =
  let len = String.length s in
  if len < 4 then raise (Lexing_error ("Invalid matrix literal: " ^ s))
  else if not (String.sub s 0 2 = "[[" && String.sub s (len-2) 2 = "]]") then
    raise (Lexing_error ("Matrix literal must start with \"[[\" and end with \"]]\": " ^ s))
  else
    let inner = String.sub s 2 (len - 4) in
    (* Improved regex to handle spaces BOTH before and after commas *)
    let rows = Str.split (Str.regexp "\\][ \t]*,[ \t]*\\[") inner in
    List.map (fun r -> parse_float_vector ("[" ^ r ^ "]")) rows

(* Note: Do not redefine the token type here. We alias it above. *)
}

(* ----------------------------------------------------------------------- *)
(* ORIGINAL CODE: Regex definitions                                       *)
(* ----------------------------------------------------------------------- *)

let digit         = ['0'-'9']
let number        = digit+
let float_number  = number "." number
let sign          = ['+' '-']?
let ident_regex   = ['a'-'z' 'A'-'Z'] (['a'-'z' 'A'-'Z' '0'-'9' '_']*) ( '\'' )?
let whitespace    = [' ' '\t' '\n']+
let h_whitespace  = [' ' '\t']+
let vec_ws        = h_whitespace ("\n")?
let mat_ws        = whitespace*
let signed_int = sign number

(* A single float regex with optional scientific notation *)
let float_number_scientific =
  ( float_number (['e' 'E'] sign? number)? )
| ( number (['e' 'E'] sign? number) )

(* For float vectors *)
let float_vec_body =
  "[" h_whitespace?
      float_number_scientific
      (h_whitespace* "," h_whitespace* float_number_scientific)*
     h_whitespace? "]"

let int_vec_body =
  "[" h_whitespace?
      signed_int
      (h_whitespace* "," h_whitespace* signed_int)*
      h_whitespace?
  "]"

let row_int =
  "[" h_whitespace?
      signed_int
      (h_whitespace* "," h_whitespace* signed_int)*
      h_whitespace?
  "]"

let row_float =
  "[" h_whitespace?
      float_number_scientific
      (h_whitespace* "," h_whitespace* float_number_scientific)*
      h_whitespace?
   "]"

let int_mat_body =
  "[" h_whitespace?
      row_int
      (h_whitespace* "," h_whitespace* row_int)*
      h_whitespace?
   "]"

let float_mat_body =
  "[" h_whitespace?
      row_float
      (h_whitespace* "," h_whitespace* row_float)*
      h_whitespace?
   "]"

(* ----------------------------------------------------------------------- *)
(* ORIGINAL CODE + NEW KEYWORDS: Main lexer rules                          *)
(* ----------------------------------------------------------------------- *)

rule token = parse
  | "//*" { comment lexbuf }
  | whitespace { token lexbuf }

  (* Keywords *)
  | "for"        { FOR }
  | "while"      { WHILE }
  | "int"        { INT }
  | "bool"       { BOOL }
  | "float"      { FLOAT }
  | "abs"        { ABS }
  | "vector"     { VECTOR }
  | "matrix"     { MATRIX }
  | "input"      { INPUT }
  | "print"      { PRINT }

  (* NEW keywords *)
  | "angle"      { ANGLE }
  | "magnitude"  { MAGNITUDE }
  | "dimension"  { DIMENSION }
  | "transpose"  { TRANSPOSE }
  | "determinant"{ DETERMINANT }
  | "power"      { POWER }
  | "plusV"       { PLUSV }
  | "minusV"      { MINUSV }
  | "plusM"       { PLUSM }
  | "minusM"      { MINUSM }
  | "multiplyM"   { MULTIPLYM }
  | "dot_product"{ DOT_PRODUCT }
  | "scalar_multiply" { SCALAR_MULTIPLY }

  (* Comma *)
  | ","           { COMMA }

  (* Vector float: dimension + float_vec_body *)
  | (sign number as dim_str) whitespace float_vec_body as lexeme {
      let dim = int_of_string dim_str in
      let index = try String.index lexeme '[' with Not_found ->
        raise (Lexing_error ("Missing '[' in vector literal: " ^ lexeme))
      in
      let vec_part = String.sub lexeme index (String.length lexeme - index) in
      let float_list = parse_float_vector vec_part in
      if List.length float_list <> dim then
         raise (Lexing_error (Printf.sprintf "Vector dimension mismatch: expected %d, got %d" dim (List.length float_list)))
      else VECTOR_FLOAT (dim, float_list)
    }

  (* Vector int: dimension + int_vec_body *)
  | (sign number as dim_str) whitespace int_vec_body as lexeme {
      let dim = int_of_string dim_str in
      let index = try String.index lexeme '[' with Not_found ->
        raise (Lexing_error ("Missing '[' in vector literal: " ^ lexeme))
      in
      let vec_part = String.sub lexeme index (String.length lexeme - index) in
      let int_list = parse_int_vector vec_part in
      if List.length int_list <> dim then
         raise (Lexing_error (Printf.sprintf "Vector dimension mismatch: expected %d, got %d" dim (List.length int_list)))
      else VECTOR_INT (dim, int_list)
    }

  (* Matrix float: rows, cols, float_mat_body *)
  | (sign number as r_str) whitespace? "," whitespace?
    (sign number as c_str) whitespace? float_mat_body as lexeme {
      let rows = int_of_string r_str in
      let cols = int_of_string c_str in
      let index = try String.index lexeme '[' with Not_found ->
        raise (Lexing_error ("Missing '[' in matrix literal: " ^ lexeme))
      in
      let mat_part = String.sub lexeme index (String.length lexeme - index) in
      let float_matrix = parse_float_matrix mat_part in
      if List.length float_matrix <> rows then
         raise (Lexing_error (Printf.sprintf "Matrix row count mismatch: expected %d, got %d" rows (List.length float_matrix)))
      else if List.exists (fun row -> List.length row <> cols) float_matrix then
         raise (Lexing_error (Printf.sprintf "Matrix column count mismatch: expected %d" cols))
      else MATRIX_FLOAT ((rows, cols), float_matrix)
    }

  (* Matrix int: rows, cols, int_mat_body *)
  | (sign number as r_str) whitespace? "," whitespace?
    (sign number as c_str) whitespace? int_mat_body as lexeme {
      let rows = int_of_string r_str in
      let cols = int_of_string c_str in
      let index = try String.index lexeme '[' with Not_found ->
        raise (Lexing_error ("Missing '[' in matrix literal: " ^ lexeme))
      in
      let mat_part = String.sub lexeme index (String.length lexeme - index) in
      let int_matrix = parse_int_matrix mat_part in
      if List.length int_matrix <> rows then
         raise (Lexing_error (Printf.sprintf "Matrix row count mismatch: expected %d, got %d" rows (List.length int_matrix)))
      else if List.exists (fun row -> List.length row <> cols) int_matrix then
         raise (Lexing_error (Printf.sprintf "Matrix column count mismatch: expected %d" cols))
      else MATRIX_INT ((rows, cols), int_matrix)
    }

  (* Stand-alone float literals (with optional exponent) *)
  |  number "." number ((['e' 'E'] sign? number))? as f {
      FLOAT_LITERAL (float_of_string f)
    }
  |  number (['e' 'E'] sign? number) as f {
      FLOAT_LITERAL (float_of_string f)
    }

  (* Numeric literal immediately followed by letter/underscore -> error *)
  |  number (['a'-'z' 'A'-'Z' '_']+ ) as invalid {
      raise (Lexing_error (Printf.sprintf "Invalid numeric literal: %s" invalid))
    }

  (* Integer literal *)
  |  number as num {
      INT_LITERAL (int_of_string num)
    }

  (* Boolean literals *)
  | "true"       { BOOL_LITERAL true }
  | "false"      { BOOL_LITERAL false }

  (* Control-flow keywords *)
  | "if"         { IF }
  | "then"       { THEN }
  | "else"       { ELSE }

  (* Boolean ops *)
  | "not"        { NOT }
  | "&&"         { AND }
  | "||"         { OR }

  (* Comparison ops *)
  | "=="         { EQUAL }
  | ">="         { GTE }
  | "<="         { LTE }
  | ">"          { GT }
  | "<"          { LT }

  (* Finally, the single "." => DOT.
     IMPORTANT: must come AFTER the float-literal rules. *)

  (* Identifiers *)
  | ident_regex as id { IDENT id }

  (* Arithmetic ops *)
  | "+"          { PLUS }
  | "-"          { MINUS }
  | "*"          { MULTIPLY }
  | "/"          { DIVIDE }
  | "%"          { MOD }



  (* Assignment *)
  | ":="         { ASSIGN }

  (* Delimiters *)
  | "("          { LPAREN }
  | ")"          { RPAREN }
  | ";"          { SEMICOLON }
  | "["          { LBRACKET }
  | "]"          { RBRACKET }
  | "{"          { LBRACE }
  | "}"          { RBRACE }

  (* End of file *)
  | eof          { EOF }


  | "\"" [^'"']* "\"" as str {
      let content = String.sub str 1 (String.length str - 2) in
      STRING_LITERAL content
    }

  (* Unknown char *)
  | _ as c {
      raise (Lexing_error (Printf.sprintf "Unexpected character: %c" c))
    }

and comment = parse
  | "*//" { token lexbuf }
  | _ { comment lexbuf }
  | eof { raise (Lexing_error "Unterminated comment") }
