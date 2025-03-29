%{
  open Ast
%}
%right UMINUS UPLUS NOT

(* Token declarations; ensure these match your lexer. *)
%token <int>        INT_LITERAL
%token <float>      FLOAT_LITERAL
%token              INT
%token              FLOAT
%token              BOOL
%token              VECTOR
%token              MATRIX
%token              ABS
%token              FOR
%token              WHILE
%token               INPUT
%token               PRINT
%token <string>     STRING_LITERAL
%token <string>     IDENT
%token <bool>       BOOL_LITERAL
%token              PLUS
%token              MINUS
%token              MULTIPLY
%token              DIVIDE
%token              MOD
%token              ASSIGN
%token              LPAREN
%token              RPAREN
%token              SEMICOLON
%token              LBRACKET
%token              RBRACKET
%token              LBRACE
%token              RBRACE
%token              IF
%token              THEN
%token              ELSE
%token              NOT
%token              AND
%token              OR
%token              EQUAL
%token              GT
%token              GTE
%token              LT
%token              LTE
%token <int * int list>            VECTOR_INT
%token <int * float list>          VECTOR_FLOAT
%token <(int * int) * int list list>   MATRIX_INT
%token <(int * int) * float list list> MATRIX_FLOAT
%token              EOF
%token              ANGLE
%token              MAGNITUDE
%token              DIMENSION
%token              TRANSPOSE
%token              DETERMINANT
%token              POWER
(* New tokens *)
%token              DOT_PRODUCT
%token              SCALAR_MULTIPLY
%token              PLUSV
%token              MINUSV
%token              MULTIPLYV
%token              PLUSM
%token              MINUSM
%token              MULTIPLYM
%token              COMMA

%start program
%type <Ast.program> program

%%

(* Top-level: a program is a list of statements (with an optional trailing semicolon) *)
program:
    statement_list EOF              { Program($1) }
;

statement_list:
    | terminated_statement          { [$1] }
    | statement_list terminated_statement { $1 @ [$2] }
;

terminated_statement:
    | statement SEMICOLON           { $1 }
;

(* Special rule for for-loop initialization *)
for_init:
    | INT IDENT ASSIGN expr         { DeclareInt($2, $4) }
    | IDENT ASSIGN expr             { Assign($1, $3) }
;

(* Special rule for for-loop update *)
for_update:
    | IDENT ASSIGN expr             { Assign($1, $3) }
;

(* Statements: declarations, assignment, input/print, expression statements, and blocks *)
statement:
    | INT IDENT ASSIGN expr         { DeclareInt($2, $4) }
    | FLOAT IDENT ASSIGN expr       { DeclareFloat($2, $4) }
    | BOOL IDENT ASSIGN expr        { DeclareBool($2, $4) }
    | VECTOR INT IDENT ASSIGN expr  { DeclareVectorInt($3, $5) }
    | VECTOR FLOAT IDENT ASSIGN expr { DeclareVectorFloat($3, $5) }
    | MATRIX INT IDENT ASSIGN expr  { DeclareMatrixInt($3, $5) }
    | MATRIX FLOAT IDENT ASSIGN expr { DeclareMatrixFloat($3, $5) }
    | IDENT ASSIGN expr             { Assign($1, $3) }
    | IDENT LBRACKET expr RBRACKET ASSIGN expr  
                                    { AssignVector($1, $3, $6) }
    | IDENT LBRACKET expr RBRACKET LBRACKET expr RBRACKET ASSIGN expr  
                                    { AssignMatrix($1, $3, $6, $9) }
    | INPUT LPAREN optional_string RPAREN { InputStmt($3) }
    | PRINT LPAREN optional_ident RPAREN { PrintStmt($3) }
    | expr                          { ExprStmt($1) }
    | statement_block               { Block($1) }
;

optional_string:
    | STRING_LITERAL                { $1 }
    | /* empty */                   { "" }
;

optional_ident:
    | IDENT                         { Some($1) }
    | /* empty */                   { None }
;

statement_block:
    | LBRACE statement_list RBRACE  { $2 }
    | LBRACE RBRACE                 { [] }
;

(* Block that can be used in expressions (for if/while/for bodies) *)
expression_block:
    | LBRACE statement_list RBRACE  { BlockExp($2) }
    | LBRACE RBRACE                 { BlockExp([]) }
;

(* Expression grammar by precedence, from highest to lowest *)
expr:
    control_flow_expr               { $1 }
;

(* Control flow expressions (if, while, for) *)
control_flow_expr:
    | IF expr THEN expression_block ELSE expression_block 
                                    { If($2, $4, $6) }
    | WHILE expr expression_block   { While($2, $3) }
    | FOR LPAREN for_init SEMICOLON expr SEMICOLON for_update RPAREN expression_block
                                    { For($3, $5, $7, $9) }
    | logical_or_expr               { $1 }
;

(* Logical OR expressions *)
logical_or_expr:
    | logical_or_expr OR logical_and_expr  
                                    { Or($1, $3) }
    | logical_and_expr              { $1 }
;

(* Logical AND expressions *)
logical_and_expr:
    | logical_and_expr AND comparison_expr  
                                    { And($1, $3) }
    | comparison_expr               { $1 }
;

(* Comparison/relational expressions *)
comparison_expr:
    | comparison_expr EQUAL additive_expr  
                                    { Eq($1, $3) }
    | comparison_expr GT additive_expr     
                                    { Greater($1, $3) }
    | comparison_expr GTE additive_expr    
                                    { GreaterEq($1, $3) }
    | comparison_expr LT additive_expr     
                                    { Less($1, $3) }
    | comparison_expr LTE additive_expr    
                                    { LessEq($1, $3) }
    | additive_expr                 { $1 }
;

(* Addition/subtraction expressions *)
additive_expr:
    | additive_expr PLUS multiplicative_expr  
                                    { Add($1, $3) }
    | additive_expr MINUS multiplicative_expr 
                                    { Sub($1, $3) }
    | multiplicative_expr           { $1 }
;

(* Multiplication/division/modulo expressions *)
multiplicative_expr:
    | multiplicative_expr MULTIPLY unary_expr  
                                    { Mul($1, $3) }
    | multiplicative_expr DIVIDE unary_expr    
                                    { Div($1, $3) }
    | multiplicative_expr MOD unary_expr       
                                    { Mod($1, $3) }
    | unary_expr                    { $1 }
;

(* Unary expressions (negation, not) *)
unary_expr:
    | MINUS unary_expr %prec UMINUS { Neg($2) }
    | PLUS unary_expr  %prec UPLUS  { Pos($2) }
    | NOT unary_expr                { Not($2) }
    | primary_expr                  { $1 }
;

(* Primary expressions (literals, variables, function calls, etc.) *)
primary_expr:
    | IDENT LBRACKET expr RBRACKET  
                                    { VectorAcess($1, $3) }
    | IDENT LBRACKET expr RBRACKET LBRACKET expr RBRACKET 
                                    { MatrixAcess($1, $3, $6) }
    | POWER LPAREN expr COMMA expr RPAREN 
                                    { Power($3, $5) }
    (* Vector and matrix arithmetic functions *)
    | DOT_PRODUCT LPAREN expr COMMA expr RPAREN 
                                    { DotProduct($3, $5) }
    | SCALAR_MULTIPLY LPAREN expr COMMA expr RPAREN 
                                    { ScalarMultiply($3, $5) }
    | PLUSV LPAREN expr COMMA expr RPAREN 
                                    { PlusV($3, $5) }
    | MINUSV LPAREN expr COMMA expr RPAREN 
                                    { MinusV($3, $5) }
    | PLUSM LPAREN expr COMMA expr RPAREN 
                                    { PlusM($3, $5) }
    | MINUSM LPAREN expr COMMA expr RPAREN 
                                    { MinusM($3, $5) }
    | MULTIPLYM LPAREN expr COMMA expr RPAREN 
                                    { MultiplyM($3, $5) }
    (* Vector/matrix operations *)
    | ANGLE LPAREN expr COMMA expr RPAREN      { Angle($3, $5) }
    | MAGNITUDE LPAREN expr RPAREN  { Magnitude($3) }
    | DIMENSION LPAREN expr RPAREN  { Dimension($3) }
    | TRANSPOSE LPAREN expr RPAREN  { Transpose($3) }
    | DETERMINANT LPAREN expr RPAREN 
                                    { Determinant($3) }
    (* Literals and variables *)
    | INT_LITERAL                   { Int($1) }
    | FLOAT_LITERAL                 { Float($1) }
    | BOOL_LITERAL                  { Bool($1) }
    | IDENT                         { Var($1) }
    | VECTOR_INT                    { VectorInt($1) }
    | VECTOR_FLOAT                  { VectorFloat($1) }
    | MATRIX_INT                    { MatrixInt($1) }
    | MATRIX_FLOAT                  { MatrixFloat($1) }
    | ABS primary_expr              { Abs($2) }
    | LPAREN expr RPAREN            { $2 }
;