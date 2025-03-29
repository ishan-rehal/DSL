# Makefile for building and running the OCaml parser project

# OCaml compiler command
OCAMLC = ocamlc

# Executable target name
TARGET = my_parser

# Source files
SOURCES = ast.ml typecheck.ml parser.mly lexer.mll main.ml

# Generated files
GEN_PARSER_ML = parser.ml
GEN_PARSER_MLI = parser.mli
GEN_LEXER_ML  = lexer.ml

# Object files
OBJS = ast.cmo typecheck.cmo parser.cmo lexer.cmo main.cmo

all: $(TARGET)

# Build the final executable by linking object files with str.cma library
$(TARGET): $(OBJS)
	$(OCAMLC) -o $(TARGET) -I +str str.cma $(OBJS)

# Compile each OCaml module.
ast.cmo: ast.ml
	$(OCAMLC) -c ast.ml

typecheck.cmo: typecheck.ml ast.cmo
	$(OCAMLC) -c typecheck.ml

# Compile parser.mli to generate parser.cmi.
parser.cmi: $(GEN_PARSER_MLI)
	$(OCAMLC) -c $(GEN_PARSER_MLI)

# Compile parser.ml (depends on parser.cmi).
parser.cmo: $(GEN_PARSER_ML) parser.cmi
	$(OCAMLC) -c $(GEN_PARSER_ML)

lexer.cmo: $(GEN_LEXER_ML)
	$(OCAMLC) -I +str -c $(GEN_LEXER_ML)

main.cmo: main.ml ast.cmo typecheck.cmo
	$(OCAMLC) -c main.ml

# Generate parser.ml and parser.mli from parser.mly using ocamlyacc.
$(GEN_PARSER_ML) $(GEN_PARSER_MLI): parser.mly
	ocamlyacc parser.mly

# Generate lexer.ml from lexer.mll using ocamllex.
$(GEN_LEXER_ML): lexer.mll
	ocamllex lexer.mll

# Clean up generated and compiled files.
clean:
	rm -f *.cmo *.cmi $(TARGET) $(GEN_PARSER_ML) $(GEN_PARSER_MLI) $(GEN_LEXER_ML)

# Run the parser executable.
run: $(TARGET)
	./$(TARGET)