# Makefile for building and running the OCaml parser project

# OCaml compiler command
OCAMLC = ocamlc

# Executable target name
TARGET = my_parser

# Source files
SOURCES = ast.ml typecheck.ml parser.mly lexer.mll main.ml interpretor.ml

# Generated files
GEN_PARSER_ML = parser.ml
GEN_PARSER_MLI = parser.mli
GEN_LEXER_ML  = lexer.ml

# Object files
OBJS = ast.cmo parser.cmo lexer.cmo typecheck.cmo interpretor.cmo main.cmo

all: $(TARGET)

# Build the final executable by linking object files with str.cma library
$(TARGET): $(OBJS)
	$(OCAMLC) -o $(TARGET) -I +str str.cma $(OBJS)

# Compile each OCaml module.
ast.cmo: ast.ml
	$(OCAMLC) -c ast.ml

typecheck.cmo: typecheck.ml ast.cmo parser.cmi lexer.cmo
	$(OCAMLC) -c typecheck.ml

# Compile parser.mli to generate parser.cmi.
parser.cmi: $(GEN_PARSER_MLI) ast.cmo
	$(OCAMLC) -c $(GEN_PARSER_MLI)

# Compile parser.ml (depends on parser.cmi and ast.cmo).
parser.cmo: $(GEN_PARSER_ML) parser.cmi ast.cmo
	$(OCAMLC) -c $(GEN_PARSER_ML)

# Lexer depends on parser.cmi for token types
lexer.cmo: $(GEN_LEXER_ML) parser.cmi
	$(OCAMLC) -I +str -c $(GEN_LEXER_ML)

# Interpretor depends on all the modules it opens
interpretor.cmo: interpretor.ml ast.cmo typecheck.cmo parser.cmo lexer.cmo
	$(OCAMLC) -c interpretor.ml

# Main depends on all other modules
main.cmo: main.ml ast.cmo typecheck.cmo interpretor.cmo parser.cmo lexer.cmo
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
