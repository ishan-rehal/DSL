# COL_226_Interpretor

This repository contains an interpreter project that demonstrates parsing, type-checking, abstract syntax tree (AST) construction, and interpretation for a small custom language. Below is a brief explanation of each file and detailed instructions on how to build and run the project.

---

## Files

1. **ast.ml**  
   - Defines the abstract syntax tree (AST) data structures that represent the program’s constructs.
   - Provides helper functions or types to manipulate AST nodes when parsing or interpreting the language.

2. **parser.mly**  
   - Implements the grammar of the language using OCaml’s Menhir parser generator.
   - Translates the source code into an AST by matching tokens to grammar rules.

3. **typecheck.ml**  
   - Contains functions to perform static type-checking on the AST.
   - Ensures that the program is well-typed before execution, detecting type errors and providing relevant messages.

4. **interpretor.ml**  
   - Implements the core logic to interpret (or evaluate) the AST.
   - Traverses the AST at runtime, executing expressions and statements to produce results.

5. **main.ml**  
   - Serves as the entry point of the entire application.
   - Orchestrates parsing, type-checking, and interpretation, tying all components together into a complete program flow.

6. **Makefile**  
   - Automates the build and run process.
   - Contains rules for compiling the project (`make`) and running it (`make run`).

---

## How to Build and Run

1. **Ensure Dependencies**  
   - Install [OCaml](https://ocaml.org/) on your system.  
   - Install [Menhir](https://gitlab.inria.fr/fpottier/menhir) (an OCaml parser generator).  
   - Verify your installation by running `ocamlc -v` and `menhir --version`.

2. **Clone or Download the Repository**  
   - Navigate to the directory where you want the project and run:  
     ```bash
     git clone <repository_url>
     ```
   - Replace `<repository_url>` with the actual link to this repository.  
   - Move into the newly created directory:
     ```bash
     cd COL_226_Interpretor
     ```

3. **Compile the Project**  
   - From the project’s root directory, run:
     ```bash
     make
     ```
   - What happens under the hood:
     - Menhir processes `parser.mly`.
     - All `.ml` files (`ast.ml`, `typecheck.ml`, `interpretor.ml`, `main.ml`, etc.) are compiled in the correct order.
     - The resulting compiled executable (often named `main`) is generated in the project folder.

4. **Run the Interpreter**  
   - Once compilation is successful, you can run:
     ```bash
     make run
     ```
   - This command does the following:
     - Executes the compiled binary (e.g., `./main`).
     - Captures the main output in `output.txt`.
     - Prints log messages or any debug statements to your terminal window.
     
   - **Viewing the Results**:
     - Check the `output.txt` file to see the primary output from the execution.
     - Any print statements, such as debug logs or type errors, will appear in the terminal.

5. **Providing Custom Input (If Applicable)**  
   - If your interpreter requires an input file or expects user interaction, you can:
     - Modify the `Makefile` “run” target to pass a file to `./main`, or
     - Directly execute `./main < input_file` to feed input from a file.
   - Adjust these commands to fit your language and program’s requirements.

6. **Cleaning Up**  
   - If needed, remove all compiled files (e.g., `.cmi`, `.cmx`, executables) by running:
     ```bash
     make clean
     ```
   - This keeps your repository clean and ready for new builds.

---

## Summary

By following the above steps, you will be able to:

1. Install all dependencies.
2. Compile the project into an executable.
3. Run the interpreter to parse, type-check, and evaluate your program code.
4. Inspect the terminal for logs and `output.txt` for the main output.

Feel free to explore or modify the source files to experiment with different features of this custom language. Recompile with `make` and rerun with `make run` to observe changes in behavior.
