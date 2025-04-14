open Z3

let ctx = mk_context []
let verbose = false

(* Debugging *)
let debug msg =
  if verbose then Printf.printf "%s\n" msg

(* Read JSON input and deserialize into term or formula *)
let read_input json_file =
  debug (Printf.sprintf "Reading input from %s" json_file);
  let json = Yojson.Safe.from_file json_file in
  debug (Printf.sprintf "Parsed JSON: %s" (Yojson.Safe.to_string json));
  match Ltlxms.term_of_yojson json with
  | Ok term -> `Term term
  | Error _ -> (
      match Ltlxms.formula_of_yojson json with
      | Ok formula -> `Formula formula
      | Error err -> failwith (Printf.sprintf "Failed to parse input: %s" err)
    )

(* Read expected output *)
let read_expected_output output_file =
  debug (Printf.sprintf "Reading expected output from %s" output_file);
  let ic = open_in output_file in
  let rec read_all_lines acc =
    try
      let line = input_line ic in
      read_all_lines (acc ^ line ^ "\n")
    with End_of_file -> acc
  in
  let content = read_all_lines "" in
  close_in ic;
  debug (Printf.sprintf "Expected output: %s" content);
  content
(*Transformar em variavel z3*)
(* Convert a term into a Z3 expression *)
let rec term_to_z3 ctx term =
  match term with
  | Ltlxms.Proposition p -> Z3.Boolean.mk_const ctx (Z3.Symbol.mk_string ctx p)
  | Ltlxms.Intersection (t1, t2) -> Formulas.Logic.single_and ctx (term_to_z3 ctx t1) (term_to_z3 ctx t2)
  | Ltlxms.Union (t1, t2) -> Formulas.Logic.single_or ctx (term_to_z3 ctx t1) (term_to_z3 ctx t2)
  | Ltlxms.Expand (t, value) -> 
      let expand_symbol = Z3.Symbol.mk_string ctx "expand" in
      let expand_func = Z3.FuncDecl.mk_func_decl ctx expand_symbol [Z3.Boolean.mk_sort ctx; Z3.Arithmetic.Real.mk_sort ctx] (Z3.Boolean.mk_sort ctx) in
      Z3.Expr.mk_app ctx expand_func [term_to_z3 ctx t; Z3.Arithmetic.Real.mk_numeral_s ctx (string_of_float value)]
  | Ltlxms.NextT t ->
    let next_symbol = Z3.Symbol.mk_string ctx "nextt" in
    let next_func = Z3.FuncDecl.mk_func_decl ctx next_symbol [Z3.Boolean.mk_sort ctx] (Z3.Boolean.mk_sort ctx) in
    Z3.Expr.mk_app ctx next_func [term_to_z3 ctx t]
  | _ -> failwith "Unsupported term type for conversion to Z3"

  (* Convert a formula into a Z3 expression *)
let rec formula_to_z3 ctx formula =
  match formula with
  | Ltlxms.SubSetEq (t1, t2) -> Formulas.Logic.sub_set_eq ctx (term_to_z3 ctx t1) (term_to_z3 ctx t2)
  | Ltlxms.And (f1, f2) -> Formulas.Logic.single_and ctx (formula_to_z3 ctx f1) (formula_to_z3 ctx f2)
  | Ltlxms.Or (f1, f2) -> Formulas.Logic.single_or ctx (formula_to_z3 ctx f1) (formula_to_z3 ctx f2)
  | Ltlxms.Not f -> Formulas.Logic.single_not ctx (formula_to_z3 ctx f)
  | Ltlxms.Next f -> 
      let next_symbol = Z3.Symbol.mk_string ctx "next" in
      let next_func = Z3.FuncDecl.mk_func_decl ctx next_symbol [Z3.Boolean.mk_sort ctx] (Z3.Boolean.mk_sort ctx) in
      Z3.Expr.mk_app ctx next_func [formula_to_z3 ctx f]
  | Ltlxms.Always f -> 
      let always_symbol = Z3.Symbol.mk_string ctx "always" in
      let always_func = Z3.FuncDecl.mk_func_decl ctx always_symbol [Z3.Boolean.mk_sort ctx] (Z3.Boolean.mk_sort ctx) in
      Z3.Expr.mk_app ctx always_func [formula_to_z3 ctx f]
  | Ltlxms.Eventually f ->
      let eventually_symbol = Z3.Symbol.mk_string ctx "eventually" in
      let eventually_func = Z3.FuncDecl.mk_func_decl ctx eventually_symbol [Z3.Boolean.mk_sort ctx] (Z3.Boolean.mk_sort ctx) in
      Z3.Expr.mk_app ctx eventually_func [formula_to_z3 ctx f]
  | Ltlxms.Once f ->
      let once_symbol = Z3.Symbol.mk_string ctx "once" in
      let once_func = Z3.FuncDecl.mk_func_decl ctx once_symbol [Z3.Boolean.mk_sort ctx] (Z3.Boolean.mk_sort ctx) in
      Z3.Expr.mk_app ctx once_func [formula_to_z3 ctx f]
  | Ltlxms.Overlap (t1, t2) ->
      let overlap_symbol = Z3.Symbol.mk_string ctx "overlap" in
      let overlap_func = Z3.FuncDecl.mk_func_decl ctx overlap_symbol [Z3.Boolean.mk_sort ctx; Z3.Boolean.mk_sort ctx] (Z3.Boolean.mk_sort ctx) in
      Z3.Expr.mk_app ctx overlap_func [term_to_z3 ctx t1; term_to_z3 ctx t2]
  | Ltlxms.Implies (f1, f2) -> Formulas.Logic.single_implies ctx (formula_to_z3 ctx f1) (formula_to_z3 ctx f2)
  | Ltlxms.Previous f ->
    let previous_symbol = Z3.Symbol.mk_string ctx "previous" in
    let previous_func = Z3.FuncDecl.mk_func_decl ctx previous_symbol [Z3.Boolean.mk_sort ctx] (Z3.Boolean.mk_sort ctx) in
    Z3.Expr.mk_app ctx previous_func [formula_to_z3 ctx f]
  | _ -> failwith "Unsupported formula type for conversion to Z3"

(* Perform operation and compare with expected output *)
let test_operation input_file output_file =
  debug "Starting operation test";
  let input = read_input input_file in
  let expected_output = read_expected_output output_file in
  debug "Transforming input into Z3 expression";

  (* Convert the input into a Z3 expression *)
  let actual_output_expr =
    match input with
    | `Term term -> term_to_z3 ctx term
    | `Formula formula -> formula_to_z3 ctx formula
  in

  (* Convert the Z3 expression to a string without simplification *)
  let actual_output = Z3.Expr.to_string actual_output_expr in
  debug (Printf.sprintf "Actual output: %s" actual_output);

  (* Compare actual output with expected output *)
  let actual_output = String.trim actual_output in
  let expected_output = String.trim expected_output in
  if actual_output = expected_output then
    Printf.printf "Test passed for %s!\n" input_file
  else (
    Printf.printf "Test failed for %s!\n" input_file;
    Printf.printf "Expected: %s\n" expected_output;
    Printf.printf "Actual: %s\n" actual_output
  )

(* Run all tests *)
let () =
  let files = Sys.readdir "." in
  Array.iter (fun file ->
    if Filename.check_suffix file ".json" then
      let input_file = file in
      let output_file = Filename.chop_suffix file ".json" ^ "out.txt" in
      if Sys.file_exists output_file then (
        debug (Printf.sprintf "Running test with input file: %s and output file: %s" input_file output_file);
        test_operation input_file output_file
      ) else
        Printf.printf "Output file %s not found for input file %s\n" output_file input_file
  ) files