open Z3

let ctx = mk_context []
let verbose = true

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
  let line = input_line ic in
  close_in ic;
  debug (Printf.sprintf "Expected output: %s" line);
  line

(* Convert a term into a Z3 expression *)
let rec term_to_z3 ctx term =
  match term with
  | Ltlxms.Proposition p -> Z3.Boolean.mk_const ctx (Z3.Symbol.mk_string ctx p)
  | Ltlxms.Intersection (t1, t2) -> Formulas.Logic.single_and ctx (term_to_z3 ctx t1) (term_to_z3 ctx t2)
  | Ltlxms.Union (t1, t2) -> Formulas.Logic.single_or ctx (term_to_z3 ctx t1) (term_to_z3 ctx t2)
  | _ -> failwith "Unsupported term type for conversion to Z3"

(* Convert a formula into a Z3 expression *)
(* Convert a formula into a Z3 expression *)
let rec formula_to_z3 ctx formula =
  match formula with
  | Ltlxms.SubSetEq (t1, t2) -> Formulas.Logic.sub_set_eq ctx (term_to_z3 ctx t1) (term_to_z3 ctx t2)
  | Ltlxms.And (f1, f2) -> Formulas.Logic.single_and ctx (formula_to_z3 ctx f1) (formula_to_z3 ctx f2)
  | Ltlxms.Or (f1, f2) -> Formulas.Logic.single_or ctx (formula_to_z3 ctx f1) (formula_to_z3 ctx f2)
  | Ltlxms.Not f -> Formulas.Logic.single_not ctx (formula_to_z3 ctx f)
  | Ltlxms.Next f -> 
      (* Handle the Next constructor, assuming 1 step *)
      let next_symbol = Z3.Symbol.mk_string ctx "next" in
      let next_func = Z3.FuncDecl.mk_func_decl ctx next_symbol [Z3.Boolean.mk_sort ctx] (Z3.Boolean.mk_sort ctx) in
      Z3.Expr.mk_app ctx next_func [formula_to_z3 ctx f]
  | Ltlxms.Implies (f1, f2) -> Formulas.Logic.single_implies ctx (formula_to_z3 ctx f1) (formula_to_z3 ctx f2)
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
  let actual_output = Z3.Expr.to_string actual_output_expr in
  debug (Printf.sprintf "Actual output: %s" actual_output);

  (* Compare actual output with expected output *)
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