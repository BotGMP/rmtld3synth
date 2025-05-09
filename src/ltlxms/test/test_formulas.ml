open Z3
open Formulas 
(* Initialize Z3 context *)
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

(* Perform operation and compare with expected output *)
let test_operation input_file output_file =
  debug "Starting operation test";
  let input = read_input input_file in
  let expected_output = read_expected_output output_file in
  debug "Transforming input into Z3 expression";

  (* Convert the input into a Z3 expression *)
  let actual_output_expr =
    match input with
    | `Term term -> Formulas.term_to_z3 ctx term
    | `Formula formula -> Formulas.formula_to_z3 ctx formula
  in

  (* Convert the Z3 expression to a string *)
  let actual_output = String.trim (Z3.Expr.to_string actual_output_expr) in
  let expected_output = String.trim expected_output in

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