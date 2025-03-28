open Z3
open Formulas.Logic 

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
  | Ok term -> term
  | Error err -> failwith (Printf.sprintf "Failed to parse term: %s" err)

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
  | Ltlxms.Intersection (t1, t2) -> single_and ctx (term_to_z3 ctx t1) (term_to_z3 ctx t2)
  | Ltlxms.Union (t1, t2) -> single_or ctx (term_to_z3 ctx t1) (term_to_z3 ctx t2)
  | _ -> failwith "Unsupported term type for conversion to Z3"

(* Perform operation and compare with expected output *)
let test_operation input_file output_file =
  debug "Starting operation test";
  let term = read_input input_file in
  let expected_output = read_expected_output output_file in
  debug "Transforming term into Z3 expression";

  (* Convert the term into a Z3 expression *)
  let actual_output_expr = term_to_z3 ctx term in
  let actual_output = Z3.Expr.to_string actual_output_expr in
  debug (Printf.sprintf "Actual output: %s" actual_output);

  (* Compare actual output with expected output *)
  if actual_output = expected_output then
    print_endline "Test passed!"
  else (
    print_endline "Test failed!";
    Printf.printf "Expected: %s\n" expected_output;
    Printf.printf "Actual: %s\n" actual_output
  )

let () =
  let input_file = "test_input.json" in
  let output_file = "test_output.txt" in
  debug (Printf.sprintf "Running test with input file: %s and output file: %s" input_file output_file);
  test_operation input_file output_file