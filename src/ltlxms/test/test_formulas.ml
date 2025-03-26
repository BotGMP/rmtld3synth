open Z3
open Yojson.Basic.Util
open Formulas.Logic

let ctx = mk_context []
let verbose = false

(* Debugging helper function *)
let debug msg =
  if verbose then Printf.printf "%s\n" msg

(* Function to read JSON input *)
let read_input json_file =
  debug (Printf.sprintf "Reading input from %s" json_file);
  let json = Yojson.Basic.from_file json_file in
  let expr1 = 
    if json |> member "expr1" |> member "type" |> to_string = "true" then 
      Boolean.mk_true ctx 
    else 
      Boolean.mk_false ctx 
  in
  let expr2 = 
    if json |> member "expr2" |> member "type" |> to_string = "true" then 
      Boolean.mk_true ctx 
    else 
      Boolean.mk_false ctx 
  in
  debug (Printf.sprintf "Parsed expr1: %s" (Expr.to_string expr1));
  debug (Printf.sprintf "Parsed expr2: %s" (Expr.to_string expr2));
  (expr1, expr2)

(* Function to read expected output *)
let read_expected_output output_file =
  debug (Printf.sprintf "Reading expected output from %s" output_file);
  let ic = open_in output_file in
  let line = input_line ic in
  close_in ic;
  debug (Printf.sprintf "Expected output: %s" line);
  line

(* Function to perform OR operation and compare with expected output *)
let test_or_operation input_file output_file =
  debug "Starting OR operation test";
  let (expr1, expr2) = read_input input_file in
  let expected_output = read_expected_output output_file in

  (* Perform OR operation using Logic.single_or *)
  debug "Performing OR operation on expr1 and expr2";
  let or_result = single_or ctx expr1 expr2 in
  let simplified_result = Expr.simplify or_result None in
  let actual_output = 
    if Expr.equal simplified_result (Boolean.mk_true ctx) then "true"
    else if Expr.equal simplified_result (Boolean.mk_false ctx) then "false"
    else "unknown"
  in
  debug (Printf.sprintf "OR operation result: %s" (Expr.to_string or_result));
  debug (Printf.sprintf "Simplified result: %s" (Expr.to_string simplified_result));
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
  test_or_operation input_file output_file