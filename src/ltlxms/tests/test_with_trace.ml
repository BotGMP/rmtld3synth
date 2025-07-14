(* Running Instructions: 
 * 1. Run "dune build" to compile
 * 2. Navigate to the project root directory
 * 3. Run "dune exec
   src/ltlxms/test/test_with_trace.exe src/trace/test/scenario5.json
   src/ltlxms/test/property.json" *)

open Z3
open Z3.Arithmetic
open Sequence.Snapshot
open Ltlxms.Syntax
open Ltlxms.Encoding

(* Global debug flag *)
let debug_mode = false

let create_z3_context () =
  let cfg = [("model", "true"); ("proof", "false")] in
  Z3.mk_context cfg

(* Helper to get all car Ids from the trace *)
let get_all_car_ids (parsed_trace : Sequence.Snapshot.trace) : int list =
  parsed_trace.trace
  |> List.concat_map (fun event -> List.map (fun e -> e.id) event.elements)
  |> List.sort_uniq compare

let run_ltlxms_trace_check ~trace_file ~property_file =
  let ctx = create_z3_context () in
  (* Create shared function declarations for temporal operators *)
  let bool_sort = Boolean.mk_sort ctx in
  let shared_next_decl =
    FuncDecl.mk_func_decl_s ctx "next" [bool_sort] bool_sort
  in
  let shared_nextt_decl =
    FuncDecl.mk_func_decl_s ctx "nextt" [bool_sort] bool_sort
  in
  let shared_previous_decl =
    FuncDecl.mk_func_decl_s ctx "previous" [bool_sort] bool_sort
  in
  let shared_once_decl =
    FuncDecl.mk_func_decl_s ctx "once" [bool_sort] bool_sort
  in
  let shared_always_decl =
    FuncDecl.mk_func_decl_s ctx "always" [bool_sort] bool_sort
  in
  let shared_eventually_decl =
    FuncDecl.mk_func_decl_s ctx "eventually" [bool_sort] bool_sort
  in
  let temporal_decls : temporal_operator_decls =
    { next_decl= shared_next_decl
    ; nextt_decl= shared_nextt_decl
    ; previous_decl= shared_previous_decl
    ; once_decl= shared_once_decl
    ; always_decl= shared_always_decl
    ; eventually_decl= shared_eventually_decl }
  in
  Printf.printf "Processing trace file: %s\n" trace_file ;
  let json = Yojson.Basic.from_file trace_file in
  let parsed_trace = parse_trace json in
  let num_timestamps = List.length parsed_trace.trace in
  if num_timestamps = 0 then failwith "Trace is empty" ;
  if num_timestamps < 2 then
    failwith
      "Trace needs at least 2 timestamps to check 'next' behavior \
       meaningfully." ;
  Printf.printf "Number of timestamps (events) in trace: %d\n" num_timestamps ;
  let x_var = Real.mk_const_s ctx "x" in
  let y_var = Real.mk_const_s ctx "y" in
  if debug_mode then
    Printf.printf "DEBUG: x_var: %s, y_var: %s\n" (Expr.to_string x_var)
      (Expr.to_string y_var) ;
  (* Get all car IDs and their event expressions *)
  let car_ids = get_all_car_ids parsed_trace in
  let car_event_expressions =
    List.map
      (fun car_id ->
        ( car_id
        , get_car_geometric_expressions ctx parsed_trace car_id x_var y_var
        ) )
      car_ids
  in
  List.iter
    (fun (car_id, exprs) ->
      if List.length exprs <> num_timestamps then
        failwith
          (Printf.sprintf
             "Could not extract expressions for all events for car %d" car_id )
      )
    car_event_expressions ;
  Printf.printf "Reading property formula from: %s\n" property_file ;
  let property_json = Yojson.Safe.from_file property_file in
  let property_formula = formula_of_yojson property_json in
  let property_z3 =
    Formulas.formula_to_z3 ctx temporal_decls property_formula
  in
  Printf.printf "Z3 expression for property: %s\n"
    (Expr.to_string property_z3) ;
  let solver = Solver.mk_simple_solver ctx in
  Solver.add solver [property_z3] ;
  (* Add all "next" operator semantics for all cars *)
  for t = 0 to num_timestamps - 2 do
    List.iter
      (fun (_car_id, exprs) ->
        let current_expr = List.nth exprs t in
        let next_actual_expr = List.nth exprs (t + 1) in
        let z3_symbolic_next =
          Formulas.Temporal.next ctx temporal_decls current_expr
        in
        let next_definition =
          Boolean.mk_iff ctx z3_symbolic_next next_actual_expr
        in
        Solver.add solver [next_definition] )
      car_event_expressions
  done ;
  if debug_mode then
    Printf.printf "\nDEBUG: Final assertions in solver before check:\n%s\n"
      (Solver.to_string solver) ;
  (*Sat?*)
  Printf.printf
    "\n\
     Checking satisfiability of the property against the trace semantics...\n" ;
  match Solver.check solver [] with
  | SATISFIABLE -> (
      Printf.printf "\027[32mSATISFIABLE\027[0m\n" ;
      match Solver.get_model solver with
      | Some model -> Printf.printf "Model:\n%s\n" (Model.to_string model)
      | None -> Printf.printf "No model available.\n" )
  | UNSATISFIABLE -> Printf.printf "UNSATISFIABLE \n"
  | UNKNOWN ->
      Printf.printf "Solver returned UNKNOWN. Reason: %s\n"
        (Solver.get_reason_unknown solver)

(* Standalone entry point for command-line usage *)
let () =
  if Array.length Sys.argv = 3 then
    run_ltlxms_trace_check ~trace_file:Sys.argv.(1)
      ~property_file:Sys.argv.(2)
  else if Array.length Sys.argv > 1 then
    Printf.printf "Usage: %s <trace_file.json> <property_file.json>\n"
      Sys.argv.(0)
