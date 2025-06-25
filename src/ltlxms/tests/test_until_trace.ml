(* Running Instructions:
1. Run "dune build" to compile
2. Navigate to the project root directory
3. Run "dune exec src/ltlxms/test/test_until_trace.exe src/trace/test/Scenario5_Test1.json src/ltlxms/test/property.json"
*)
(*
TODO :
 ARGUMENTS DONE
 BASH SCRIPT (ARGS & Flags & Verbose/Debug)
 TEST CASES WITH DIFFERENT TRACES
 *)
(*
C1 U C2 DONE
C1 SubSetEq C2 DONE 
C1 SubSetEq C2 and C2 SubSetEq C1 === Equal DONE
(C1 SubSetEq Negation C2) and (C2 SubSetEq Negation C1)=== Disconected DONE
(C1 Disconected C2) Until (C1 Overlap C2) DONE
*)

open Z3
open Z3.Arithmetic
open Trace.Xyz
open Ltlxms.Syntax
open Ltlxms.Encoding

(* Global debug flag *)
let debug_mode = false

let create_z3_context () =
  let cfg = [("model", "true"); ("proof", "false")] in
  Z3.mk_context cfg

let get_circle_params (trace: trace) car_id t ctx =
    let event = List.nth trace.trace t in
    let element = List.find (fun e -> e.id = car_id) event.elements in
    let x = Z3.Arithmetic.Real.mk_numeral_s ctx (string_of_float element.position.x) in
    let y = Z3.Arithmetic.Real.mk_numeral_s ctx (string_of_float element.position.y) in
    let r = Z3.Arithmetic.Real.mk_numeral_s ctx (string_of_float element.region.radius) in
    (x, y, r)

(* Get the car expressions*)
let get_car_geometric_expressions ctx (trace: trace) (target_car_id: int) (x_var: Expr.expr) (y_var: Expr.expr) : Expr.expr list =
  if debug_mode then Printf.printf "DEBUG: get_car_geometric_expressions for car ID %d\n" target_car_id;
  List.filter_map (fun event ->
    if debug_mode then Printf.printf "DEBUG: Processing event ID %d for car ID %d\n" event.event_id target_car_id;
    List.find_opt (fun element -> element.id = target_car_id) event.elements
    |> Option.map (fun element ->
      if debug_mode then Printf.printf "DEBUG: Found element ID %d, type %s for car ID %d in event %d\n" element.id element.element_type target_car_id event.event_id;
      match element.region with
      | { region_type = "circle"; radius } ->
          let element_x = Real.mk_numeral_s ctx (string_of_float element.position.x) in
          let element_y = Real.mk_numeral_s ctx (string_of_float element.position.y) in
          let r_val = Real.mk_numeral_s ctx (string_of_float radius) in
          let dx = Arithmetic.mk_sub ctx [x_var; element_x] in
          let dy = Arithmetic.mk_sub ctx [y_var; element_y] in
          let distance_squared = Arithmetic.mk_add ctx [
            Arithmetic.mk_mul ctx [dx; dx];
            Arithmetic.mk_mul ctx [dy; dy]
          ] in
          let radius_squared = Arithmetic.mk_mul ctx [r_val; r_val] in
          let expr = Arithmetic.mk_lt ctx distance_squared radius_squared in
          if debug_mode then Printf.printf "DEBUG: Car %d, Event %d (Circle): %s\n" target_car_id event.event_id (Expr.to_string expr);
          expr
      | { region_type = "triangle"; radius (* circumradius *) } ->
          let element_x = Real.mk_numeral_s ctx (string_of_float element.position.x) in
          let element_y = Real.mk_numeral_s ctx (string_of_float element.position.y) in
          let circumradius_val = Real.mk_numeral_s ctx (string_of_float radius) in
          let dx = Arithmetic.mk_sub ctx [x_var; element_x] in
          let dy = Arithmetic.mk_sub ctx [y_var; element_y] in
          let distance_squared = Arithmetic.mk_add ctx [
            Arithmetic.mk_mul ctx [dx; dx];
            Arithmetic.mk_mul ctx [dy; dy]
          ] in
          let circumradius_squared = Arithmetic.mk_mul ctx [circumradius_val; circumradius_val] in
          let expr = Arithmetic.mk_lt ctx distance_squared circumradius_squared in
          if debug_mode then Printf.printf "DEBUG: Car %d, Event %d (Triangle): %s\n" target_car_id event.event_id (Expr.to_string expr);
          expr
      | _ -> 
          if debug_mode then Printf.printf "ERROR: Unsupported region type for car %d in event %d\n" target_car_id event.event_id;
          failwith ("Unsupported region type for car " ^ string_of_int target_car_id)
    )
  ) trace.trace

(* Temporal unrolling *)
let rec unroll_formula_over_trace ctx _ parsed_trace formula car1_exprs car2_exprs step =
  let num_steps = List.length car1_exprs in
  match formula with
  | Always f ->
      let sub_exprs = List.init num_steps (fun t ->
        unroll_formula_over_trace ctx () parsed_trace f car1_exprs car2_exprs t
      ) in
      Formulas.Logic.list_and ctx sub_exprs
  | Eventually f ->
      let sub_exprs = List.init num_steps (fun t ->
        unroll_formula_over_trace ctx () parsed_trace f car1_exprs car2_exprs t
      ) in
      Formulas.Logic.list_or ctx sub_exprs
  | Next f ->
      if step + 1 < num_steps then
        unroll_formula_over_trace ctx () parsed_trace f car1_exprs car2_exprs (step + 1)
      else
        Formulas.Logic.list_and ctx []  (* or mk_true *)
  | Previous f ->
      if step > 0 then
        unroll_formula_over_trace ctx () parsed_trace f car1_exprs car2_exprs (step - 1)
      else
        Formulas.Logic.list_and ctx []  (* or mk_true *)
  | SubSetEq (Proposition "C1", Proposition "C2") ->
      Formulas.Logic.sub_set_eq ctx (List.nth car1_exprs step) (List.nth car2_exprs step)
  | Overlap (Proposition "C1", Proposition "C2") ->
      let (x1, y1, r1) = get_circle_params parsed_trace 1 step ctx in
      let (x2, y2, r2) = get_circle_params parsed_trace 2 step ctx in
      Formulas.Logic.circle_overlap ctx (x1, y1, r1) (x2, y2, r2)
  | Disconnected (Proposition "C1", Proposition "C2") ->
      let not_c2 = Formulas.Logic.single_not ctx (List.nth car2_exprs step) in
      let not_c1 = Formulas.Logic.single_not ctx (List.nth car1_exprs step) in
      let sub1 = Formulas.Logic.sub_set_eq ctx (List.nth car1_exprs step) not_c2 in
      let sub2 = Formulas.Logic.sub_set_eq ctx (List.nth car2_exprs step) not_c1 in
      Formulas.Logic.single_and ctx sub1 sub2
  | _ ->
      failwith "Temporal unrolling for this formula not implemented."
      
let run_ltlxms_trace_check ~trace_file ~property_file =
  let ctx = create_z3_context () in

  (* Create shared function declarations for temporal operators *)
  let bool_sort = Boolean.mk_sort ctx in
  let shared_next_decl = FuncDecl.mk_func_decl_s ctx "next" [bool_sort] bool_sort in
  let shared_nextt_decl = FuncDecl.mk_func_decl_s ctx "nextt" [bool_sort] bool_sort in
  let shared_previous_decl = FuncDecl.mk_func_decl_s ctx "previous" [bool_sort] bool_sort in
  let shared_once_decl = FuncDecl.mk_func_decl_s ctx "once" [bool_sort] bool_sort in
  let shared_always_decl = FuncDecl.mk_func_decl_s ctx "always" [bool_sort] bool_sort in
  let shared_eventually_decl = FuncDecl.mk_func_decl_s ctx "eventually" [bool_sort] bool_sort in

  let temporal_decls : temporal_operator_decls = {
    next_decl = shared_next_decl;
    nextt_decl = shared_nextt_decl;
    previous_decl = shared_previous_decl;
    once_decl = shared_once_decl;
    always_decl = shared_always_decl;
    eventually_decl = shared_eventually_decl;
  } in

  Printf.printf "Processing trace file: %s\n" trace_file; 
  let json = Yojson.Basic.from_file trace_file in
  let parsed_trace = parse_trace json in 

  let num_timestamps = List.length parsed_trace.trace in
  if num_timestamps = 0 then failwith "Trace is empty";
  if num_timestamps < 2 then failwith "Trace needs at least 2 timestamps to check 'next' behavior meaningfully.";
  Printf.printf "Number of timestamps (events) in trace: %d\n" num_timestamps; 

  let x_var = Real.mk_const_s ctx "x" in
  let y_var = Real.mk_const_s ctx "y" in
  if debug_mode then Printf.printf "DEBUG: x_var: %s, y_var: %s\n" (Expr.to_string x_var) (Expr.to_string y_var);

  let car1_event_expressions = get_car_geometric_expressions ctx parsed_trace 1 x_var y_var in
  let car2_event_expressions = get_car_geometric_expressions ctx parsed_trace 2 x_var y_var in

  if List.length car1_event_expressions <> num_timestamps || List.length car2_event_expressions <> num_timestamps then
    failwith "Could not extract expressions for all events for specified cars";

  Printf.printf "Reading property formula from: %s\n" property_file;
  let property_json = Yojson.Safe.from_file property_file in
  let property_formula = formula_of_yojson property_json in

  let property_z3 = Formulas.formula_to_z3 ctx temporal_decls property_formula in

  Printf.printf "Z3 expression for property: %s\n" (Expr.to_string property_z3);

  let solver = Solver.mk_simple_solver ctx in
  Solver.add solver [property_z3];

  (* Add all "next" operator semantics *)
  for t = 0 to num_timestamps - 2 do
    (* Car 1 *)
    let current_expr_c1 = List.nth car1_event_expressions t in
    let next_actual_expr_c1 = List.nth car1_event_expressions (t + 1) in
    let z3_symbolic_next_of_current_c1 = Formulas.Temporal.next ctx temporal_decls current_expr_c1 in
    let c1_next_definition = Boolean.mk_iff ctx z3_symbolic_next_of_current_c1 next_actual_expr_c1 in
    Solver.add solver [c1_next_definition];

    (* Car 2 *)
    let current_expr_c2 = List.nth car2_event_expressions t in
    let next_actual_expr_c2 = List.nth car2_event_expressions (t + 1) in
    let z3_symbolic_next_of_current_c2 = Formulas.Temporal.next ctx temporal_decls current_expr_c2 in
    let c2_next_definition = Boolean.mk_iff ctx z3_symbolic_next_of_current_c2 next_actual_expr_c2 in
    Solver.add solver [c2_next_definition];
  done;

  if debug_mode then Printf.printf "\nDEBUG: Final assertions in solver before check:\n%s\n" (Solver.to_string solver);

  (*Sat?*)
  Printf.printf "\nChecking satisfiability of the property against the trace semantics...\n";
  match Solver.check solver [] with
  | SATISFIABLE ->
      Printf.printf "SATISFIABLE\n";
      (match Solver.get_model solver with
      | Some model -> Printf.printf "Model:\n%s\n" (Model.to_string model)
      | None -> Printf.printf "No model available.\n")
  | UNSATISFIABLE ->
      Printf.printf "UNSATISFIABLE \n"
  | UNKNOWN ->
      Printf.printf "Solver returned UNKNOWN. Reason: %s\n" (Solver.get_reason_unknown solver)

(* Standalone entry point for command-line usage *)
let () =
  if Array.length Sys.argv = 3 then
    run_ltlxms_trace_check ~trace_file:Sys.argv.(1) ~property_file:Sys.argv.(2)
  else if Array.length Sys.argv > 1 then
    Printf.printf "Usage: %s <trace_file.json> <property_file.json>\n" Sys.argv.(0)