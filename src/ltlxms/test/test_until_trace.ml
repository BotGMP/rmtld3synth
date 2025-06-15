open Z3
open Z3.Arithmetic
open Trace.TraceStructure 
open Formulas 

(* Global debug flag *)
let debug_mode = false

let create_z3_context () =
  let cfg = [("model", "true"); ("proof", "false")] in
  Z3.mk_context cfg

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

let () =
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

  let trace_file_path = "/home/gmp/Projeto/rmtld3synth/src/trace/test/Scenario5_Test1.json" in
  Printf.printf "Processing trace file: %s\n" trace_file_path; 

  let json = Yojson.Basic.from_file trace_file_path in
  let parsed_trace = Trace.TraceStructure.parse_trace json in 

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

  let e1_base = List.hd car1_event_expressions in (* This is e1 at t=0 *)
  let e2_base = List.hd car2_event_expressions in (* This is e2 at t=0 *)

  let unravel_depth = num_timestamps - 1 in

  Printf.printf "Base expression for Car 1 (e1_base at t=0): %s\n" (Expr.to_string e1_base);
  Printf.printf "Base expression for Car 2 (e2_base at t=0): %s\n" (Expr.to_string e2_base);
  Printf.printf "Unravelling depth for Until (k_unravel): %d\n" unravel_depth;

  if debug_mode then Printf.printf "DEBUG: Calling Formulas.Temporal.until with e1_base, e2_base, depth %d\n" unravel_depth;
  let until_property_z3 = Formulas.Temporal.until ctx temporal_decls e1_base e2_base unravel_depth in
  Printf.printf "Z3 expression for (e1_base U e2_base): %s\n" (Expr.to_string until_property_z3);

  let solver = Solver.mk_simple_solver ctx in
  if debug_mode then Printf.printf "DEBUG: Adding to solver: until_property_z3\n";
  Solver.add solver [until_property_z3];

  if debug_mode then Printf.printf "DEBUG: Defining 'next' operator semantics:\n";
  
  if debug_mode then
    if num_timestamps >= 2 then ( 
      let e1_at_t0 = List.nth car1_event_expressions 0 in (* Same as e1_base *)
      let e1_at_t1 = List.nth car1_event_expressions 1 in (* e1 at the next timestamp *)
      let symbolic_next_of_e1_at_t0 = Formulas.Temporal.next ctx temporal_decls e1_at_t0 in

      Printf.printf "\n--- DETAILED NEXT OPERATOR CHECK (Car 1, t=0 to t=1) ---\n";
      Printf.printf "Expression for e1 at t=0 (e1_base) : %s\n" (Expr.to_string e1_at_t0);
      Printf.printf "Symbolic (next e1_base)            : %s\n" (Expr.to_string symbolic_next_of_e1_at_t0);
      Printf.printf "Expression for e1 at t=1           : %s\n" (Expr.to_string e1_at_t1);
      Printf.printf "--- END DETAILED NEXT OPERATOR CHECK ---\n\n";
    );

  for t = 0 to num_timestamps - 2 do
    (* Car 1 *)
    let current_expr_c1 = List.nth car1_event_expressions t in
    let next_actual_expr_c1 = List.nth car1_event_expressions (t + 1) in
    
    let z3_symbolic_next_of_current_c1 = Formulas.Temporal.next ctx temporal_decls current_expr_c1 in
    
    if debug_mode then Printf.printf "DEBUG: For Car 1, t=%d:\n" t;
    if debug_mode then Printf.printf "DEBUG:   current_expr_c1 (g1_t%d): %s\n" t (Expr.to_string current_expr_c1);
    if debug_mode then Printf.printf "DEBUG:   next_actual_expr_c1 (g1_t%d): %s\n" (t+1) (Expr.to_string next_actual_expr_c1);
    if debug_mode then Printf.printf "DEBUG:   Formulas.Temporal.next(g1_t%d): %s\n" t (Expr.to_string z3_symbolic_next_of_current_c1);
    
    let c1_next_definition = Boolean.mk_iff ctx z3_symbolic_next_of_current_c1 next_actual_expr_c1 in
    if debug_mode then Printf.printf "DEBUG:   Asserting for Car 1, t=%d: iff(%s, %s)\n" t (Expr.to_string z3_symbolic_next_of_current_c1) (Expr.to_string next_actual_expr_c1);
    Solver.add solver [c1_next_definition];

    (* Car 2 *)
    let current_expr_c2 = List.nth car2_event_expressions t in
    let next_actual_expr_c2 = List.nth car2_event_expressions (t + 1) in
    if debug_mode then Printf.printf "DEBUG: For Car 2, t=%d:\n" t;
    if debug_mode then Printf.printf "DEBUG:   current_expr_c2 (g2_t%d): %s\n" t (Expr.to_string current_expr_c2);
    if debug_mode then Printf.printf "DEBUG:   next_actual_expr_c2 (g2_t%d): %s\n" (t+1) (Expr.to_string next_actual_expr_c2);
    let z3_symbolic_next_of_current_c2 = Formulas.Temporal.next ctx temporal_decls current_expr_c2 in
    if debug_mode then Printf.printf "DEBUG:   Formulas.Temporal.next(g2_t%d): %s\n" t (Expr.to_string z3_symbolic_next_of_current_c2);
    let c2_next_definition = Boolean.mk_iff ctx z3_symbolic_next_of_current_c2 next_actual_expr_c2 in
    if debug_mode then Printf.printf "DEBUG:   Asserting for Car 2, t=%d: iff(%s, %s)\n" t (Expr.to_string z3_symbolic_next_of_current_c2) (Expr.to_string next_actual_expr_c2);
    Solver.add solver [c2_next_definition];
  done;
  
  (*W/out this solver always just choses a point inside e2_base radius*)
  let not_e2_base_at_t0 = Boolean.mk_not ctx e2_base in
  if debug_mode then Printf.printf "DEBUG: Adding constraint: NOT (e2_base at t=0): %s\n" (Expr.to_string not_e2_base_at_t0);
  Solver.add solver [not_e2_base_at_t0]; 
  Printf.printf "Added constraint: NOT (e2_base at t=0)\n";

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
