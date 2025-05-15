open Z3
open Z3.Arithmetic
open Z3.Boolean
open Trace.TraceStructure

let create_z3_context () =
  let cfg = [("model", "true"); ("proof", "false")] in
  Z3.mk_context cfg

  let create_event_expressions ctx trace =
    let x = Real.mk_const_s ctx "x" in
    let y = Real.mk_const_s ctx "y" in
  
    List.map (fun event ->
      let event_id = event.event_id in
      let event_exprs_and_aabbs = List.map (fun element ->
        match element.region with
        | { region_type = "circle"; radius } ->
          let element_x = Real.mk_numeral_s ctx (string_of_float element.position.x) in
          let element_y = Real.mk_numeral_s ctx (string_of_float element.position.y) in
          let radius = Real.mk_numeral_s ctx (string_of_float radius) in
  
          let dx = Arithmetic.mk_sub ctx [x; element_x] in
          let dy = Arithmetic.mk_sub ctx [y; element_y] in
          let distance_squared = Arithmetic.mk_add ctx [
            Arithmetic.mk_mul ctx [dx; dx];
            Arithmetic.mk_mul ctx [dy; dy]
          ] in
          let radius_squared = Arithmetic.mk_mul ctx [radius; radius] in
  
          (* Create the expression: (x - element_x)^2 + (y - element_y)^2 < radius^2 *)
          let expr = mk_lt ctx distance_squared radius_squared in
  
          (* AABB *)
          let min_x = element.position.x -. element.region.radius in
          let max_x = element.position.x +. element.region.radius in
          let min_y = element.position.y -. element.region.radius in
          let max_y = element.position.y +. element.region.radius in
          let aabb = (min_x, max_x, min_y, max_y) in
  
          Boolean.mk_const_s ctx (Printf.sprintf "a_%d" event_id), expr, aabb
  
        | { region_type = "triangle"; radius } ->
          let element_x = Real.mk_numeral_s ctx (string_of_float element.position.x) in
          let element_y = Real.mk_numeral_s ctx (string_of_float element.position.y) in
          let circumradius = Real.mk_numeral_s ctx (string_of_float radius) in
  
          let dx = Arithmetic.mk_sub ctx [x; element_x] in
          let dy = Arithmetic.mk_sub ctx [y; element_y] in
          let distance_squared = Arithmetic.mk_add ctx [
            Arithmetic.mk_mul ctx [dx; dx];
            Arithmetic.mk_mul ctx [dy; dy]
          ] in
          let circumradius_squared = Arithmetic.mk_mul ctx [circumradius; circumradius] in
  
          (* Create the expression: (x - element_x)^2 + (y - element_y)^2 < circumradius^2 *)
          let expr = mk_lt ctx distance_squared circumradius_squared in
  
          (* AABB *)
          let min_x = element.position.x -. element.region.radius in
          let max_x = element.position.x +. element.region.radius in
          let min_y = element.position.y -. element.region.radius in
          let max_y = element.position.y +. element.region.radius in
          let aabb = (min_x, max_x, min_y, max_y) in
  
          Boolean.mk_const_s ctx (Printf.sprintf "a_%d" event_id), expr, aabb
  
        | _ -> failwith "Unsupported region type"
      ) event.elements in
  
      (* Combine all element expressions *)
      let combined_expr = mk_and ctx (List.map (fun (_, expr, _) -> expr) event_exprs_and_aabbs) in
      let aabbs = List.map (fun (_, _, aabb) -> aabb) event_exprs_and_aabbs in
      (event_id, combined_expr, List.map (fun (z3_var, _, _) -> z3_var) event_exprs_and_aabbs, aabbs)
    ) trace.trace
  
  let () =
    let ctx = create_z3_context () in
  
    (* Get all .json files in current dir*)
    let json_files =
      Sys.readdir "."
      |> Array.to_list
      |> List.filter (fun file -> Filename.check_suffix file ".json")
    in
  
    List.iter (fun json_file ->
      let json_path = json_file in
      Printf.printf "Processing file: %s\n" json_path;
  
      let json = Yojson.Basic.from_file json_path in
      let parsed_trace = parse_trace json in
  
      let event_expressions = create_event_expressions ctx parsed_trace in
  
      List.iter (fun (event_id, combined_expr, z3_vars, aabbs) ->
        Printf.printf "Event ID: %d\n" event_id;
        Printf.printf "Expression: %s\n" (Expr.to_string combined_expr);
        List.iter (fun z3_var ->
          Printf.printf "Z3 Variable: %s\n" (Expr.to_string z3_var)
        ) z3_vars;
        List.iter (fun (min_x, max_x, min_y, max_y) ->
          Printf.printf "AABB: min_x=%f, max_x=%f, min_y=%f, max_y=%f\n" min_x max_x min_y max_y
        ) aabbs
      ) event_expressions;
  
      Printf.printf "\n"
    ) json_files