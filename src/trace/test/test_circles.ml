open Z3
open Z3.Arithmetic
open Z3.Boolean
open Trace.TraceStructure

let create_z3_context () =
  let cfg = [("model", "true")] in
  mk_context cfg

let create_event_expressions ctx trace =
  let x = Real.mk_const_s ctx "x" in
  let y = Real.mk_const_s ctx "y" in

  List.map (fun event ->
    let event_id = event.event_id in
    let event_exprs = List.map (fun element ->
      let element_x = Real.mk_numeral_s ctx (string_of_float element.position.x) in
      let element_y = Real.mk_numeral_s ctx (string_of_float element.position.y) in
      let radius = Real.mk_numeral_s ctx (string_of_float element.region.radius) in

      let dx = Arithmetic.mk_sub ctx [x; element_x] in
      let dy = Arithmetic.mk_sub ctx [y; element_y] in
      let distance_squared = Arithmetic.mk_add ctx [
        Arithmetic.mk_mul ctx [dx; dx];
        Arithmetic.mk_mul ctx [dy; dy]
      ] in
      let radius_squared = Arithmetic.mk_mul ctx [radius; radius] in

      (* Create the expression: (x - element_x)^2 + (y - element_y)^2 < radius^2 *)
      mk_lt ctx distance_squared radius_squared
    ) event.elements in

    (* Combine all element expressions *)
    (event_id, mk_and ctx event_exprs)
  ) trace.trace

let () =
  let ctx = create_z3_context () in
  let json_file = "/home/gmp/Projeto/rmtld3synth/src/trace/test/Scenario1_Test1.json" in
  let json = Yojson.Basic.from_file json_file in
  let parsed_trace = parse_trace json in

  let event_expressions = create_event_expressions ctx parsed_trace in

  List.iter (fun (event_id, expr) ->
    Printf.printf "Event ID: %d\n" event_id;
    Printf.printf "Expression: %s\n" (Expr.to_string expr)
  ) event_expressions