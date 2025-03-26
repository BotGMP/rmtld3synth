open Z3
open Formulas

let ctx = mk_context []

let test_logic_operations () =
  let expr1 = Boolean.mk_true ctx in
  let expr2 = Boolean.mk_false ctx in

  (* Test single_or *)
  let or_result = Logic.single_or expr1 expr2 in
  assert (Boolean.is_true or_result);

  (* Test single_and *)
  let and_result = Logic.single_and expr1 expr2 in
  assert (Boolean.is_false and_result);

  (* Test single_not *)
  let not_result = Logic.single_not expr1 in
  assert (Boolean.is_false not_result);

  print_endline "Logic operations test passed!"

let test_temporal_operations () =
  let expr1_list = [Boolean.mk_true ctx; Boolean.mk_false ctx] in
  let expr2_list = [Boolean.mk_false ctx; Boolean.mk_true ctx] in

  (* Test until *)
  let until_result = Temporal.until expr1_list expr2_list 0 in
  assert (Boolean.is_false until_result);

  (* Test previous *)
  let previous_result = Temporal.previous (Boolean.mk_true ctx) 1 in
  assert (Boolean.is_true previous_result);

  print_endline "Temporal operations test passed!"

let () =
  test_logic_operations ();
  test_temporal_operations ()