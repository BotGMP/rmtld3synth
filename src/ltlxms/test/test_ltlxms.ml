open Yojson.Basic.Util
open Ltlxms

(* Example function to parse JSON input and create a formula *)
let rec parse_formula json = 
  match json |> member "type" |> to_string with
  | "Proposition" -> Proposition (json |> member "value" |> to_string)
  | "Intersection" ->
      let left = parse_formula (json |> member "left") in
      let right = parse_formula (json |> member "right") in
      Intersection (left, right)
  | "Union" ->
      let left = parse_formula (json |> member "left") in
      let right = parse_formula (json |> member "right") in
      Union (left, right)
  | "Negation" ->
      let term = parse_formula (json |> member "term") in
      Negation term
  | _ -> failwith "Unsupported formula type"

(* Test case *)
let test_parse_formula () =
  let json_input = Yojson.Basic.from_string {|
    {
      "type": "Intersection",
      "left": { "type": "Proposition", "value": "p" },
      "right": { "type": "Proposition", "value": "q" }
    }
  |} in
  let formula = parse_formula json_input in
  match formula with
  | Intersection (Proposition "p", Proposition "q") -> print_endline "Test passed!"
  | _ -> print_endline "Test failed!"

let () = test_parse_formula ()