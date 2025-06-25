open Ltlxms

let () =
  (* Construct the formula *)
  let formula =
    And (
      SubSetEq (Proposition "a", Proposition "b"),
      Next (
        Next (
          SubSetEq (Proposition "a", Proposition "b")
        )
      )
    )
  in

  (* Convert the formula to JSON *)
  let json = Ltlxms.formula_to_yojson formula in

  (* Print the JSON *)
  Printf.printf "Expected JSON: %s\n" (Yojson.Safe.pretty_to_string json)