let () =
  let term = Intersection (Proposition "p", Proposition "q") in
  let json = Ltlxms.term_to_yojson term in
  Printf.printf "Expected JSON: %s\n" (Yojson.Safe.pretty_to_string json)