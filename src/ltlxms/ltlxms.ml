type term =
  | Proposition of string
  | Intersection of term * term
  | Union of term * term
  | Negation of term
  | NextT of term * int
  | Expand of term * float
  | UntilT of term * term
  [@@deriving yojson]

type formula =
  | SubSetEq of term * term
  | And of formula * formula
  | Or of formula * formula
  | Not of formula
  | Next of formula * int
  | Until of formula * formula
  | Always of formula
  | Eventually of formula
  | Previous of formula * int
  | Since of formula * formula
  | Implies of formula * formula
  | Overlap of term * term
  | Equal of term * term
  | Include of term * term
  | Disconnect of term * term
  | AlwaysPast of formula
  | Once of formula
  [@@deriving yojson]