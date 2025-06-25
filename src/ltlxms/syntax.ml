type term =
  | Proposition of string
  | Intersection of term * term
  | Union of term * term
  | Negation of term
  | NextT of term
  | Expand of term * float
  | UntilT of term * term * int
  [@@deriving yojson]

type formula =
  | SubSetEq of term * term
  | Collides of term * term * int
  | Equal of term * term
  | Disconnected of term * term
  | And of formula * formula
  | Or of formula * formula
  | Not of formula
  | Next of formula
  | Until of formula * formula * int
  | Always of formula
  | Eventually of formula
  | Previous of formula
  | Since of formula * formula
  | Implies of formula * formula
  | Overlap of term * term
  | Include of term * term
  | AlwaysPast of formula
  | Once of formula
  [@@deriving yojson]