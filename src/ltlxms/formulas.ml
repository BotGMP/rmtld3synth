module Formulas = struct
(* Module for basic logical operations *)
module Logic = struct
  let single_or (ctx : Z3.context) (expr1 : Z3.Expr.expr) (expr2 : Z3.Expr.expr) : Z3.Expr.expr =
    Z3.Boolean.mk_or ctx [expr1; expr2]

  let single_and (ctx : Z3.context) (expr1 : Z3.Expr.expr) (expr2 : Z3.Expr.expr) : Z3.Expr.expr =
    Z3.Boolean.mk_and ctx [expr1; expr2]

  let single_not (ctx : Z3.context) (expr : Z3.Expr.expr) : Z3.Expr.expr =
    Z3.Boolean.mk_not ctx expr

  let single_implies (ctx : Z3.context) (expr1 : Z3.Expr.expr) (expr2 : Z3.Expr.expr) : Z3.Expr.expr =
    Z3.Boolean.mk_implies ctx expr1 expr2

  let sub_set_eq (ctx : Z3.context) (tau1 : Z3.Expr.expr) (tau2 : Z3.Expr.expr) : Z3.Expr.expr =
    let x = Z3.Arithmetic.Real.mk_const_s ctx "xs" in
    let y = Z3.Arithmetic.Real.mk_const_s ctx "ys" in
    let quantifier = Z3.Quantifier.mk_forall_const ctx [x; y] (Z3.Boolean.mk_implies ctx tau1 tau2) None [] [] None None in
    Z3.Quantifier.expr_of_quantifier quantifier
  let overlap (ctx : Z3.context) (expr1 : Z3.Expr.expr) (expr2 : Z3.Expr.expr) : Z3.Expr.expr =
    let symbol = Z3.Symbol.mk_string ctx "overlap" in
    let func_decl = Z3.FuncDecl.mk_func_decl ctx symbol [Z3.Boolean.mk_sort ctx; Z3.Boolean.mk_sort ctx] (Z3.Boolean.mk_sort ctx) in
    Z3.Expr.mk_app ctx func_decl [expr1; expr2]
end

(* Module for temporal logic operations *)
module Temporal = struct
  (* NextT operator *)
  let nextt (ctx : Z3.context) (formula : Z3.Expr.expr) : Z3.Expr.expr =
    let symbol = Z3.Symbol.mk_string ctx "nextt" in
    let func_decl = Z3.FuncDecl.mk_func_decl ctx symbol [Z3.Boolean.mk_sort ctx] (Z3.Boolean.mk_sort ctx) in
    Z3.Expr.mk_app ctx func_decl [formula]
  (* Once operator *)
  let once (ctx : Z3.context) (formula : Z3.Expr.expr) : Z3.Expr.expr =
    let symbol = Z3.Symbol.mk_string ctx "once" in
    let func_decl = Z3.FuncDecl.mk_func_decl ctx symbol [Z3.Boolean.mk_sort ctx] (Z3.Boolean.mk_sort ctx) in
    Z3.Expr.mk_app ctx func_decl [formula]
  (* Next operator *)
  let next (ctx : Z3.context) (formula : Z3.Expr.expr) : Z3.Expr.expr =
    let symbol = Z3.Symbol.mk_string ctx "next" in
    let func_decl = Z3.FuncDecl.mk_func_decl ctx symbol [Z3.Boolean.mk_sort ctx] (Z3.Boolean.mk_sort ctx) in
    Z3.Expr.mk_app ctx func_decl [formula]
  
  (* Always operator *)
  let always (ctx : Z3.context) (formula : Z3.Expr.expr) : Z3.Expr.expr =
    let symbol = Z3.Symbol.mk_string ctx "always" in
    let func_decl = Z3.FuncDecl.mk_func_decl ctx symbol [Z3.Boolean.mk_sort ctx] (Z3.Boolean.mk_sort ctx) in
    Z3.Expr.mk_app ctx func_decl [formula]

  (* Eventually operator *)
  let eventually (ctx : Z3.context) (formula : Z3.Expr.expr) : Z3.Expr.expr =
    let symbol = Z3.Symbol.mk_string ctx "eventually" in
    let func_decl = Z3.FuncDecl.mk_func_decl ctx symbol [Z3.Boolean.mk_sort ctx] (Z3.Boolean.mk_sort ctx) in
    Z3.Expr.mk_app ctx func_decl [formula]

  (* Auxiliary function for the below until implementation *)
  let until_aux (ctx : Z3.context) (expr1_list : Z3.Expr.expr list) (expr2_list : Z3.Expr.expr list) (n : int) (m : int) : Z3.Expr.expr =
    if n >= List.length expr1_list || m >= List.length expr2_list then
      Z3.Boolean.mk_false ctx
    else
      let expr1_n = List.nth expr1_list n in
      let expr2_m = List.nth expr2_list m in
      Logic.single_and ctx expr1_n expr2_m

  let rec until (ctx : Z3.context) (expr1_list : Z3.Expr.expr list) (expr2_list : Z3.Expr.expr list) (n : int) : Z3.Expr.expr =
    if List.length expr1_list <= 1 || List.length expr2_list <= 1 || List.length expr1_list <> List.length expr2_list then
      invalid_arg "The lists must have the same size and length > 1"
    else if n + 2 = List.length expr1_list then
      Z3.Boolean.mk_false ctx
    else
      let aux = until_aux ctx expr1_list expr2_list (n + 2) (n + 3) in
      let rest = until ctx expr1_list expr2_list (n + 1) in
      Logic.single_or ctx aux rest

  (* Previous operator*)
  let previous (ctx : Z3.context) (formula : Z3.Expr.expr) : Z3.Expr.expr =
    let symbol = Z3.Symbol.mk_string ctx "previous" in
    let func_decl = Z3.FuncDecl.mk_func_decl ctx symbol [Z3.Boolean.mk_sort ctx] (Z3.Boolean.mk_sort ctx) in
    Z3.Expr.mk_app ctx func_decl [formula]

  (* Auxiliary function for the below since implementation *)
  let rec since_aux (ctx : Z3.context) (formula1 : Z3.Expr.expr) (formula2 : Z3.Expr.expr) (n : int) : Z3.Expr.expr =
    if n = 0 then
      Logic.single_and ctx formula1 formula2
    else if n > 0 then
      Logic.single_and ctx (previous ctx formula1) (since_aux ctx formula1 formula2 (n - 1))
    else
      previous ctx formula2

  let rec since_formula (ctx : Z3.context) (formula1 : Z3.Expr.expr) (formula2 : Z3.Expr.expr) (bound : int) (m : int) : Z3.Expr.expr =
    if m >= bound then
      Z3.Boolean.mk_false ctx
  else if m + 2 <= bound then
    Logic.single_or
      ctx
      (since_aux ctx formula1 formula2 (m + 1))
      (since_formula ctx formula1 formula2 bound (m + 1))
  else
    Z3.Boolean.mk_false ctx
end

(* Convert a term into a Z3 expression *)
let rec term_to_z3 ctx term =
  match term with
  | Ltlxms.Proposition p -> Z3.Boolean.mk_const ctx (Z3.Symbol.mk_string ctx p)
  | Ltlxms.Intersection (t1, t2) -> Logic.single_and ctx (term_to_z3 ctx t1) (term_to_z3 ctx t2)
  | Ltlxms.Union (t1, t2) -> Logic.single_or ctx (term_to_z3 ctx t1) (term_to_z3 ctx t2)
  | Ltlxms.Expand (t, value) -> 
      let expand_symbol = Z3.Symbol.mk_string ctx "expand" in
      let expand_func = Z3.FuncDecl.mk_func_decl ctx expand_symbol [Z3.Boolean.mk_sort ctx; Z3.Arithmetic.Real.mk_sort ctx] (Z3.Boolean.mk_sort ctx) in
      Z3.Expr.mk_app ctx expand_func [term_to_z3 ctx t; Z3.Arithmetic.Real.mk_numeral_s ctx (string_of_float value)]
  | Ltlxms.NextT t -> Temporal.nextt ctx (term_to_z3 ctx t)
  | _ -> failwith "Unsupported term type for conversion to Z3"

(* Convert a formula into a Z3 expression *)
let rec formula_to_z3 ctx formula =
  match formula with
  | Ltlxms.SubSetEq (t1, t2) -> Logic.sub_set_eq ctx (term_to_z3 ctx t1) (term_to_z3 ctx t2)
  | Ltlxms.And (f1, f2) -> Logic.single_and ctx (formula_to_z3 ctx f1) (formula_to_z3 ctx f2)
  | Ltlxms.Or (f1, f2) -> Logic.single_or ctx (formula_to_z3 ctx f1) (formula_to_z3 ctx f2)
  | Ltlxms.Not f -> Logic.single_not ctx (formula_to_z3 ctx f)
  | Ltlxms.Next f -> Temporal.next ctx (formula_to_z3 ctx f)
  | Ltlxms.Always f -> Temporal.always ctx (formula_to_z3 ctx f)
  | Ltlxms.Eventually f -> Temporal.eventually ctx (formula_to_z3 ctx f)
  | Ltlxms.Once f -> Temporal.once ctx (formula_to_z3 ctx f)
  | Ltlxms.Overlap (t1, t2) -> Logic.overlap ctx (term_to_z3 ctx t1) (term_to_z3 ctx t2)
  | Ltlxms.Implies (f1, f2) -> Logic.single_implies ctx (formula_to_z3 ctx f1) (formula_to_z3 ctx f2)
  | Ltlxms.Previous f -> Temporal.previous ctx (formula_to_z3 ctx f)
  | _ -> failwith "Unsupported formula type for conversion to Z3"
end