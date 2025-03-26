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
end

(* Module for temporal logic operations *)
module Temporal = struct
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

  let previous (ctx : Z3.context) (formula : Z3.Expr.expr) (repetitions : int) : Z3.Expr.expr =
  if formula = Z3.Boolean.mk_true ctx then
    Z3.Boolean.mk_true ctx
  else if formula = Z3.Boolean.mk_false ctx then
    Z3.Boolean.mk_false ctx
  else
    let symbol = Z3.Symbol.mk_string ctx "previous" in
    let n = Z3.FuncDecl.mk_func_decl ctx symbol [Z3.Boolean.mk_sort ctx; Z3.Arithmetic.Integer.mk_sort ctx] (Z3.Boolean.mk_sort ctx) in
    Z3.Expr.mk_app ctx n [formula; Z3.Arithmetic.Integer.mk_numeral_i ctx repetitions]

  (* Auxiliary function for the below since implementation *)
  let rec since_aux (ctx : Z3.context) (formula1 : Z3.Expr.expr) (formula2 : Z3.Expr.expr) (n : int) (m : int) : Z3.Expr.expr =
    if n = 0 then
      Logic.single_and ctx formula1 formula2
    else if n > 0 then
      Logic.single_and ctx (previous ctx formula1 (n - 1)) (since_aux ctx formula1 formula2 (n - 1) m)
    else if n = -1 then
      previous ctx formula2 (m - 1)
    else
      Z3.Boolean.mk_false ctx

  let rec since_formula (ctx : Z3.context) (formula1 : Z3.Expr.expr) (formula2 : Z3.Expr.expr) (bound : int) (m : int) : Z3.Expr.expr =
    if m >= bound then
      Z3.Boolean.mk_false ctx
    else if m + 2 <= bound then
      Logic.single_or
        ctx
        (since_aux ctx formula1 formula2 (m + 1) (m + 2))
        (since_formula ctx formula1 formula2 bound (m + 1))
    else
      Z3.Boolean.mk_false ctx
end