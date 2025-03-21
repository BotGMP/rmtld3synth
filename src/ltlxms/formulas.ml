(* Module for basic logical operations *)
module Logic = struct
  let single_or (expr1 : Z3.Expr.expr) (expr2 : Z3.Expr.expr) : Z3.Expr.expr =
    Z3.Boolean.mk_or ctx [expr1; expr2]

  let single_and (expr1 : Z3.Expr.expr) (expr2 : Z3.Expr.expr) : Z3.Expr.expr =
    Z3.Boolean.mk_and ctx [expr1; expr2]

  let single_not (expr : Z3.Expr.expr) : Z3.Expr.expr =
    Z3.Boolean.mk_not ctx expr

  let single_implies (expr1 : Z3.Expr.expr) (expr2 : Z3.Expr.expr) : Z3.Expr.expr =
    Z3.Boolean.mk_implies ctx expr1 expr2

  let sub_set_eq (tau1 : Z3.Expr.expr) (tau2 : Z3.Expr.expr) : Z3.Expr.expr =
    let x = Z3.Arithmetic.Real.mk_const_s ctx "xs" in
    let y = Z3.Arithmetic.Real.mk_const_s ctx "ys" in
    Z3.Quantifier.mk_forall_const ctx [x; y] (Z3.Boolean.mk_implies ctx tau1 tau2) None [] [] None None

end

(* Module for temporal logic operations *)
module Temporal = struct
  (*Auxiliary function for the bellow until implementation:*)
  let until_aux (expr1_list : Z3.Expr.expr list) (expr2_list : Z3.Expr.expr list) (n : int) (m : int) : Z3.Expr.expr =
    if n >= List.length expr1_list || m >= List.length expr2_list then
      Z3.Boolean.mk_false ctx
    else
      let expr1_n = List.nth expr1_list n in
      let expr2_m = List.nth expr2_list m in
      Logic.single_and expr1_n expr2_m

  let rec until (expr1_list : Z3.Expr.expr list) (expr2_list : Z3.Expr.expr list) (n : int) : Z3.Expr.expr =
    if List.length expr1_list <= 1 || List.length expr2_list <= 1 || List.length expr1_list <> List.length expr2_list then
      invalid_arg "The lists must have the same size and length > 1"
    else if n + 2 = List.length expr1_list then
      Z3.Boolean.mk_false ctx
    else
      let aux = until_aux expr1_list expr2_list (n + 2) (n + 3) in
      let rest = until expr1_list expr2_list (n + 1) in
      Logic.single_or aux rest

  let previous (formula : Z3.Expr.expr) (repetitions : int) : Z3.Expr.expr =
    if formula = Z3.Boolean.mk_true ctx then
      Z3.Boolean.mk_true ctx
    else if formula = Z3.Boolean.mk_false ctx then
      Z3.Boolean.mk_false ctx
    else
      let n = Z3.FuncDecl.mk_func_decl ctx "previous" [Z3.Boolean.mk_sort ctx; Z3.Arithmetic.Integer.mk_sort ctx] (Z3.Boolean.mk_sort ctx) in
      Z3.Expr.mk_app ctx n [formula; Z3.Arithmetic.Integer.mk_numeral_i ctx repetitions]

  (*Auxiliary function for the bellow since implementation*)
  let rec since_aux (formula1 : Z3.Expr.expr) (formula2 : Z3.Expr.expr) (n : int) (m : int) : Z3.Expr.expr =
    if n = 0 then
      Logic.single_and formula1 formula2
    else if n > 0 then
      Logic.single_and (previous formula1 (n - 1)) (since_aux formula1 formula2 (n - 1) m)
    else if n = -1 then
      previous formula2 (m - 1)
    else
      Z3.Boolean.mk_false ctx

  let rec since_formula (formula1 : Z3.Expr.expr) (formula2 : Z3.Expr.expr) (bound : int) (m : int) : Z3.Expr.expr =
    if m >= bound then
      Z3.Boolean.mk_false ctx
    else if m + 2 <= bound then
      Logic.single_or
        (since_aux formula1 formula2 (m + 1) (m + 2))
        (since_formula formula1 formula2 bound (m + 1))
    else
      Z3.Boolean.mk_false ctx
end