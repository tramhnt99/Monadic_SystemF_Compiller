open Monad_lambda_subst
open LambdaMonad

(*Testing Monad Lambda Caluculus for Propagating Errors *)
let func1 = Fun ("x", Var "x")
let func2 = Fun ("x", Binop (Add, Var "x", Int 1))
let func3 = Fun ("y", Fun ("x", Binop (Mul, Var ("x"), Var ("y"))))

let app1 = App (Var "x", Var "y")
let app2 = App (func1, Int 2)
let app3 = App (Int 2, Int 3)
let app4 = App (func2, func1)
let app5 = App (App (func3, Int 2), func1)
let app6 = App ( func2, Int 3)
let app7 = App (App ( func3 , Int 2), Int 3)

let%test "Monadfunction Intepreter 2 App1" = eval app1 = (None, ("eval free variable", ["eval Var x"]))
let%test "Monadfunction Intepreter 2 App2" = eval app2 = (Some (IntV 2),
 ("", ["eval Int 2"; "subst x IntV 2Var x"; "eval Int 2"; "eval Fun xVar x"]))
let%test "Monadfunction Intepreter 2 App3" =
  eval app3 = (None,
 ("tried to apply to non-function",
  ["eval App Int 2, Int 3"; "eval Int 3"; "eval Int 2"]))
let%test "Monadfunction Intepreter App4" = 
  eval app4 = (None,
 ("e1 and/or e2 are not integers",
  ["eval Binop Add , Fun xVar x, Int 1"; "eval Int 1"; "eval Fun xVar x";
   "subst x FunV xVar xBinop Add , Var x, Int 1"; "subst x FunV xVar xInt 1";
   "subst x FunV xVar xVar x"; "eval Fun xVar x";
   "eval Fun xBinop Add , Var x, Int 1"]))
let%test "Monadfunction Intepreter App5" = 
  eval app5 = (None,
 ("e1 and/or e2 are not integers",
  ["eval Binop Mul , Fun xVar x, Int 2"; "eval Int 2"; "eval Fun xVar x";
   "subst x FunV xVar xBinop Mul , Var x, Int 2"; "subst x FunV xVar xInt 2";
   "subst x FunV xVar xVar x"; "eval Fun xVar x";
   "eval Fun xBinop Mul , Var x, Int 2";
   "subst y IntV 2Fun xBinop Mul , Var x, Var y";
   "subst y IntV 2Binop Mul , Var x, Var y"; "subst y IntV 2Var y";
   "subst y IntV 2Var x"; "eval Int 2";
   "eval Fun yFun xBinop Mul , Var x, Var y"]))
let%test "Monadfunction Intepreter App6" = eval app6 = (Some (IntV 4),
 ("",
  ["eval Binop Add , Int 3, Int 1"; "eval Int 1"; "eval Int 3";
   "subst x IntV 3Binop Add , Var x, Int 1"; "subst x IntV 3Int 1";
   "subst x IntV 3Var x"; "eval Int 3"; "eval Fun xBinop Add , Var x, Int 1"]))
let%test "Monadfunction Intepreter App7" = eval app7 = (Some (IntV 6),
 ("",
  ["eval Binop Mul , Int 3, Int 2"; "eval Int 2"; "eval Int 3";
   "subst x IntV 3Binop Mul , Var x, Int 2"; "subst x IntV 3Int 2";
   "subst x IntV 3Var x"; "eval Int 3"; "eval Fun xBinop Mul , Var x, Int 2";
   "subst y IntV 2Fun xBinop Mul , Var x, Var y";
   "subst y IntV 2Binop Mul , Var x, Var y"; "subst y IntV 2Var y";
   "subst y IntV 2Var x"; "eval Int 2";
   "eval Fun yFun xBinop Mul , Var x, Var y"]))
