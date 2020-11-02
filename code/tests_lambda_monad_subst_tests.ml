open Lambda_monad_subst
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

let%test "Monadfunction Intepreter App1" = eval app1 = (None, ("eval free variable", [Eval (Var "x")]))
let%test "Monadfunction Intepreter 2 App2" = eval app2 = (Some (IntV 2),
 ("",
  [Eval (Int 2); Subst ("x", IntV 2, Var "x"); Eval (Int 2);
   Eval (Fun ("x", Var "x"))]))
let%test "Monadfunction Intepreter 2 App3" =
  eval app3 = (None,
 ("tried to apply to non-function",
  [Eval (App (Int 2, Int 3)); Eval (Int 3); Eval (Int 2)]))
let%test "Monadfunction Intepreter App4" = 
  eval app4 = (None,
 ("e1 and/or e2 are not integers",
  [Eval (Binop (Add, Fun ("x", Var "x"), Int 1)); Eval (Int 1);
   Eval (Fun ("x", Var "x"));
   Subst ("x", FunV ("x", Var "x"), Binop (Add, Var "x", Int 1));
   Subst ("x", FunV ("x", Var "x"), Int 1);
   Subst ("x", FunV ("x", Var "x"), Var "x"); Eval (Fun ("x", Var "x"));
   Eval (Fun ("x", Binop (Add, Var "x", Int 1)))]))
let%test "Monadfunction Intepreter App5" = 
  eval app5 = (None,
 ("e1 and/or e2 are not integers",
  [Eval (Binop (Mul, Fun ("x", Var "x"), Int 2)); Eval (Int 2);
   Eval (Fun ("x", Var "x"));
   Subst ("x", FunV ("x", Var "x"), Binop (Mul, Var "x", Int 2));
   Subst ("x", FunV ("x", Var "x"), Int 2);
   Subst ("x", FunV ("x", Var "x"), Var "x"); Eval (Fun ("x", Var "x"));
   Eval (Fun ("x", Binop (Mul, Var "x", Int 2)));
   Subst ("y", IntV 2, Fun ("x", Binop (Mul, Var "x", Var "y")));
   Subst ("y", IntV 2, Binop (Mul, Var "x", Var "y"));
   Subst ("y", IntV 2, Var "y"); Subst ("y", IntV 2, Var "x"); Eval (Int 2);
   Eval (Fun ("y", Fun ("x", Binop (Mul, Var "x", Var "y"))))]))
let%test "Monadfunction Intepreter App6" = eval app6 = (Some (IntV 4),
 ("",
  [Eval (Binop (Add, Int 3, Int 1)); Eval (Int 1); Eval (Int 3);
   Subst ("x", IntV 3, Binop (Add, Var "x", Int 1));
   Subst ("x", IntV 3, Int 1); Subst ("x", IntV 3, Var "x"); Eval (Int 3);
   Eval (Fun ("x", Binop (Add, Var "x", Int 1)))]))
let%test "Monadfunction Intepreter App7" = eval app7 = (Some (IntV 6),
 ("",
  [Eval (Binop (Mul, Int 3, Int 2)); Eval (Int 2); Eval (Int 3);
   Subst ("x", IntV 3, Binop (Mul, Var "x", Int 2));
   Subst ("x", IntV 3, Int 2); Subst ("x", IntV 3, Var "x"); Eval (Int 3);
   Eval (Fun ("x", Binop (Mul, Var "x", Int 2)));
   Subst ("y", IntV 2, Fun ("x", Binop (Mul, Var "x", Var "y")));
   Subst ("y", IntV 2, Binop (Mul, Var "x", Var "y"));
   Subst ("y", IntV 2, Var "y"); Subst ("y", IntV 2, Var "x"); Eval (Int 2);
   Eval (Fun ("y", Fun ("x", Binop (Mul, Var "x", Var "y"))))]))
