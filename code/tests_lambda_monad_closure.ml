open Lambda_monad_closure
open LambdaMonadClosure

(*Testing Monad Closure Lambda Caluculus*)
let func1 = Fun ("x", Var "x")
let func2 = Fun ("x", Binop (Add, Var "x", Int 1))
let func3 = Fun ("y", Fun ("x", Binop (Mul, Var ("x"), Var ("y"))))

let app1 = App (Var "x", Var "y")
let app2 = App (func1, Int 2)
let app3 = App (func1, Var "x")
let app4 = App (func2, func1)
let app5 = App (App (func3, Int 2), func1)
let app6 = App ( func2, Int 3)
let app7 = App (App ( func3 , Int 2), Var "x")

let env1 = [("x", IntV 3); ("y", IntV 42)]


let%test "Monad Lambda Closure App1" = 
  eval env1 app1 = 
    (None,
     [Error "tried to apply non-function";
      Lookup ("y", [("x", IntV 3); ("y", IntV 42)]);
      Lookup ("x", [("x", IntV 3); ("y", IntV 42)])])
let%test "Monad Lambda Closure App2" = 
  eval [] app2 = 
    (Some (IntV 2),
     [Lookup ("x", [("x", IntV 2)]); Eval ([], Int 2);
      Eval ([], Fun ("x", Var "x"))])
let%test "Monad Lambda Closure App3" = 
  eval env1 app3 = 
    (Some (IntV 3),
     [Lookup ("x", [("x", IntV 3); ("x", IntV 3); ("y", IntV 42)]);
      Lookup ("x", [("x", IntV 3); ("y", IntV 42)]);
      Eval ([("x", IntV 3); ("y", IntV 42)], Fun ("x", Var "x"))])
let%test "Monad Lambda Closure App4" = 
  eval [] app4 =
    (None,
     [Error "tried to binop non-integers";
      Eval ([("x", Closure ([], "x", Var "x"))], Int 1);
      Lookup ("x", [("x", Closure ([], "x", Var "x"))]);
      Eval ([], Fun ("x", Var "x"));
      Eval ([], Fun ("x", Binop (Add, Var "x", Int 1)))])
let%test "Monad Lambda Closure App5" = 
  eval [] app5 =
    (None,
     [Error "tried to binop non-integers";
      Lookup ("y", [("x", Closure ([], "x", Var "x")); ("y", IntV 2)]);
      Lookup ("x", [("x", Closure ([], "x", Var "x")); ("y", IntV 2)]);
      Eval ([], Fun ("x", Var "x"));
      Eval ([("y", IntV 2)], Fun ("x", Binop (Mul, Var "x", Var "y")));
      Eval ([], Int 2);
      Eval ([], Fun ("y", Fun ("x", Binop (Mul, Var "x", Var "y"))))])
let%test "Monad Lambda Closure App6" =
  eval [] app6 =
    (Some (IntV 4),
     [Eval ([("x", IntV 3)], Binop (Add, Var "x", Int 1));
      Eval ([("x", IntV 3)], Int 1); Lookup ("x", [("x", IntV 3)]);
      Eval ([], Int 3); Eval ([], Fun ("x", Binop (Add, Var "x", Int 1)))])
let%test "Monad Lambda Closure App7" = 
  eval env1 app7 =
    (Some (IntV 6),
     [Eval ([("x", IntV 3); ("y", IntV 2); ("x", IntV 3); ("y", IntV 42)],
            Binop (Mul, Var "x", Var "y"));
      Lookup ("y", [("x", IntV 3); ("y", IntV 2); ("x", IntV 3); ("y", IntV 42)]);
      Lookup ("x", [("x", IntV 3); ("y", IntV 2); ("x", IntV 3); ("y", IntV 42)]);
      Lookup ("x", [("x", IntV 3); ("y", IntV 42)]);
      Eval ([("y", IntV 2); ("x", IntV 3); ("y", IntV 42)],
            Fun ("x", Binop (Mul, Var "x", Var "y")));
      Eval ([("x", IntV 3); ("y", IntV 42)], Int 2);
      Eval ([("x", IntV 3); ("y", IntV 42)],
            Fun ("y", Fun ("x", Binop (Mul, Var "x", Var "y"))))])
      
