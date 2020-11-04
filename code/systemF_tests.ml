open SystemF_TypeChecker
open SystemF0TypeChecker
open SystemF_eval
open SystemF0Evaluator
open SystemF_sig
open SystemF0Signature

let id_func = ETAbs ("X", Abs ("x", TVar "X", Var "x"))
let double = ETAbs ("X", Abs ("f", TFunc (TVar "X", TVar "X"), Abs ("a", TVar ("X"), App (Var "f", App (Var "f", Var "a")))))
let int_func = Abs ("k",TInt, Binop (Add, Var "k", Int 2))


(*Evalution tests *)
let eval' = (fun exp -> eval [] exp) 

let%test "Evaluate polymorphic id function" = 
  eval' (App (ETApp (id_func, Typ TInt), Int 1)) = IntV 1
let%test "Evaluate double for all polymorphic \"X\" to become Int" =
  eval' (ETApp (double, Typ TInt)) = 
    Closure ([("X", TypV TInt)], Some "f",
             TFunc (TInt, TInt),
             Abs ("a", TInt, App (Var "f", App (Var "f", Var "a"))))
let%test "Evalute double applied to Int function" =
  eval' (App (App (double, int_func), Int 2)) = IntV 6


(*Typechecker tests*)
let%test "Type of id function should be polymorphic and correct" =
  type_of_exp [] id_func |> fst = TForAll ("X", TFunc (TVar "X", TVar "X"))
let%test "Instatiating Type of id function" =
  type_of_exp [] (ETApp (id_func, Typ TInt)) |> fst = TFunc (TInt, TInt)
let%test "Id function evaluted or not should have the same type" =
  type_of_exp [] (ETApp (id_func, Typ TInt)) |> fst = 
    type_of_value [] (eval' (ETApp (id_func, Typ TInt)))
let%test "Function type of double function is polymorphic and correct" =
  type_of_exp [] double |> fst =
    TForAll ("X", TFunc (TFunc (TVar "X", TVar "X"), TFunc (TVar "X", TVar "X")))
let%test "Instatiating types in function double" =
  type_of_exp [] (ETApp (double, Typ TInt)) |> fst =
    TFunc (TFunc (TInt, TInt), TFunc (TInt, TInt))
