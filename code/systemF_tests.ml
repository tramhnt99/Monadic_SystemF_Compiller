open SystemF
open SystemF0


let id_func = ETAbs ("X", Abs ("x", TVar "X", Var "x"))
let double = ETAbs ("X", Abs ("f", TFunc (TVar "X", TVar "X"), Abs ("a", TVar ("X"), App (Var "f", App (Var "f", Var "a")))))
let int_func = Abs ("k",TInt, Binop (Add, Var "k", Int 2))


(*Evalution tests *)
let%test "Evaluate polymorphic id function" = 
  eval [] (App ( ETApp (id_func, Typ TInt), Int 1)) = IntV 1
let%test "Evaluate double for all polymorphic \"X\" to become Int" =
  eval [] (ETApp (double, Typ TInt)) = 
    Closure ([("X", TypV TInt)], Some "f",
             TFunc (TInt, TInt),
             Abs ("a", TInt, App (Var "f", App (Var "f", Var "a"))))
let%test "Evalute double applied to Int function" =
  eval [] (App (App (double, int_func), Int 2)) = IntV 6


(*Typechecker tests*)
