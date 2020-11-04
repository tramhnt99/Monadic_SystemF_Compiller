open SystemF
open SystemF0


let id_func = TAbs ("X", Abs ("x", TVar "X", Var "x"))
let double = TAbs ("X", Abs ("f", TFunc (TVar "X", TVar "X"), Abs ("a", TVar ("X"), App (Var "f", App (Var "f", Var "a")))))
let int_func = Abs ("k",Int, Binop (Add, Var "k", Int 2))



let%test "Evaluate polymorphic id function" = 
  eval [] (App ( TApp (id_func, Typ Int), Int 1)) = IntV 1
let%test "Evaluate double for all polymorphic \"X\" to become Int" =
  eval [] (TApp (double, Typ Int)) = 
    Closure ([("X", TypV SystemF.SystemF0.Int)], Some "f",
             TFunc (SystemF.SystemF0.Int, SystemF.SystemF0.Int),
             Abs ("a", SystemF.SystemF0.Int, App (Var "f", App (Var "f", Var "a"))))
let%test "Evalute double applied to Int function" =
  eval [] (App (App (double, int_func), Int 2)) = IntV 6
