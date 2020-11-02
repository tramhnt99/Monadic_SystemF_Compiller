open SystemF
open SystemF0


let id_func = TAbs ("X", Abs ("x", TVar "X", Var "x"))

let%test "Evaluate polymorphic id function" = 
  eval [] (App ( TApp (id_func, Typ Int), Int 1)) = IntV 1
