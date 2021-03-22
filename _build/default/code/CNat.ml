(*
Church numerals
Year 4 Capstone Project
Tram Hoang
 *)
open SystemFSig.SystemF0Signature

(*Church Numertals Type *)
let cNat: ty = TForAll ("X", TFunc (TFunc (TVar "X", TVar "X"), TFunc (TVar "X", TVar "X")))

(*Church Numerals Numbers *)
let czero = ETAbs ("X", Abs ("s", TFunc (TVar "X", TVar "X"), Abs ("z", TVar "X", Var "z")))
let cone = ETAbs ("X", 
                  Abs ("s", TFunc (TVar "X", TVar "X"), 
                       Abs ("z", TVar "X", App (Var "s", Var "z")))) 
let ctwo = ETAbs ("X", 
                  Abs ("s", TFunc (TVar "X", TVar "X"), 
                       Abs ("z", TVar "X", App (Var "s", App (Var "s", Var "z"))))) 

(*Church functions like successor, add, and times*)
(* 
let csucc = Abs ("n", cNat, ETAbs ("X", Abs ("s", TFunc (TVar "X" ,TVar "X"), 
              Abs ("z", TVar "X", 
                   App (Var "s", 
                        App (App ( ETApp (Var "n", Typ (TVar "X")), Var "s" ), Var "z"))))))

let csucc = Abs ("n", cNat, ETAbs ("X", Abs ("s", TFunc (TVar "X", TVar "X"), 
              Abs ("z", TVar "X", 
                   App (Var "s", App (ETApp (Var "n", Typ (TVar "X")), App (Var "s", Var "z"))
                        ))))) *)
