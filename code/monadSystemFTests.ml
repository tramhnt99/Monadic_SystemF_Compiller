open CNat
open MonadSystemFTypechecker
open MonadSystemFEval
open SystemFSig.SystemF0Signature
open MonadSystemFSig

module Evaluator = MonadicEvaluator(LogMonad)
module TypeChecker = MonadicTypeChecker(LogMonad)



(* ********************************* Evaluator tests ************************************** *)
let id_func = ETAbs ("X", Abs ("x", TVar "X", Var "x"))
let double = ETAbs ("X", Abs ("f", TFunc (TVar "X", TVar "X"), Abs ("a", TVar ("X"), App (Var "f", App (Var "f", Var "a")))))
let int_func = Abs ("k",TInt, Binop (Add, Var "k", Int 2))
let selfApp = Abs ("x", TForAll ("X", TFunc (TVar "X", TVar "X")), App (ETApp (Var "x", Typ (TForAll ("X", TFunc (TVar "X", TVar "X")))), Var "x"))

let empty_env : environment = {types = []; variables = []}

let eval' = (fun exp -> Evaluator.eval empty_env exp |> Evaluator.get_result)

let%test "Evaluate polymorphic id func ec tion" = 
  eval' (App (ETApp (id_func, Typ TInt), Int 1)) = Some (IntV 1)
let%test "Evaluate double for all polymorphic \"X\" to become Int" =
  eval' (ETApp (double, Typ TInt)) = 
    Some (Closure ({types = [("X", TypV TInt)]; variables = []}, Some "f",
                   TFunc (TInt, TInt), Abs ("a", TInt, App (Var "f", App (Var "f", Var "a")))))
let%test "Evalute double applied to Int function" =
  eval' (App (App (double, int_func), Int 2)) = Some (IntV 6)

(* ********************************* Typechecker tests ************************************** *)
let typecheck_correct = (fun res -> 
    let temp = TypeChecker.get_result res in
    if Option.is_some temp then Option.get temp |> fst
    else failwith "Typechecker didn't pass")

let type_of_exp = TypeChecker.type_of_exp



let%test "Type of id function should be polymorphic and correct" =
  type_of_exp empty_env id_func |> typecheck_correct = TForAll ("X", TFunc (TVar "X", TVar "X"))
let%test "Instatiating Type of id function" =
  type_of_exp empty_env (ETApp (id_func, Typ TInt)) |> typecheck_correct = TFunc (TInt, TInt)
let%test "Id function evaluted or not should have the same type" =
  type_of_exp empty_env (ETApp (id_func, Typ TInt)) |> typecheck_correct = 
    let res = eval' (ETApp (id_func, Typ TInt)) |> Option.get in
    TypeChecker.type_of_value empty_env res |> TypeChecker.get_result |> Option.get
let%test "Function type of double function is polymorphic and correct" =
  type_of_exp empty_env double |> typecheck_correct =
    TForAll ("X", TFunc (TFunc (TVar "X", TVar "X"), TFunc (TVar "X", TVar "X")))
let%test "Instatiating types in function double" =
  type_of_exp empty_env (ETApp (double, Typ TInt)) |> typecheck_correct =
    TFunc (TFunc (TInt, TInt), TFunc (TInt, TInt))
let%test "Typecheck double applied to Int function" =
  type_of_exp empty_env (App (App (double, int_func), Int 2)) |> typecheck_correct =
    TFunc (TInt, TFunc (TInt, TInt))
let%test "Typecheck ETApp error" =
  type_of_exp empty_env (ETApp (Abs ("x", TVar "X", id_func), Int 1)) |> TypeChecker.get_result = None
let%test "Typecheck App error" =
  type_of_exp empty_env (App (Int 3, Int 1)) |> TypeChecker.get_result = None
let%test "Typecheck applying types to a wrong type" =
  type_of_exp empty_env (App (Abs ("x", TFunc (TVar "X", TVar "x"), Var "x"), Int 2)) |> TypeChecker.get_result = None
let%test "SelfApp Typing is correct" = 
  type_of_exp empty_env selfApp |> typecheck_correct =
    TFunc (TForAll ("X", TFunc (TVar "X", TVar "X")),
    TForAll ("X", TFunc (TVar "X", TVar "X")))
      (* (For all X. X -> X) -> (For all X. X -> X)*) (*pg. 136 *)



(* ******************************* Church numerals tests ************************************ *)
let%test "Czero is of type cNat" = 
  type_of_exp empty_env czero |> typecheck_correct = cNat
let%test "Cone is of type cNat" = 
  type_of_exp empty_env cone |> typecheck_correct = cNat
let%test "Ctwo is of type cNat" = 
  type_of_exp empty_env ctwo |> typecheck_correct = cNat



(* ***************************** CPS Monad tests ******************************************** *)
open CPSMonadicEvaluator

let%test "CPS Monad test1" =
  eval_without_cps (App (ETApp (id_func, Typ TInt), Int 1)) empty_env =
    (Ok (IntV 1, {types = [("X", TypV TInt)]; variables = [("x", IntV 1)]}),
 [Eval (ETAbs ("X", Abs ("x", TVar "X", Var "x"))); Eval (Typ TInt);
  Eval (Abs ("x", TVar "X", Var "x")); Eval (Int 1); Eval (Var "x")])

let%test "CPS Monad test2" = 
  eval_without_cps (ETApp (double, Typ TInt)) empty_env =
    (Ok
  (Closure ({types = [("X", TypV TInt)]; variables = []}, Some "f",
    TFunc (TInt, TInt), Abs ("a", TInt, App (Var "f", App (Var "f", Var "a")))),
   {types = [("X", TypV TInt)]; variables = []}),
 [Eval
   (ETAbs ("X",
     Abs ("f", TFunc (TVar "X", TVar "X"),
      Abs ("a", TVar "X", App (Var "f", App (Var "f", Var "a"))))));
  Eval (Typ TInt);
  Eval
   (Abs ("f", TFunc (TVar "X", TVar "X"),
     Abs ("a", TVar "X", App (Var "f", App (Var "f", Var "a")))))])

let%test "CPSMonad test3" =
  eval_without_cps (App (App (double, int_func), Int 2)) empty_env =
    (Ok
  (IntV 6,
   {types = [];
    variables =
     [("k", IntV 4); ("a", IntV 2);
      ("f",
       Closure ({types = []; variables = []}, Some "k", TInt,
        Binop (Add, Var "k", Int 2)))]}),
 [Eval
   (ETAbs ("X",
     Abs ("f", TFunc (TVar "X", TVar "X"),
      Abs ("a", TVar "X", App (Var "f", App (Var "f", Var "a"))))));
  Eval (Abs ("k", TInt, Binop (Add, Var "k", Int 2)));
  Eval
   (Abs ("f", TFunc (TVar "X", TVar "X"),
     Abs ("a", TVar "X", App (Var "f", App (Var "f", Var "a")))));
  Eval (Abs ("k", TInt, Binop (Add, Var "k", Int 2)));
  Eval (Abs ("a", TVar "X", App (Var "f", App (Var "f", Var "a"))));
  Eval (Int 2); Eval (Abs ("k", TInt, Binop (Add, Var "k", Int 2)));
  Eval (Abs ("k", TInt, Binop (Add, Var "k", Int 2))); Eval (Var "a");
  Eval (Var "k"); Eval (Int 2); Eval (Binop (Add, Var "k", Int 2));
  Eval (Var "k"); Eval (Int 2); Eval (Binop (Add, Var "k", Int 2))])

let%test "CPSMonad testError1" =
  eval_without_cps (App (Int 2, Int 3 )) empty_env =
    (Error "App to a non function", [Eval (Int 2); Eval (Int 3)])