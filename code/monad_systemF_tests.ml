open Monad_systemF_typechecker.MonadSystemFTypeChecker
open Monad_systemF_eval.MonadSystemFEvaluator
open SystemF_sig.SystemF0Signature
open Monad_systemF_sig.MonadSystemFSignature
open CNat



(* ********************************* Evaluator tests ************************************** *)
let id_func = ETAbs ("X", Abs ("x", TVar "X", Var "x"))
let double = ETAbs ("X", Abs ("f", TFunc (TVar "X", TVar "X"), Abs ("a", TVar ("X"), App (Var "f", App (Var "f", Var "a")))))
let int_func = Abs ("k",TInt, Binop (Add, Var "k", Int 2))
let selfApp = Abs ("x", TForAll ("X", TFunc (TVar "X", TVar "X")), App (ETApp (Var "x", Typ (TForAll ("X", TFunc (TVar "X", TVar "X")))), Var "x"))

let%test "Evaluate polymorphic id function" = 
  eval [] (App (ETApp (id_func, Typ TInt), Int 1)) |> fst = Some (IntV 1)
let%test "Evaluate double for all polymorphic \"X\" to become Int" =
  eval [] (ETApp (double, Typ TInt)) |> fst = 
    Some (Closure ([("X", TypV TInt)], Some "f",
             TFunc (TInt, TInt),
             Abs ("a", TInt, App (Var "f", App (Var "f", Var "a")))))
let%test "Evalute double applied to Int function" =
  eval [] (App (App (double, int_func), Int 2)) |> fst = Some (IntV 6)



(* ********************************* Typechecker tests ************************************** *)
let typecheck_correct = fun res -> res |> fst |> Option.get |> fst

let%test "Type of id function should be polymorphic and correct" =
  type_of_exp [] id_func |> typecheck_correct = TForAll ("X", TFunc (TVar "X", TVar "X"))
let%test "Instatiating Type of id function" =
  type_of_exp [] (ETApp (id_func, Typ TInt)) |> typecheck_correct = TFunc (TInt, TInt)
let%test "Id function evaluted or not should have the same type" =
  type_of_exp [] (ETApp (id_func, Typ TInt)) |> typecheck_correct = 
    ((eval [] (ETApp (id_func, Typ TInt)) >>= fun res ->
    type_of_value [] res) |> fst |> Option.get)
let%test "Function type of double function is polymorphic and correct" =
  type_of_exp [] double |> typecheck_correct =
    TForAll ("X", TFunc (TFunc (TVar "X", TVar "X"), TFunc (TVar "X", TVar "X")))
let%test "Instatiating types in function double" =
  type_of_exp [] (ETApp (double, Typ TInt)) |> typecheck_correct =
    TFunc (TFunc (TInt, TInt), TFunc (TInt, TInt))
let%test "Typecheck double applied to Int function" =
  type_of_exp [] (App (App (double, int_func), Int 2)) |> typecheck_correct =
    TFunc (TInt, TFunc (TInt, TInt))
let%test "Typecheck ETApp error" =
  type_of_exp [] (ETApp (Abs ("x", TVar "X", id_func), Int 1)) = 
    (None, [Error "ETApp applied e2 is not a type"])
let%test "Typecheck App error" =
  type_of_exp [] (App (Int 3, Int 1)) = 
    (None,
 [Error "Wrong application types. Failed the T-App rule with Type TInt ";
  TypeOfExp ([], Int 1); TypeOfExp ([], Int 3)])
let%test "Typecheck applying types to a wrong type" =
  type_of_exp [] (App (Abs ("x", TFunc (TVar "X", TVar "x"), Var "x"), Int 2)) = 
    (None,
     [Error
        "Wrong application types. Failed the T-App rule where ty_param is TFunc TVar X TVar x and e2_ty is TInt ";
      TypeOfExp ([], Int 2);
      TypeOfExp ([], Abs ("x", TFunc (TVar "X", TVar "x"), Var "x"));
      TypeOfExp ([("x", TypV (TFunc (TVar "X", TVar "x")))], Var "x");
      GetTypV (TypV (TFunc (TVar "X", TVar "x")));
      Lookupty ("x", [("x", TypV (TFunc (TVar "X", TVar "x")))]);
      EvalAbsTy ([], TFunc (TVar "X", TVar "x")); Lookupty ("x", []);
      Lookupty ("X", [])])
let%test "SelfApp Typing is correct" = 
  type_of_exp [] selfApp |> typecheck_correct =
    TFunc (TForAll ("X", TFunc (TVar "X", TVar "X")),
    TForAll ("X", TFunc (TVar "X", TVar "X")))
      (* (For all X. X -> X) -> (For all X. X -> X)*) (*pg. 136 *)



(* ******************************* Church numerals tests ************************************ *)
let%test "Czero is of type cNat" = 
  type_of_exp [] czero |> typecheck_correct = cNat
let%test "Cone is of type cNat" = 
  type_of_exp [] cone |> typecheck_correct = cNat
let%test "Ctwo is of type cNat" = 
  type_of_exp [] ctwo |> typecheck_correct = cNat




(*Write testing for breaking the typechecker/evaluator. e.g. app to wrong type*)
(*
open Monad_systemF_typechecker.MonadSystemFTypeChecker;;
open Monad_systemF_eval.MonadSystemFEvaluator;;
open SystemF_sig.SystemF0Signature;;
open Monad_systemF_sig.MonadSystemFSignature;;
open Monad_systemF_tests;;
open CNat;;

type_of_exp [("x", TypV (TForAll ("X", TFunc (TVar "X", TVar "X"))))] 
(ETApp (Var "x", Typ (TForAll ("X", TFunc (TVar "X", TVar "X")))));;
 *)
