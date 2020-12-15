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

let%test "Evaluate polymorphic id function" = 
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
