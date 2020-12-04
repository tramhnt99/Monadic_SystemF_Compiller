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

let empty_env : environment = {types = []; variables = []}

let eval' = (fun exp -> eval empty_env exp)

let%test "Evaluate polymorphic id function" = 
  eval' (App (ETApp (id_func, Typ TInt), Int 1)) |> fst = Some (IntV 1)
let%test "Evaluate double for all polymorphic \"X\" to become Int" =
  eval' (ETApp (double, Typ TInt)) |> fst = 
    Some (Closure ({types = [("X", TypV TInt)]; variables = []}, Some "f",
                   TFunc (TInt, TInt), Abs ("a", TInt, App (Var "f", App (Var "f", Var "a")))))
let%test "Evalute double applied to Int function" =
  eval' (App (App (double, int_func), Int 2)) |> fst = Some (IntV 6)

(* ********************************* Typechecker tests ************************************** *)
let typecheck_correct = fun res -> res |> fst |> Option.get |> fst

let%test "Type of id function should be polymorphic and correct" =
  type_of_exp empty_env id_func |> typecheck_correct = TForAll ("X", TFunc (TVar "X", TVar "X"))
let%test "Instatiating Type of id function" =
  type_of_exp empty_env (ETApp (id_func, Typ TInt)) |> typecheck_correct = TFunc (TInt, TInt)
let%test "Id function evaluted or not should have the same type" =
  type_of_exp empty_env (ETApp (id_func, Typ TInt)) |> typecheck_correct = 
    ((eval' (ETApp (id_func, Typ TInt)) >>= fun res ->
    type_of_value empty_env res) |> fst |> Option.get)
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
  type_of_exp empty_env (ETApp (Abs ("x", TVar "X", id_func), Int 1)) = 
    (None, [Error "ETApp applied e2 is not a type"])
let%test "Typecheck App error" =
  type_of_exp empty_env (App (Int 3, Int 1)) = 
    (None,
 [Error "Wrong application types. Failed the T-App rule with Type TInt ";
  TypeOfExp (empty_env, Int 1); TypeOfExp (empty_env, Int 3)])
let%test "Typecheck applying types to a wrong type" =
  type_of_exp empty_env (App (Abs ("x", TFunc (TVar "X", TVar "x"), Var "x"), Int 2)) = 
    (None,
     [Error
        "Wrong application types. Failed the T-App rule where ty_param is TFunc TVar X TVar x and e2_ty is TInt ";
      TypeOfExp ({types = []; variables = []}, Int 2);
      TypeOfExp ({types = []; variables = []},
                 Abs ("x", TFunc (TVar "X", TVar "x"), Var "x"));
      TypeOfExp ({types = [("x", TypV (TFunc (TVar "X", TVar "x")))]; variables = []}, Var "x");
      LookupType ("x",{types = [("x", TypV (TFunc (TVar "X", TVar "x")))]; variables = []});
      PropTy ({types = []; variables = []}, TFunc (TVar "X", TVar "x"));
      LookupType ("x", {types = []; variables = []});
      LookupType ("X", {types = []; variables = []})])
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
