(*
Monadic System F Interpreter - TypeChecker
Year 4 Capstone Project
Tram Hoang
*)

module MonadSystemFEvaluator = struct
  open Monad_systemF_typechecker.MonadSystemFTypeChecker
  open Monad_systemF_sig.MonadSystemFSignature
  open Monad_systemF_helpers.MonadSystemFHelpers
  open SystemF_sig.SystemF0Signature

  (*Evaluate statements to a final value*)
  let rec eval_help (env: environment) (t: exp) : value t = 
    let name = Eval (env, t) in
    let ret = fun res -> return res name in
    match t with
    | Int i -> IntV i |> ret
    | Typ ty -> TypV ty |> ret
    | Var v -> lookup_var v env >>= fun nv -> Option.get nv |> ret
    | ETVar tv -> lookup_var tv env >>= fun nv -> Option.get nv |> ret
    | ETAbs (tv, exp)-> Closure (env, None, TVar tv, exp) |> ret
    | ETApp (e1, e2) ->
       eval_help env e1 >>= fun lhs ->
       eval_help env e2 >>= fun rhs ->
       (match lhs, rhs with
        | (Closure (cenv, _, TVar x, body), TypV ty') -> 
           let new_env = {types = (x, TypV ty') :: cenv.types; 
                          variables = cenv.variables}
           in
           eval_help new_env body
        | _ -> None, [Error "App to a non closure/tried to apply a non type"]
       )
    | Abs (v, ty, exp) -> 
       propTy env ty >>= fun nty ->
       propValTy env exp >>= fun ne ->
       Closure (env, Some v, nty, ne) |> ret
    | App (e1, e2) ->
       eval_help env e1 >>= fun lhs ->
       eval_help env e2 >>= fun rhs ->
       (match lhs, rhs with
        | Closure (cenv, Some x, _, body), v ->
           let new_env = { types = cenv.types
                         ; variables = (x,v) :: cenv.variables}
           in
           eval_help new_env body
        | Closure (cenv, None, _, body), _ -> 
           eval_help cenv (App (body, e2))
        (*application to a polymorphic type, just continue*)
        | _ -> None, [Error "App to a non function"]
       )
    | Binop (b, e1, e2) ->
       eval_help env e1 >>= fun lhs ->
       eval_help env e2 >>= fun rhs ->
       (match lhs, rhs with
        | IntV i1, IntV i2 ->
           (match b with
            | Add -> IntV (i1 + i2) |> ret
            | Sub -> IntV (i1 - i2) |> ret
            | Mul -> IntV (i1 * i2) |> ret
            | Div -> IntV (i1 / i2) |> ret
           )
        | _ -> None, [Error "Binop applied to non-Int type"]
       ) 

  (*Evaluate with type-checking *)
  let eval (env': environment) (t': exp) : value t =
    (*Typecheck the program before running*)
    type_of_exp empty_env t' >>= fun _ ->
    eval_help env' t'

end
