(*
Monadic System F Interpreter - TypeChecker
Year 4 Capstone Project
Tram Hoang
*)

open MonadSystemFTypechecker
open MonadSystemFHelpers
open SystemFSig.SystemF0Signature
open MonadSystemFSig

module type Evaluator = 
  sig
    type 'a monad
    val eval_without_tc: environment -> exp -> value monad
    val eval: environment -> exp -> value monad
    val get_result: 'a monad -> 'a option
  end

module MonadicEvaluator (Monad: MonadAbstractSig): Evaluator = struct

  module TypeChecker = MonadicTypeChecker(Monad) 
  let (>>=) = Monad.(>>=)
  let get_result = Monad.get_result
  type 'a monad = 'a Monad.monad
            
  (*Evaluate statements to a final value*)
  let rec eval_without_tc (env: environment) (t: exp) : value monad = 
    let name = Eval t in
    let ret = fun res -> Monad.return res name (Some env) in
    match t with
    | Int i -> IntV i |> ret
    | Typ ty -> TypV ty |> ret
    | Var v -> 
       let nv = lookup_var v env in
       if Option.is_some nv then Option.get nv |> ret
       else Monad.return_error "Var doesn't exist in the environment" env
    | ETVar tv -> 
       let nv = lookup_ty tv env in
       if Option.is_some nv then TypV (Option.get nv) |> ret 
       else Monad.return_error "Type var doesn't exist in the environment" env
    | ETAbs (tv, exp)-> Closure (env, None, TVar tv, exp) |> ret
    | ETApp (e1, e2) ->
       eval_without_tc env e1 >>= fun lhs ->
       eval_without_tc env e2 >>= fun rhs ->
       (match lhs, rhs with
        | (Closure (cenv, _, TVar x, body), TypV ty') -> 
           let new_env = {types = (x, TypV ty') :: cenv.types; 
                          variables = cenv.variables}
           in
           (* logState t env >>= fun _ ->  *)
           eval_without_tc new_env body
        | _ -> Monad.return_error "App to a non closure/tried to apply a non type" env
       )
    | Abs (v, ty, exp) ->
       Closure (env, Some v, propTy env ty, propValTy env exp) |> ret
    | App (e1, e2) ->
       eval_without_tc env e1 >>= fun lhs ->
       eval_without_tc env e2 >>= fun rhs ->
       (match lhs, rhs with
        | Closure (cenv, Some x, _, body), v ->
           let new_env = { types = cenv.types
                         ; variables = (x,v) :: cenv.variables}
           in
           eval_without_tc new_env body
        | Closure (cenv, None, _, body), _ -> 
           eval_without_tc cenv (App (body, e2))
        (*application to a polymorphic type, just continue*)
        | _ -> Monad.return_error "App to a non function" env
       )
    | Binop (b, e1, e2) ->
       eval_without_tc env e1 >>= fun lhs ->
       eval_without_tc env e2 >>= fun rhs ->
       (match lhs, rhs with
        | IntV i1, IntV i2 ->
           (match b with
            | Add -> IntV (i1 + i2) |> ret
            | Sub -> IntV (i1 - i2) |> ret
            | Mul -> IntV (i1 * i2) |> ret
            | Div -> IntV (i1 / i2) |> ret
           )
        | _ -> Monad.return_error "Binop applied to non-Int type" env
       ) 

  (*Evaluate with type-checking *)
  let eval (env: environment) (t: exp) : value monad =
    (*Typecheck the program before running*)
    let _ = TypeChecker.type_of_exp TypeChecker.empty_env t in
    eval_without_tc env t

end
