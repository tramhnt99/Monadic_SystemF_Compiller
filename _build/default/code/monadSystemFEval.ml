(*
Monadic System F Interpreter - TypeChecker
Year 4 Capstone Project
Tram Hoang

MonadicEvaluator
WrapperEvaluator
*)

open MonadSystemFTypechecker
open MonadSystemFHelpers
open SystemFSig.SystemF0Signature
open MonadSystemFSig

module type Evaluator = 
  sig
    type 'a monad
    type m 
    val eval_without_tc: environment -> exp -> value monad
    val eval: environment -> exp -> value monad
    val get_result: 'a monad -> 'a option
    val get_monad: 'a monad -> m
    val monad_to_string: m -> string
  end

module MonadicEvaluator (Monad: MonadAbstractSig): Evaluator = struct
  module TypeChecker = MonadicTypeChecker(Monad) 
  type 'a monad = 'a Monad.monad
  type m = Monad.m
  let (>>=) = Monad.(>>=)
  let get_result = Monad.get_result
  let get_monad = Monad.get_monad
  let monad_to_string = Monad.monad_to_string
            
  (*Evaluate statements to a final value*)
  let rec eval_without_tc (env: environment) (t: exp) : value monad = 
    let name = Eval t in
    let ret = fun res -> Monad.return res name env in
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


module CPSMonadicEvaluator = struct
  module C = CPSLogMonad 
  type ('a, 'b) monad = ('a, 'b) C.monad
  let (>>=) = C.(>>=)
  let return = C.return
  let update_w = C.update_wrapper
               
  (*Using continuation passing style*)
  (*TODO: make lookup_var monadic again!!*)
  let rec eval_without_tc (t:exp) (env: environment) : (value * environment, log list -> ('c, 'd) result) monad =
    let name = Eval t in
    match t with
    | Int i -> return (IntV i, env)
    | Typ ty -> return (TypV ty, env)
    | Var v ->
      let nv = lookup_var v env in
      let thunk () = return (Option.get nv, env) in
      if Option.is_some nv then update_w thunk [name]
      else C.return_error "Var doesn't exist in the environment"
    | ETVar tv -> 
      let nv = lookup_ty tv env in
      let thunk () = return (TypV (Option.get nv), env) in
      if Option.is_some nv then update_w thunk [name]
      else C.return_error "Type var doesn't exist in the environment"
    | ETAbs (tv, exp) -> 
      return (Closure (env, None, TVar tv, exp), env)
    | ETApp (e1, e2) ->
      eval_without_tc e1 env >>= fun lhs ->
      eval_without_tc e2 env >>= fun rhs ->
      (match fst lhs, fst rhs with
       | (Closure (cenv, _, TVar x, body), TypV ty') -> 
           let new_env = {types = (x, TypV ty') :: cenv.types; 
                          variables = cenv.variables}
           in 
         eval_without_tc body new_env 
       | _ -> C.return_error "App to a non closure/tried to apply a non type"
      )
    | Abs (v, ty, exp) -> 
      return (Closure (env, Some v, propTy env ty, propValTy env exp), env)
    | App (e1 ,e2) -> 
      eval_without_tc e1 env >>= fun lhs ->
      eval_without_tc e2 env >>= fun rhs ->
      (match fst lhs, fst rhs with 
      | Closure (cenv, Some x, _, body), v -> 
           let new_env = { types = cenv.types
                         ; variables = (x,v) :: cenv.variables}
           in
           eval_without_tc body new_env
      | Closure (cenv, None, _, body), _ -> 
           eval_without_tc (App (body, e2)) cenv
        (*application to a polymorphic type, just continue*)
      | _ -> C.return_error "App to a non function")
    | Binop (b, e1, e2) ->
       eval_without_tc e1 env >>= fun lhs ->
       eval_without_tc e2 env >>= fun rhs ->
       (match fst lhs, fst rhs with
        | IntV i1, IntV i2 ->
           (match b with
            | Add -> return (IntV (i1 + i2), env)
            | Sub -> return (IntV (i1 - i2), env)
            | Mul -> return (IntV (i1 * i2), env)
            | Div -> return (IntV (i1 / i2), env)
           )
        | _ -> C.return_error "Binop applied to non-Int type"
       )    

   let init_log r log' = 
      match r with
      | Ok z -> Ok (z, log')
      | Error msg -> Error (msg, log')

   let eval_without_cps (t:exp) (env:environment) = 
      let eval_res = eval_without_tc t env init_log [] in
      match eval_res with
         | Ok (z, log) -> Ok z, log
         | Error (msg, log) -> Error msg, log


end
