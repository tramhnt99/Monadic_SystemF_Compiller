(*
System F Interpreter - Evaluator
Based on Type Systems for Programming Language by Benjamin C. Pierce
pg. 133-134
Year 4 Capstone Project
Tram Hoang
*)

module SystemF0Evaluator = struct
  open SystemF
  open SystemF0
  open SystemF_TypeChecker
  open SystemF0TypeChecker
  open SystemF_sig
  open SystemF0Signature

  (*Evaluate statements to a final value*)
  let rec eval (env: environment) (t: exp) (ty_env: ty_environment) : value =
    (*Recursively use the type environment to typecheck programs*)
    let (_, new_ty_env) = type_of_exp ty_env t in
    let eval' = (fun env' e' -> eval env' e' new_ty_env) in
    match t with
    | Int i -> IntV i
    | Typ ty -> TypV ty
    | Var v -> Option.get (lookup v env)
    | ETVar tv -> Option.get (lookup tv env)
    | ETAbs (tv, exp)-> Closure (env, None, TVar tv, exp)
    | ETApp (e1, e2) ->
       (match eval' env e1, eval' env e2 with
        | (Closure (cenv, _, TVar x, body), TypV ty') -> 
           eval' ((x, TypV ty') :: cenv) body
        | _ -> failwith "App to a non closure/tried to apply a non type"
       )
    | Abs (v, ty, exp) -> 
       Closure (env, Some v, evalAbsty env ty lookup, inter_eval env exp)
    | App (e1, e2) ->
       (match eval' env e1, eval' env e2 with
        | Closure (cenv, Some x, _, body), v ->
           eval' ((x, v) :: cenv) body
        | Closure (cenv, None, _, body), _ -> 
           eval' cenv (App (body, e2))
        (*application to a polymorphic type, just continue*)
        | _ -> failwith "App to a non function"
       )
    | Binop (b, e1, e2) ->
       (match eval' env e1, eval' env e2 with
        | IntV i1, IntV i2 ->
           (match b with
            | Add -> IntV (i1 + i2)
            | Sub -> IntV (i1 - i2)
            | Mul -> IntV (i1 * i2)
            | Div -> IntV (i1 / i2)
           )
        | _ -> failwith "Binop applied to non-Int type"
       ) 

end
