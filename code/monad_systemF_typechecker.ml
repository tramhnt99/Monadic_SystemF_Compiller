(*
Monadic System F Interpreter - TypeChecker
Year 4 Capstone Project
Tram Hoang
*)

module MonadSystemFTypeChecker = struct
  open Monad_systemF_sig.MonadSystemFSignature
  open SystemF_sig.SystemF0Signature
  open Monad_systemF_helpers.MonadSystemFHelpers

  let add_env (v: tvar) (x: ty) (env: environment) : environment =
    { types = (v, TypV x) :: env.types
    ; variables = env.variables}

  let empty_env : environment = {types = []; variables = []}

  let rec type_of_exp (env: environment) (e: exp) : (ty * environment) t = 
    let name = TypeOfExp (env, e) in
    let rt = fun res -> return res name in
    match e with
    | Int _ -> (TInt, env) |> rt
    | Typ ty -> (ty, env) |> rt
    | Var v -> 
       ( lookup_ty v env >>= fun nv ->
         match nv with
        | Some ty -> (ty, env) |> rt
        | None -> None, [Error "Type of Var v is not specified anywhere"] )  
    (* ^ this case is using function without instating type *)
    | ETVar tv -> (TVar tv, env) |> rt
    | ETAbs (tv, e) -> 
       type_of_exp env e >>= fun nt ->
       (TForAll (tv, nt |> fst), env) |> rt
    | ETApp (e1, e2) -> 
       (*Check that what's being applied is a type*)
       let ty_app = 
         match e2 with
         | Typ ty -> ty |> rt
         | _ -> None, [Error "ETApp applied e2 is not a type"]
       in
       (*Check that e1 is of type for all*)
       let tvar = 
         type_of_exp env e1 >>= fun nt ->
         match nt |> fst with
         | TForAll (tv, _) -> tv |> rt
         | _ -> None, [Error "ETApp type of e1 is not for all, cannot perform App"]
       in 
       ty_app >>= fun tya ->
       tvar >>= fun tv -> 
       let new_env = add_env tv tya env in
       (*Propagate that X polymorphic is no longer*)
       (match e1 with
        | ETAbs (_, e) -> type_of_exp new_env e
        | _ -> type_of_exp new_env e2
       )
    | Abs (v, ty, exp) -> 
       (*First, put v in env*)
       propTy env ty >>= fun new_ty ->
       let new_env = add_env v new_ty env in
       type_of_exp new_env exp >>= fun ne -> 
       (TFunc (new_ty, ne |> fst), new_env) |> rt
    | App (e1, e2) -> 
       (*Find type of e1*)
       type_of_exp env e1 >>= fun e1_ty -> 
       (*Check e2 is a valid type for argument in e1*)
       type_of_exp env e2 >>= fun e2_ty ->
       let res: ty t = 
         match e1_ty with
         | TFunc (ty_param, ty), _ -> 
            if ty_param = fst e2_ty then ty |> rt
            else None, [Error ("Wrong application types. Failed the T-App rule where ty_param is " ^ 
                             (Utils.string_of_ty ty_param) ^
                               " and e2_ty is " ^ (fst e2_ty |> Utils.string_of_ty))]
         | TForAll _, _ -> (*then any type suffices*)
            (*TODO: As long is ty matches the form the e2_ty*)
            fst e2_ty |> rt
         | _ ->
            None, [Error ("Wrong application types. Failed the T-App rule with Type "
              ^ Utils.string_of_ty (fst e1_ty))]
       in
       res >>= fun r -> (r, env) |> rt
    | Binop _ -> (TFunc (TInt, TFunc (TInt, TInt)), env) |> rt
    
  let type_of_value (env: environment) (v: value): ty t = 
    let name = TypeOfValue (env, v) in
    let rt = fun res -> return res name in
    match v with
    | IntV _ -> TInt |> rt
    | Closure (_, v_op, t, e) -> 
       (match v_op with
        | None -> 
           type_of_exp env e >>= fun ne ->
           TFunc (t, ne |> fst) |> rt
        | Some v -> 
           let new_env = add_env v t env in
           type_of_exp new_env e >>= fun res ->
           TFunc (t, res |> fst) |> rt
       )
    | TypV ty -> ty |> rt
end
