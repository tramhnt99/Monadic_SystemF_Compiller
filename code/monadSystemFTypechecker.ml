(*
Monadic System F Interpreter - TypeChecker
Year 4 Capstone Project
Tram Hoang
*)
open MonadSystemFSig
open SystemFSig.SystemF0Signature
open MonadSystemFHelpers

module type TypeChecker = 
  sig
    type 'a monad
    val type_of_exp: environment -> exp -> (ty * environment) monad
    val type_of_value: environment -> value -> ty monad
    val empty_env: environment
    val get_result: 'a monad -> 'a option
  end

module MonadicTypeChecker (Monad: MonadAbstractSig): TypeChecker = struct
  
  type 'a monad = 'a Monad.monad
  let (>>=) = Monad.(>>=)
  let return = Monad.return
  let get_result = Monad.get_result

  let add_env (v: tvar) (x: ty) (env: environment) : environment =
    { types = (v, TypV x) :: env.types
    ; variables = env.variables}

  let empty_env : environment = {types = []; variables = []}

  let rec type_of_exp (env: environment) (e: exp) : (ty * environment) monad = 
    let name = TypeOfExp e in
    let rt = fun res -> return res name (Some env) in
    match e with
    | Int _ -> (TInt, env) |> rt
    | Typ ty -> (ty, env) |> rt
    | Var v -> 
       ( let nv = lookup_ty v env in
         match nv with
        | Some ty -> (ty, env) |> rt
        | None -> Monad.return_error "Type of Var v is not specified anywhere" env)
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
         | _ -> Monad.return_error "ETApp applied e2 is not a type" env
       in
       (*Check that e1 is of type for all*)
       let tvar = 
         type_of_exp env e1 >>= fun nt ->
         match nt |> fst with
         | TForAll (tv, _) -> tv |> rt
         | _ -> Monad.return_error "ETApp type of e1 is not for all, cannot perform App" env
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
       let new_ty = propTy env ty in
       let new_env = add_env v new_ty env in
       type_of_exp new_env exp >>= fun ne -> 
       (TFunc (new_ty, ne |> fst), new_env) |> rt
    | App (e1, e2) -> 
       (*Find type of e1*)
       type_of_exp env e1 >>= fun e1_ty -> 
       (*Check e2 is a valid type for argument in e1*)
       type_of_exp env e2 >>= fun e2_ty ->
       let res: ty monad = 
         match e1_ty with
         | TFunc (ty_param, ty), _ -> 
            if ty_param = fst e2_ty then ty |> rt
            else Monad.return_error ("Wrong application types. Failed the T-App rule where ty_param is " ^ 
                             (Utils.string_of_ty ty_param) ^
                               " and e2_ty is " ^ (fst e2_ty |> Utils.string_of_ty)) env
         | TForAll _, _ -> (*then any type suffices*)
            (*TODO: As long is ty matches the form the e2_ty*)
            fst e2_ty |> rt
         | _ ->
            Monad.return_error ("Wrong application types. Failed the T-App rule with Type "
              ^ Utils.string_of_ty (fst e1_ty)) env
       in
       res >>= fun r -> (r, env) |> rt
    | Binop _ -> (TFunc (TInt, TFunc (TInt, TInt)), env) |> rt
    
  let type_of_value (env: environment) (v: value): ty monad = 
    let name = TypeOfValue v in
    let rt = fun res -> return res name (Some env) in
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
