(*
Monadic System F Interpreter - Helper functions
Year 4 Capstone Project
Tram Hoang
 *)


module MonadSystemFHelpers = struct
  open Monad_systemF_sig.MonadSystemFSignature
  open SystemF_sig.SystemF0Signature
     
  let lookup_var x (env: environment) : value option t =
    let name = LookupVar(x, env) in
    let env_var = env.variables in
    try return (Some (List.assoc x env_var)) name
    with _ -> return None name

  let lookup_ty ty (env: environment) : ty option t =
    let name = LookupType(ty, env) in
    let env_ty = env.types in
    try 
      match List.assoc ty env_ty with
      | TypV ty' -> return (Some ty') name
      | _ -> None, [Error ("lookup_ty got a non-type result")]
    with _ -> return None name
  
  (* let getTypV typv : ty t = 
   *   let name = GetTypV (typv) in
   *   match typv with
   *   | TypV ty -> return ty name
   *   | _ -> None, [Error "wrong function"] *)
         
  (*Evaluate Abs for making a polymorphic type a monomoprhic one*)
  let propTy (env: environment) (ty: ty): ty t =
    let name = PropTy (env, ty) in
    match ty with
    | TVar tv -> 
       (lookup_ty tv env >>= fun ty' ->
       match ty' with
       | Some new_ty -> return new_ty name
       | None -> return ty name)
    | TFunc (TVar tv1, TVar tv2) ->
       let equal = tv1 = tv2 in
       lookup_ty tv1 env >>= fun subst_tv1 ->
       lookup_ty tv2 env >>= fun subst_tv2 ->
       let check_subst_tv1 = Option.is_some subst_tv1 in
       let check_subst_tv2 = Option.is_some subst_tv2 in
       begin
         match equal, check_subst_tv1, check_subst_tv2 with
         | true, true, _ -> 
            lookup_ty tv1 env >>= fun nt ->
            return (TFunc (Option.get nt, Option.get nt)) name
         | true, false, _ -> return ty name
         | false, true, true ->
            lookup_ty tv1 env >>= fun nt1 ->
            lookup_ty tv2 env >>= fun nt2 ->
            return (TFunc (Option.get nt1, Option.get nt2)) name
         | false, true, false ->
            lookup_ty tv1 env >>= fun nt1 ->
            return (TFunc (Option.get nt1, TVar tv2)) name
         | false, false, true ->
            lookup_ty tv2 env >>= fun nt2 ->
            return (TFunc (TVar tv1, Option.get nt2)) name
         | _ -> return ty name
       end
    | _ -> return ty name

  (* Get expression of value *)
  let exp_of_value (v: value) : exp t =
    let name = ExpOfValue v in
    match v with
    | IntV i -> return (Int i) name
    | Closure (_, Some v, ty, exp) -> return (Abs (v, ty, exp)) name
    | Closure (_, None, TVar tv, exp) -> return (ETAbs (tv, exp)) name
    | TypV t ->  return (Typ t) name
    | _ -> None, [Error "Cannot get exp of this value"]


  (* Intermediate eval to expressions (instead of values), used to make polymorphic types to a monomorphic, or propagate a variable definition*)
  let rec propValTy (env: environment) (t: exp) : exp t = 
    let name = PropValTy(env, t) in
    match t with
    | Abs (v, ty, exp) -> 
       propValTy env exp >>= fun new_exp ->
       propTy env ty >>= fun new_ty ->
       return (Abs (v, new_ty, new_exp)) name
    | App (e1, e2) -> 
       begin
         propValTy env e1 >>= fun i1 ->
         match i1 with
         | Int _ | Typ _ -> failwith "Already not applied to a function"
         | _ -> 
            propValTy env e2 >>= fun i2 ->
            return (App (i1, i2)) name
       end
    | Var v -> 
       begin
         lookup_var v env >>= fun nt ->
         match nt with
         | Some x -> exp_of_value x >>= fun res -> return res name
         | None -> return (Var v) name
       end
    | Binop (b, e1, e2) -> 
       propValTy env e1 >>= fun lhs ->
       propValTy env e2 >>= fun rhs ->
       return (Binop (b, lhs, rhs)) name
    | _ -> return t name

end
