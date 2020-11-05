(*
Monadic System F Interpreter - Helper functions
Year 4 Capstone Project
Tram Hoang
 *)


module MonadSystemFHelpers = struct
  open Monad_systemF_sig.MonadSystemFSignature
  open SystemF_sig.SystemF0Signature
     
  let lookup x (env: environment) : value option t =
    let name = Lookup(x, env) in
    try return (Some (List.assoc x env)) name
    with _ -> return None name

  let getTypV typv : ty t = 
    let name = GetTypV (typv) in
    match typv with
    | TypV ty -> return ty name
    | _ -> None, [Error "wrong function"]
         
  (*Evaluate Abs for making a polymorphic type a monomoprhic one*)
  let evalAbsty (env: ('a * 'b) list) 
        (ty: ty) (lookup: 'a -> ('a * 'b) list -> 'b option t): ty t =
    let name = EvalAbsTy (env, ty) in
    match ty with
    | TVar tv -> 
       (lookup tv env >>= fun ty' ->
       match ty' with
       | Some TypV new_ty -> return new_ty name
       | None -> return ty name
       | _ -> (None, [Error "TVar: type lookup didn't return a type"]))
    | TFunc (TVar tv1, TVar tv2) ->
       let equal = tv1 = tv2 in
       lookup tv1 env >>= fun subst_tv1 ->
       lookup tv2 env >>= fun subst_tv2 ->
       let check_subst_tv1 = Option.is_some subst_tv1 in
       let check_subst_tv2 = Option.is_some subst_tv2 in
       begin
         match equal, check_subst_tv1, check_subst_tv2 with
         | true, true, _ -> 
            lookup tv1 env >>= fun nt ->
            Option.get nt |> getTypV >>= fun new_ty ->
            return (TFunc (new_ty, new_ty)) name
         | true, false, _ -> return ty name
         | false, true, true ->
            lookup tv1 env >>= fun nt1 ->
            lookup tv2 env >>= fun nt2 ->
            Option.get nt1 |> getTypV >>= fun new_ty1 ->
            Option.get nt2 |> getTypV >>= fun new_ty2 ->
            return (TFunc (new_ty1, new_ty2)) name
         | false, true, false ->
            lookup tv1 env >>= fun nt1 ->
            Option.get nt1 |> getTypV >>= fun new_ty1 ->
            return (TFunc (new_ty1, TVar tv2)) name
         | false, false, true ->
            lookup tv2 env >>= fun nt2 ->
            Option.get nt2 |> getTypV >>= fun new_ty2 ->
            return (TFunc (TVar tv1, new_ty2)) name
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
  let rec inter_eval (env: environment) (t: exp) : exp t = 
    let name = InterEval(env, t) in
    match t with
    | Abs (v, ty, exp) -> 
       inter_eval env exp >>= fun new_exp ->
       evalAbsty env ty lookup >>= fun new_ty ->
       return (Abs (v, new_ty, new_exp)) name
    | App (e1, e2) -> 
       begin
         inter_eval env e1 >>= fun i1 ->
         match i1 with
         | Int _ | Typ _ -> failwith "Already not applied to a function"
         | _ -> 
            inter_eval env e2 >>= fun i2 ->
            return (App (i1, i2)) name
       end
    | Var v -> 
       begin
         lookup v env >>= fun nt ->
         match nt with
         | Some x -> exp_of_value x >>= fun res -> return res name
         | None -> return (Var v) name
       end
    | Binop (b, e1, e2) -> 
       inter_eval env e1 >>= fun lhs ->
       inter_eval env e2 >>= fun rhs ->
       return (Binop (b, lhs, rhs)) name
    | _ -> return t name

end
