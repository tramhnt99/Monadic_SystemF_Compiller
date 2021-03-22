(*
Monadic System F Interpreter - Helper functions
Year 4 Capstone Project
Tram Hoang
 *)

(*All functions in this module are not monadic *)

open SystemFSig.SystemF0Signature
   
let lookup_var x (env: environment) : value option =
  let env_var = env.variables in
  try Some (List.assoc x env_var)
  with _ -> None

let lookup_ty ty (env: environment) : ty option =
  let env_ty = env.types in
  try 
    match List.assoc ty env_ty with
    | TypV ty' -> Some ty'
    | _ -> failwith "Should be caught with try with"
  with _ -> None
          
(*Evaluate Abs for making a polymorphic type a monomoprhic one*)
let propTy (env: environment) (ty: ty): ty =
  match ty with
  | TVar tv -> 
     (match lookup_ty tv env with
      | Some new_ty -> new_ty
      | None -> ty)
  | TFunc (TVar tv1, TVar tv2) ->
     let equal = tv1 = tv2 in
     let check_subst_tv1 = Option.is_some (lookup_ty tv1 env) in
     let check_subst_tv2 = Option.is_some (lookup_ty tv2 env) in
     begin
       match equal, check_subst_tv1, check_subst_tv2 with
       | true, true, _ -> 
          let nt = lookup_ty tv1 env in
          TFunc (Option.get nt, Option.get nt)
       | true, false, _ -> ty
       | false, true, true ->
          TFunc (Option.get @@ lookup_ty tv1 env, Option.get @@ lookup_ty tv2 env)
       | false, true, false ->
          TFunc (Option.get @@ lookup_ty tv1 env, TVar tv2)
       | false, false, true ->
          TFunc (TVar tv1, Option.get @@ lookup_ty tv2 env)
       | _ -> ty
     end
  | _ -> ty

(* Get expression of value *)
let exp_of_value (v: value) : exp =
  match v with
  | IntV i -> Int i
  | Closure (_, Some v, ty, exp) -> Abs (v, ty, exp)
  | Closure (_, None, TVar tv, exp) -> ETAbs (tv, exp)
  | TypV t ->  Typ t
  | _ -> failwith "Cannot get exp of this value"


(* Intermediate eval to expressions (instead of values), used to make polymorphic types to a monomorphic, or propagate a variable definition*)
let rec propValTy (env: environment) (t: exp) : exp = 
  match t with
  | Abs (v, ty, exp) -> 
     Abs (v, propTy env ty, propValTy env exp)
  | App (e1, e2) -> 
     begin
       let i1 =  propValTy env e1 in
       match i1 with
       | Int _ | Typ _ -> failwith "Already not applied to a function"
       | _ -> 
          App (i1, propValTy env e2)
     end
  | Var v -> 
     begin
       match lookup_var v env with
       | Some x -> exp_of_value x
       | None -> Var v
     end
  | Binop (b, e1, e2) -> 
     Binop (b, propValTy env e1, propValTy env e2)
  | _ -> t
