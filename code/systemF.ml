(*
System F Interpreter
Based on Type Systems for Programming Language by Benjamin C. Pierce
pg. 133-134
Year 4 Capstone Project
Tram Hoang
*)

(*Church numerals pg33 *)

module SystemF0 = struct
  (* open Format *)

  type var = string
  type tvar = string

  (*Types*)
  type ty = 
    | TVar of tvar (* X *)
    | TFunc of ty * ty (* T -> T *)
    | TForAll of tvar * ty (*For all X, T*)
    | Int
    
  (* Binary Operation *)
  type binop = 
    | Add
    | Sub
    | Mul 
    | Div

  (*Terms*)
  type exp = 
    | Int of int
    | Var of var
    | TVar of tvar
    | Abs of var * ty * exp
    | App of exp * exp
    | TAbs of tvar * exp
    | TApp of exp * exp
    | Typ of ty
    | Binop of binop * exp * exp

  type value = 
    | IntV of int
    | Closure of environment * var option * ty * exp
    | TypV of ty
  and environment = (var * value) list

  let lookup x (env: environment) : value option = 
    try Some (List.assoc x env)
    with _ -> None

  let getTypV typv = 
    match typv with
    | TypV ty -> ty
    | _ -> failwith "wrong function"

  (*Evaluate Abs for making a polymorphic type a monomoprhic one*)
  let evalAbsty (env: environment) (ty: ty) : ty =
    match ty with
    | TVar tv -> 
       begin
         match lookup tv env with
         | Some TypV new_ty -> new_ty
         | None -> ty
         | _ -> failwith "TVar: type lookup didn't return a type"
       end 
    | TFunc (TVar tv1, TVar tv2) ->
       let equal = tv1 = tv2 in
       let subst_tv1 = Option.is_some (lookup tv1 env) in
       let subst_tv2 = Option.is_some (lookup tv2 env) in
       begin
         match equal, subst_tv1, subst_tv2 with
         | true, true, _ -> 
            let new_ty = Option.get (lookup tv1 env) |> getTypV in
            TFunc (new_ty, new_ty)
         | true, false, _ -> ty
         | false, true, true ->
            let new_ty1 = Option.get (lookup tv1 env) |> getTypV in
            let new_ty2 = Option.get (lookup tv2 env) |> getTypV in
            TFunc (new_ty1, new_ty2)
         | false, true, false ->
            let new_ty1 = Option.get (lookup tv1 env) |> getTypV in
            TFunc (new_ty1, TVar tv2)
         | false, false, true ->
            let new_ty2 = Option.get (lookup tv2 env) |> getTypV in
            TFunc (TVar tv1, new_ty2)
         | _ -> ty
       end
    | _ -> ty

  (* let evalAbsparam (env: environment) (v: var) : var = 
   *   match lookup env var with
   *   |  *)
  let exp_of_value (v: value) : exp =
    match v with
    | IntV i -> Int i
    | Closure (_, Some v, ty, exp) -> Abs (v, ty, exp)
    | Closure (_, None, TVar tv, exp) -> TAbs (tv, exp)
    | TypV t -> Typ t
    | _ -> failwith "Cannot get exp of this value"

  let rec string_of_exp e =
    match e with
    | Int i -> "Int " ^ (string_of_int i)
    | Var v -> "Var " ^ v
    | TVar tv -> "TVar " ^ tv
    | Abs (v, _, e') -> "Abs " ^ v ^ " " ^ string_of_exp e'
    | App (e1, e2) -> "App " ^ string_of_exp e1 ^ " " ^ string_of_exp e2
    | TAbs (tv, e') -> "TAbs " ^ tv ^ " " ^ string_of_exp e'
    | TApp (e1, e2) -> "TApp " ^ string_of_exp e1 ^ " " ^ string_of_exp e2
    | Binop (_, e1, e2) -> "Binop " ^ "some binop " ^ string_of_exp e1 ^ " " ^ string_of_exp e2
    | _ -> "Something else"

  (* Intermediate eval to expressions (instead of values), used to make polymorphic types to a monomorphic, or propagate a variable definition*)
  let rec inter_eval (env: environment) (t: exp) : exp = 
    match t with
    | Abs (v, ty, exp) -> Abs (v, evalAbsty env ty, inter_eval env exp)
    | App (e1, e2) -> 
       begin
       let i1 = inter_eval env e1 in
       match i1 with
       | Int _ | Typ _ -> failwith "Already not applied to a function"
       | _ -> App (i1, inter_eval env e2)
       end
    | Var v -> 
       begin
         match lookup v env with
         | Some x -> exp_of_value x
         | None -> Var v
       end
    | Binop (b, e1, e2) -> Binop (b, inter_eval env e1, inter_eval env e2)
    | _ -> t

  (*Evaluate statements to a final value*)
  let rec eval (env: environment) (t: exp) : value =
    (* printf "\n";
     * printf "EXP is %s" (string_of_exp t);
     * printf "ENV is %s" (List.fold_left (fun res e -> res ^ " " ^ fst e) "" env);
     * printf "\n"; *)
    match t with
    | Int i -> IntV i
    | Typ ty -> TypV ty
    | Var v -> Option.get (lookup v env)
    | TVar tv -> Option.get (lookup tv env)
    | TAbs (tv, exp)-> Closure (env, None, TVar tv, exp)
    | TApp (e1, e2) ->
       (match eval env e1, eval env e2 with
        | (Closure (cenv, _, TVar x, body), TypV ty') -> 
           eval ((x, TypV ty') :: cenv) body
        | _ -> failwith "App to a non closure/tried to apply a non type"
       )
    | Abs (v, ty, exp) -> Closure (env, Some v, evalAbsty env ty, inter_eval env exp)
    | App (e1, e2) ->
       (match eval env e1, eval env e2 with
        | Closure (cenv, Some x, _, body), v ->
           eval ((x, v) :: cenv) body
        | Closure (cenv, None, _, body), _ -> 
           eval cenv (App (body, e2))
        (*application to a polymorphic type, just continue*)
        | _ -> failwith "App to a non function"
       )
    | Binop (b, e1, e2) ->
       (match eval env e1, eval env e2 with
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

(*TODO: if given an element while type is polymorphic, can just evaluate ?? or should infer *)


(*
testing



open SystemF;;
open SystemF0;;
open SystemF_tests;;


 *)
