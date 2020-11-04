(*
System F Interpreter - Typechecker
Based on Type Systems for Programming Language by Benjamin C. Pierce
pg. 133-134
Year 4 Capstone Project
Tram Hoang
*)

module SystemF0TypeChecker = struct
  open SystemF
  open SystemF0

  type ty_environment = (var * ty) list

  let lookup_ty x (env: ty_environment) : ty option = 
    try Some (List.assoc x env)
    with _ -> None

  let add_env (v: var) (x: ty) (env: ty_environment) : ty_environment = 
    (v,x) :: env

  let type_of_exp (env': ty_environment) (e': exp) : ty = 
    let rec type_of_exp_help (env: ty_environment) (e: exp) : ty * ty_environment = 
      match e with
      | Int _ -> TInt, env
      | Typ ty -> ty, env
      | Var v -> 
         (match lookup_ty v env with
          | Some ty -> ty, env
          | None -> failwith ("Looking for type of undeclared variable " ^ v))
      | ETVar tv -> TVar tv, env
      | ETAbs (tv, e) -> TForAll (tv, type_of_exp_help env e |> fst), env
      | ETApp (e1, e2) -> 
         (*Check that e1 is of type for all*)
         let tvar = 
           match type_of_exp_help env e1 |> fst with
           | TForAll (tv, _) -> tv
           | _ -> failwith "ETApp type of e1 is not for all, cannot perform App"
         in 
         (*Check that what's being applied is a type*)
         let ty_app = 
           match e2 with
           | Typ ty -> ty
           | _ -> failwith "ETApp applied e2 is not a type"
         in
         let new_env = add_env tvar ty_app env in
         (match e1 with
          | ETAbs (_, e) -> type_of_exp_help new_env e
          | _ -> failwith "something went wrong in tvar evaluation"
         )
      | Abs (v, ty, exp) -> 
         (*First, put v in env*)
         let new_env = add_env v ty env in
         TFunc (ty, type_of_exp_help new_env exp |> fst), new_env
      | App (e1, e2) -> 
         (*Find type of e1*)
         let e1_ty = type_of_exp_help env e1 in
         (*Check e2 is a valid type for argument in e1*)
         let e2_ty = type_of_exp_help env e2 in
         let res: ty = 
           match e1_ty with
           | TFunc (ty_param, ty), _ -> 
              if ty_param = fst e2_ty then ty
              else failwith ("1 Wrong application types. Failed the T-App rule")
           | _ -> failwith ("2 Wrong application types. Failed the T-App rule")
         in
         res, env
      | Binop _ -> TFunc (TInt, TFunc(TInt, TInt)), env
    in 
  type_of_exp_help env' e' |> fst
  
  let type_of_value (v: value) (env: ty_environment) : ty = 
    match v with
    | IntV _ -> TInt
    | Closure (_, v_op, t, e) -> 
       (match v_op with
        | None -> TFunc (t, type_of_exp env e)
        | Some v -> 
           let new_env = add_env v t env in
           (*TODO: check this one, written when v tired*)
           TFunc (t, type_of_exp new_env e)
       )
    | TypV ty -> ty

end
