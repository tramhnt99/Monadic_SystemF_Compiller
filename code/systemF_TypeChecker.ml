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
  open SystemF_sig
  open SystemF0Signature

  type ty_environment = (var * value) list
  (*TODO: Current problem, currently type X instatiated by Int, for eg, is in the same list of var x of type Int (that used to be type X).*)
  (*Note: To avoid duplicated code, the environment is var * value instead of 
    var * ty to be able to reuse evalAbsty *)

  let lookup_ty x (env: ty_environment) : value option = 
    try Some (List.assoc x env)
    with _ -> None

  let add_env (v: var) (x: ty) (env: ty_environment) : ty_environment = 
    (v,TypV x) :: env
    
  let is_polym ty : bool = 
    match ty with
    | TVar _ -> true
    | TInt -> false
    | _ -> failwith "Should not substitute for this type"

  let rec type_of_exp (env: ty_environment) (e: exp) : ty * ty_environment = 
    match e with
    | Int _ -> TInt, env
    | Typ ty -> ty, env
    | Var v -> 
       (match lookup_ty v env with
        | Some ty -> getTypV ty, env
        | None -> TVar "ZXC", env) 
    (* ^ this case is using function without instating type *)
    | ETVar tv -> TVar tv, env
    | ETAbs (tv, e) -> TForAll (tv, type_of_exp env e |> fst), env
    | ETApp (e1, e2) -> 
       (*Check that e1 is of type for all*)
       let tvar = 
         match type_of_exp env e1 |> fst with
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

       (*Propagate that X polymorphic is no longer*)
       (match e1 with
        | ETAbs (_, e) -> type_of_exp new_env e
        | _ -> failwith "something went wrong in tvar evaluation"
       )
    | Abs (v, ty, exp) -> 
       (*First, put v in env*)
       let new_ty = evalAbsty env ty lookup_ty in
       let new_env = add_env v new_ty env in
       TFunc (new_ty, type_of_exp new_env exp |> fst), new_env
    | App (e1, e2) -> 
       (*Find type of e1*)
       let e1_ty = type_of_exp env e1 in
       (*Check e2 is a valid type for argument in e1*)
       let e2_ty = type_of_exp env e2 in
       let res: ty = 
         match e1_ty with
         | TFunc (ty_param, ty), _ -> 
            if ty_param = fst e2_ty then ty
            else failwith ("1 Wrong application types. Failed the T-App rule where ty_param is " ^ (Utils.string_of_ty ty_param) ^ " and e2_ty is " ^ (fst e2_ty |> Utils.string_of_ty) ^ "\n"
                         ^ "e2 is " ^ (Utils.string_of_exp e2)
                         ^ "and e1 is " ^ (Utils.string_of_exp e1)



)
         | TForAll _, _ -> (*then any type suffices*)
            (*TODO: As long is ty matches the form the e2_ty*)
            fst e2_ty
         | _ ->
            failwith ("3 Wrong application types. Failed the T-App rule with Type"
              ^ Utils.string_of_ty (fst e1_ty))
       in
       res, env
    | Binop _ -> TFunc (TInt, TFunc (TInt, TInt)), env
    
  let type_of_value (env: ty_environment) (v: value): ty = 
    match v with
    | IntV _ -> TInt
    | Closure (_, v_op, t, e) -> 
       (match v_op with
        | None -> TFunc (t, type_of_exp env e |> fst)
        | Some v -> 
           let new_env = add_env v t env in
           (*TODO: check this one, written when v tired*)
           TFunc (t, type_of_exp new_env e |> fst)
       )
    | TypV ty -> ty

end
