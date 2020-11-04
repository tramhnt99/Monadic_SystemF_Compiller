(*
System F Interpreter
Based on Type Systems for Programming Language by Benjamin C. Pierce
pg. 133-134
Year 4 Capstone Project
Tram Hoang
*)

(*Church numerals pg33 *)

module SystemF0 = struct
  open SystemF_sig
  open SystemF0Signature

  let lookup x (env: environment) : value option = 
    try Some (List.assoc x env)
    with _ -> None

  let getTypV typv = 
    match typv with
    | TypV ty -> ty
    | _ -> failwith "wrong function"
        
  (*Evaluate Abs for making a polymorphic type a monomoprhic one*)
  let evalAbsty (env: ('a * 'b) list) 
        (ty: ty) (lookup: 'a -> ('a * 'b) list -> 'b option): ty =
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

  (* Get expression of value *)
  let exp_of_value (v: value) : exp =
    match v with
    | IntV i -> Int i
    | Closure (_, Some v, ty, exp) -> Abs (v, ty, exp)
    | Closure (_, None, TVar tv, exp) -> ETAbs (tv, exp)
    | TypV t -> Typ t
    | _ -> failwith "Cannot get exp of this value"


  (* Intermediate eval to expressions (instead of values), used to make polymorphic types to a monomorphic, or propagate a variable definition*)
  let rec inter_eval (env: environment) (t: exp) : exp = 
    match t with
    | Abs (v, ty, exp) -> Abs (v, evalAbsty env ty lookup, inter_eval env exp)
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

end

(*TODO: typecheck at Application.*)


(*
open SystemF_tests;;
open SystemF_TypeChecker;;
open SystemF0TypeChecker;;
open SystemF_eval;;
open SystemF0Evaluator;;

eval' (App (App (double, int_func), Int 2));;


    (* printf "\n";
     * printf "EXP is %s" (string_of_exp t);
     * printf "ENV is %s" (List.fold_left (fun res e -> res ^ " " ^ fst e) "" env);
     * printf "\n"; *)


 *)
