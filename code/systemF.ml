(*
System F Interpreter
Based on Type Systems for Programming Language by Benjamin C. Pierce
pg. 133-134
Year 4 Capstone Project
Tram Hoang
*)


(*
Notes: Do the closure based interpreter, try with Int first to be able to
write + run the examples given in book
 *)

module SystemF0 = struct
  
  type var = string
  type tvar = string

  (*Types*)
  type ty = 
    | TVar of tvar (* X *)
    | TFunc of ty * ty (* T -> T *)
    | TForAll of tvar * ty (*For all X, T*)
    | Int (* TODO: implmenet church numerals Nat *)

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

  type value = 
    | IntV of int
    | Closure of environment * var option * ty * exp
    | TypV of ty
  and environment = (var * value) list

  let lookup x (env: environment) : value option = 
    try Some (List.assoc x env)
    with _ -> None
    
           
  (*Single polymorphic identity function*)
  let id_func = TAbs ("X", Abs ("x", TVar "X", Var "x"))
              
  (* Eval should be able to handle given type first to give a TFunc, then given a TFunc to evaluate like a normal lambda func

=>  TApp (id_func, TypV Int)
 -> Closure ([], "X", Abs ("x", TVar "X", Var "x")), Typv Int
 -> eval [("X", TypV Int)] Abs ("x", TVar "X", Var "x")

   *)
  let rec eval (env: environment) (t: exp) : value =
    match t with
    | Int i -> IntV i
    | Typ ty -> TypV ty
    | Var v -> Option.get (lookup v env)
    | TAbs (tv, exp)-> Closure (env, None, TVar tv, exp)
    | TApp (e1, e2) ->
       (match eval env e1, eval env e2 with
        | (Closure (cenv, _, TVar x, body), TypV ty') ->
           eval ((x, TypV ty') :: cenv) body
        | _ -> failwith "App to a non closure/tried to apply a non type"
       )
    | Abs (v, ty, exp) ->
       (match ty with
        | TVar tv -> 
           (match lookup tv env with
            | Some TypV new_ty -> Closure (env, Some v, new_ty, exp)
            | None -> Closure (env, Some v, ty, exp)
            | _ -> failwith "type lookup didn't return a type"
           )
        | _ -> Closure (env, Some v, ty, exp)
       )
    | App (e1, e2) ->
       (match eval env e1, eval env e2 with
        | Closure (cenv, Some x, _, body), v ->
           eval ((x, v) :: cenv) body
        | _ -> failwith "App to a non function"
       )
    | _ -> failwith "unimplemented"

end
