open SystemFSig.SystemF0Signature

(* ********************************* String of functions ************************ *)
let rec string_of_ty ty =
  match ty with
  | TVar tvar -> "TVar " ^ tvar
  | TFunc (t1, t2) -> "TFunc " ^ string_of_ty t1 ^ " " ^ string_of_ty t2
  | TForAll (tv, t) -> "TForAll " ^ tv ^ " " ^ string_of_ty t
  | TInt -> "TInt "

let rec string_of_exp e =
  match e with
  | Int i -> "Int " ^ (string_of_int i)
  | Var v -> "Var " ^ v
  | ETVar tv -> "TVar " ^ tv
  | Abs (v, ty, e') -> "Abs " ^ v ^ " " ^ string_of_ty ty ^ " " ^ string_of_exp e'
  | App (e1, e2) -> "App " ^ string_of_exp e1 ^ " " ^ string_of_exp e2
  | ETAbs (tv, e') -> "TAbs " ^ tv ^ " " ^ string_of_exp e'
  | ETApp (e1, e2) -> "TApp " ^ string_of_exp e1 ^ " " ^ string_of_exp e2 
  | Binop (_, e1, e2) -> "Binop " ^ "some binop " ^ string_of_exp e1 ^ " " ^ string_of_exp e2 
  | Typ ty -> "Typ " ^ string_of_ty ty 
            
let string_of_value v = 
  match v with
  | IntV i -> "IntV " ^ (string_of_int i)
  | Closure (_, v_op, ty, exp) ->
     (match v_op with
      | None -> "Closure ( " ^ "some env, " ^  
                  "None, " ^ (string_of_ty ty) ^ ", " ^ string_of_exp exp
      | Some var -> "Closure ( " ^ var ^", " ^  
                      "None, " ^ (string_of_ty ty) ^ ", " ^ string_of_exp exp
     )
  | TypV ty -> "TypV " ^  string_of_ty ty


let string_of_env (env: environment): string =
  let {types; variables} = env in
  let ty_s =
    List.fold_left (fun res (s, v) -> 
        res ^ " " ^
          String.concat " " [s; ","; string_of_value v; ";"]
      ) "" types
  in
  let v_s = 
    List.fold_left (fun res (s,v) -> 
        res ^ " " ^ 
          String.concat " " [s; ","; string_of_value v; ";"]
      ) "" variables
  in
  "{types: [" ^ ty_s ^ "]; variables: [" ^ v_s ^ "]}"

let string_of_log (l: log) : string = 
  match l with
  | Eval e -> "Eval (" ^ (string_of_exp e) ^ " )"
  | Error s -> "Error \"" ^ s ^ "\"" 
  | TypeOfExp e -> "TypeOfExp (" ^ (string_of_exp e) ^ " )"
  | TypeOfValue v -> "TypeOfValue (" ^ (string_of_value v) ^ " )"
