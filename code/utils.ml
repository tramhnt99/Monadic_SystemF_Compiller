open SystemF_sig
open SystemF0Signature
open Format

(* ********************************* String of functions ************************ *)
let rec string_of_ty ty =
  match ty with
  | TVar tvar -> "TVar " ^ tvar
  | TFunc (t1, t2) -> "TFunc " ^ string_of_ty t1 ^ " " ^ string_of_ty t2
  | TForAll (tv, t) -> "TForAll " ^ tv ^ " " ^ string_of_ty t
  | TInt -> "TInt "

let rec string_of_exp e =
  let contents = 
    match e with
    | Int i -> "Int " ^ (string_of_int i) ^ ", "
    | Var v -> "Var " ^ v ^ ", "
    | ETVar tv -> "TVar " ^ tv ^ ", "
    | Abs (v, ty, e') -> "Abs " ^ v ^ " " ^ string_of_ty ty ^ " " ^ string_of_exp e' ^ ", "
    | App (e1, e2) -> "App " ^ string_of_exp e1 ^ " " ^ string_of_exp e2 ^ ", "
    | ETAbs (tv, e') -> "TAbs " ^ tv ^ " " ^ string_of_exp e' ^ ", "
    | ETApp (e1, e2) -> "TApp " ^ string_of_exp e1 ^ " " ^ string_of_exp e2 ^ ", "
    | Binop (_, e1, e2) -> "Binop " ^ "some binop " ^ string_of_exp e1 ^ " " ^ string_of_exp e2 ^ ", "
    | Typ ty -> "Typ " ^ string_of_ty ty ^ ", "
  in
  "( " ^ contents ^ " ) "
  
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


(* ********************************* Printing functions ************************ *)
let print_exp (e: exp) : unit =
  printf "\n";
  printf "EXP is %s" (string_of_exp e);
  printf "\n"

let print_env env : unit =
  printf "\n";
  printf "ENV is %s" (List.fold_left 
                        (fun res e -> res ^ " " ^ fst e ^ 
                                        " : " ^ string_of_value (snd e)) "" env);
  printf "\n"
  
