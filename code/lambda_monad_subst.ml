(*
Lambda Calculus Intepreter Using Monads To Propagate Errors and Function Calls
Substitution Based Interpreter
Year 4 Capstone Project
Tram Hoang
*)

(*Lambda Calculus Monad for propagating errors *)
module LambdaMonad = struct
  type var = string

  (* Binary Operation *)
  type binop = 
    | Add
    | Sub
    | Mul 
    | Div

  (* Expressions *)
  type exp = 
    | Int of int
    | Var of var
    | Fun of var * exp
    | App of exp * exp
    | Binop of binop * exp * exp

  (*Functions are values *)
  type value = 
    | IntV of int
    | FunV of var * exp

  type log =
    | Subst of var * value * exp
    | Eval of exp

  (* First is the expression/value, then string for an error, then string for functions*)
  type 'a t = 'a option * (string * log list)
            
  let return x name = (Some x, ("", [name]))

  let (>>=) (m: 'a t) 
        (f: 'a -> 'b t) : 'b t = 
    match m with
    | (None, s) -> (None, s)
    | (Some e, (s, fl)) ->
       let res, (new_s, new_fl) = f e in
       res, (new_s ^ s, new_fl @ fl)

  (* Values are subset of expressions *)
  let exp_of_values (v:value): exp =
    match v with
    | IntV e -> Int e
    | FunV (v, e) -> Fun (v, e)

  (* Substitution in exp of t1 with t2 *)
  let rec subst (t1: var) (t2: value) (exp: exp) : exp t = 
    let name = Subst (t1, t2, exp) in
    match exp with
    | Int _ -> return exp name
    | Var x -> if x = t1 then return (exp_of_values t2) name else return (Var x) name
    | Fun (x, e) -> if x = t1 then return exp name (* x is bound by Fun *)
                    else subst t1 t2 e >>= fun e' -> return (Fun (x, e')) name
    | Binop (b, e1, e2) -> 
       subst t1 t2 e1 >>= fun lhs ->
       subst t1 t2 e2 >>= fun rhs ->
       return (Binop (b, lhs, rhs)) name
    | App (e1, e2) -> 
       subst t1 t2 e1 >>= fun lhs ->
       subst t1 t2 e2 >>= fun rhs ->
       return (App (lhs, rhs)) name

  let rec eval (e: exp) : value t = 
    let name = Eval e in
    match e with
    | Int i -> return (IntV i) name
    | Binop (b, e1, e2) ->
       eval e1 >>= fun lhs ->
       eval e2 >>= fun rhs ->
       begin
       match lhs, rhs with
       | (IntV i1, IntV i2) ->
          begin
            match b with
            | Add -> return (IntV (i1 + i2)) name
            | Sub -> return (IntV (i1 - i2)) name
            | Mul -> return (IntV (i1 * i2)) name
            | Div -> return (IntV (i1 / i2)) name
          end
       | _ -> (None, ("e1 and/or e2 are not integers", [name]))
       end
    | Var _ -> (None, ("eval free variable", [name]))
    | Fun (arg, body) -> return (FunV (arg, body)) name
    | App (e1, e2) ->
       begin
         eval e1 >>= fun lhs ->
         eval e2 >>= fun rhs ->
         match lhs, rhs with
          | (FunV(x, body), v) -> subst x v body >>= fun e -> eval e
          | _ -> None, ("tried to apply to non-function", [name])
       end
end

  
