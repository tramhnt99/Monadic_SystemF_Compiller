(*
Lambda Calculus Intepreter Using Monads To Propagate Errors and Function Calls
Closure Based Interpreter
Year 4 Capstone Project
Tram Hoang
*)

module LambdaMonadClosure = struct
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
    | Closure of environment * var * exp (*Instead of FunV, lazy evaluation*)
  and environment = (var * value) list

  type log =
    | Eval of environment * exp
    | Lookup of var * environment
    | Error of string

  type 'a t = 'a option * log list

  let return x name = (Some x, [name])

  let (>>=) (m: 'a t) 
        (f: 'a -> 'b t) : 'b t = 
    match m with
    | (None, l) -> (None, l)
    | (Some e, l) ->
       let res, new_l = f e in
       res, new_l @ l

  let lookup x (env:environment) : value t = 
    let name = Lookup (x, env) in
    try return (List.assoc x env) name
    with _ -> None, [Error "lookup unsuccessful"]
    
                                 
  let rec eval (env : environment) (e : exp) : value t =
    let name = Eval (env, e) in
    match e with 
    | Int i -> return (IntV i) name
    | Binop (b, e1, e2) ->
       eval env e1 >>= fun lhs ->
       eval env e2 >>= fun rhs ->
       (match lhs, rhs with 
         | (IntV i1, IntV i2) ->
            (match b with
             | Add -> return (IntV (i1 + i2)) name
             | Sub -> return (IntV (i1 - i2)) name
             | Mul -> return (IntV (i1 * i2)) name
             | Div -> 
                if i2 = 0 then None, [Error "Division by zero"]
                else return (IntV (i1 / i2)) name
            )
         | _ -> None, [Error "tried to binop non-integers"]
       )
    | Var x -> lookup x env
    | Fun (arg, body) -> return (Closure(env, arg, body)) name
    | App (e1, e2) ->
       eval env e1 >>= fun lhs ->
       eval env e2 >>= fun rhs ->
       (match lhs, rhs with
        | (Closure(cenv, x, body), v) -> eval ((x,v) :: cenv) body
        | _ -> None, [Error "tried to apply non-function"]
       )
end
