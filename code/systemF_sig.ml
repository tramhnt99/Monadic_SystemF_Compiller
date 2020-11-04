(*
System F Interpreter - Signature
Based on Type Systems for Programming Language by Benjamin C. Pierce
pg. 133-134
Year 4 Capstone Project
Tram Hoang
*)

module SystemF0Signature = struct
  type var = string
  type tvar = string

  (*Types*)
  type ty = 
    | TVar of tvar (* X *)
    | TFunc of ty * ty (* T -> T -> T would be represented as TFunc or TFunc *)
    | TForAll of tvar * ty (*For all X, T*)
    | TInt
    
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
    | ETVar of tvar
    | Abs of var * ty * exp
    | App of exp * exp
    | ETAbs of tvar * exp
    | ETApp of exp * exp
    | Typ of ty
    | Binop of binop * exp * exp

  type value = 
    | IntV of int
    | Closure of environment * var option * ty * exp
    | TypV of ty
  and environment = (var * value) list

end

