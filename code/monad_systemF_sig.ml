(*
Monadic System F Interpreter - Signature
Year 4 Capstone Project
Tram Hoang
*)

(*
Files:
Monad_systemF_eval - evaluate function
Monad_systemF_typechecker - typechecker
Monad_systemF_helpers - helper functions
Monad_systemF_tests - Tests
 *)

module MonadSystemFSignature = struct
  open SystemF_sig.SystemF0Signature

  (*Log stands for every single function *)
  type log = 
    | Eval of environment * exp
    | LookupVar of string * environment
    | LookupType of string * environment
    | PropValTy of environment * exp
    | PropTy of environment * ty
    | Error of string
    | GetTypV of value
    | ExpOfValue of value 
    | TypeOfExp of environment * exp
    | TypeOfValue of environment * value
    
  type 'a t = 'a option * log list

  let return x name = (Some x, [name])
                    
  let (>>=) (m: 'a t)
    (f: 'a -> 'b t) : 'b t =
    match m with
    | (None, l) -> (None, l)
    | (Some e, l) -> 
       let res, new_l = f e in
       res, new_l @ l
end


