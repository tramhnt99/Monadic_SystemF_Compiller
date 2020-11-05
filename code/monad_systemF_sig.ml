(*
Monadic System F Interpreter - Signature
Year 4 Capstone Project
Tram Hoang
*)

module MonadSystemFSignature = struct
  open SystemF_sig.SystemF0Signature

  (*Log stands for every single function *)
  type log = 
    | Eval of environment * exp
    | Lookup of string * environment
    | InterEval of environment * exp
    | EvalAbsTy of environment * ty
    | Error of string
    | GetTypV of value
    | ExpOfValue of value 
    | Lookupty of string * environment
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
