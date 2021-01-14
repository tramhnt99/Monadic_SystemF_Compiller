(*
Monadic System F Interpreter - Combining to Evaluators
Year 4 Capstone Project
Tram Hoang
*)

open MonadSystemFEval
open MonadSystemFSig

(*Evaluator that logs function calls*)
module LogEval = MonadicEvaluator(LogMonad)

(*Evaluator that logs semantics - Eval function calls and it's respective states*)
module SemanticsEval = MonadicEvaluator(SemanticsMonad)
