open QCheck
open E_gen
open E

let count = 5000
let property name = Test.make ~count ~name

let has_type e t =
  match Statics.typecheck (Exp.annot e t) with
  | () -> true
  | (exception Statics.Unbound_var _)
  | (exception Statics.Type_error _) ->
      false
  | exception Dynamics.Illformed _ ->
      failwith "Illformed expression: should be imposible"

let is_val : Exp.t -> bool = function
  | Exp.Opr (Str _)
  | Exp.Opr (Num _) ->
      true
  | _ -> false

let () =
  (* QCheck_runner.set_seed 419048757; *)
  QCheck_runner.run_tests_main
    [ property
        "Theorm 6.1: Type safety (1) -- If e : τ and e ~> e', then e' : τ"
        (pair Prog.arbitrary_exp Prog.arbitrary_typ)
        (fun (e, t) ->
          has_type e t
          ==>
          match Dynamics.eval e with
          | Val e' | Err e' -> has_type e' t
          | exception Dynamics.Illformed _ -> false)
    ; property
        "Theorm 6.5: Type safety (2) -- If e : τ, then either e err, or e val, \
         or there exists e' such that e ~> e'"
        (pair Prog.arbitrary_exp Prog.arbitrary_typ)
        (fun (e, t) ->
          has_type e t
          ==> (is_val e
              ||
              match Dynamics.eval e with
              | Val _e' -> true
              | Err _err -> true
              | exception Dynamics.Illformed _ -> false))
    ]
