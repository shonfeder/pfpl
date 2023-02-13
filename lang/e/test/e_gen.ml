module Var = struct
  let var_name_gen =
    let open QCheck.Gen in
    map (String.make 1) (char_range 'a' 'z')

  let var_name_gen_caps =
    let open QCheck.Gen in
    map (String.make 1) (char_range 'A' 'Z')

  let binding_gen =
    let open QCheck.Gen in
    map Abt.Var.Binding.v var_name_gen

  let fvar_gen =
    let open QCheck.Gen in
    map Abt.Var.v var_name_gen

  let var_gen =
    let open QCheck.Gen in
    oneof [ fvar_gen; map Abt.Var.of_binding binding_gen ]

  let arbitrary_free = QCheck.make ~print:Abt.Var.to_string fvar_gen
  let arbitrary = QCheck.make ~print:Abt.Var.to_string var_gen
end

module Prog = struct
  open E
  open QCheck.Gen

  let exp_var = map Exp.v Var.var_name_gen
  let atom = oneof
      [ return Exp.(num 0) (* ensure we always try 0 *)
      ; map Exp.num int
      ; map Exp.str Var.var_name_gen
      ]

  let typ =
    sized
    @@ fix (fun self -> function
         | 0 -> oneofl Typ.[ num; str ]
         | n ->
             let n' = n / 2 in
             map2 Typ.arr (self n') (self n')
         )

  let exp =
    sized
    @@ fix (fun self -> function
         | 0 -> atom
         | n ->
             let n' = n / 2 in
             frequency
               [ (1, map2 Exp.plus (self n') (self n'))
               ; (1, map2 Exp.times (self n') (self n'))
               ; (1, map2 Exp.div (self n') (self n'))
               ; (1, map2 Exp.cat (self n') (self n'))
               ; (1, map Exp.len (self n'))
               ; (1, return Exp.error)
               ; (1, fun st ->
                       Exp.let_
                         ~var:(Var.var_name_gen st)
                         ~typ:(typ st)
                         ~value:(self n' st)
                         (self n' st) )
               ; (1, fun st ->
                       Exp.lam
                         ~var:(Var.var_name_gen st)
                         ~typ:(typ st)
                         (self n' st) )
               ; (1, map2 Exp.app (self n') (self n'))
               ; (1, map2 Exp.annot (self n') typ)
               ])

  let arbitrary_exp = QCheck.make ~print:Exp.to_string exp
  let arbitrary_typ = QCheck.make ~print:Typ.to_string typ
end
