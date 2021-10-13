(** Language E, specified in Ch. 4 *)

module Typ = struct
  module O = struct
    type 'a t =
      | Bot of 'a (* awkward workaround for map *)
      | Num
      | Str
    [@@deriving sexp, fold, eq, map]

    let to_string = function
      | Bot _ -> failwith "impossible"
      | Num   -> "num"
      | Str   -> "str"
  end

  include Abt.Make (O)

  let num = op O.Num

  let str = op O.Str
end

module Exp = struct
  module O = struct
    open Sexplib.Std

    type 'a t =
      | Num of int
      | Str of string
      | Plus of 'a * 'a
      | Times of 'a * 'a
      | Cat of 'a * 'a
      | Len of 'a
      | Let of 'a * 'a
    [@@deriving sexp, fold, eq, map]

    let to_string = function
      | Num n        -> string_of_int n
      | Str s        -> s
      | Plus (m, n)  -> Printf.sprintf "(%s + %s)" m n
      | Times (m, n) -> Printf.sprintf "(%s * %s)" m n
      | Cat (s, s')  -> Printf.sprintf "(%s ++ %s)" s s'
      | Len s        -> Printf.sprintf "len(%s)" s
      | Let (v, bnd) -> Printf.sprintf "let %s =: %s" v bnd
  end

  include Abt.Make (O)
  open O

  let num n = op (Num n)

  let str s = op (Str s)

  let plus a b = op (Plus (a, b))

  let times a b = op (Times (a, b))

  let cat s s' = op (Cat (s, s'))

  let len s = op (Len s)

  let let_ ~var ~value exp = Let (value, var#.exp)
end

(** Specified in sec. 4.2 *)
module Statics = struct
  module Ctx = struct
    include Abt.Var.Map
    (** Context is a map from variables to types *)

    type t = Typ.t Abt.Var.Map.t
  end

  type type_judgment = Exp.t * Typ.t

  exception Type_error of Ctx.t * type_judgment * Typ.t

  exception Invalid_rule of string * Exp.t

  exception Untyped_var of Abt.Var.t

  (** iiuc, this implementation mostly fits Ex. 4.1 *)
  let rec check : Ctx.t -> type_judgment -> unit =
   fun ctx (exp, typ) ->
    let typ' = synthesize ctx exp in
    if Typ.equal typ' typ then
      ()
    else
      raise (Type_error (ctx, (exp, typ), typ'))

  (** This covers Lema 4.1 *)
  and synthesize : Ctx.t -> Exp.t -> Typ.t =
   fun ctx ->
    let check_bin_op e1 e2 typ =
      check ctx (e1, typ);
      check ctx (e2, typ);
      typ
    in
    function
    | Var v -> (
        try Ctx.find v ctx with
        | Not_found -> raise (Untyped_var v))
    | Opr (Str _) -> Typ.str
    | Opr (Num _) -> Typ.num
    | Opr (Plus (e1, e2))
    | Opr (Times (e1, e2)) ->
        check_bin_op e1 e2 Typ.num
    | Opr (Cat (e1, e2)) -> check_bin_op e1 e2 Typ.str
    | Opr (Len e) ->
        check ctx (e, Typ.str);
        Typ.num
    | Opr (Let (e1, Bnd (bnd, e2))) ->
        let v = Abt.Var.of_binding bnd in
        let t1 = synthesize ctx e1 in
        let ctx' = Ctx.add v t1 ctx in
        let t2 = synthesize ctx' e2 in
        check ctx' (e2, t2);
        t2
    | Opr (Let _)
    | Bnd _ ->
        failwith "Invalid term"
end
