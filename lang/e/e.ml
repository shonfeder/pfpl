(** Ex. 4.2 *)

module Typ = struct
  module O = struct
    type 'a t =
      | Bot of 'a (* awkward workaround for map *)
      | Num
      | Str
    [@@deriving sexp, fold, eq, map]

    let to_string = function
      | Bot _ -> failwith "impossible"
      | Num   -> "Num"
      | Str   -> "Str"
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
      | Let of 'a * Typ.t * 'a
      | Annot of 'a * Typ.t
    [@@deriving sexp, fold, eq, map]

    let to_string = function
      | Num n           -> string_of_int n
      | Str s           -> Printf.sprintf "%S" s
      | Plus (m, n)     -> Printf.sprintf "(%s + %s)" m n
      | Times (m, n)    -> Printf.sprintf "(%s * %s)" m n
      | Cat (s, s')     -> Printf.sprintf "(%s ++ %s)" s s'
      | Len s           -> Printf.sprintf "len(%s)" s
      | Let (v, t, bnd) ->
          Printf.sprintf "let %s : %s =: %s" v (Typ.to_string t) bnd
      | Annot (e, t)    -> Printf.sprintf "%s : %s" e (Typ.to_string t)
  end

  include Abt.Make (O)
  open O

  let num n = op (Num n)
  let str s = op (Str s)
  let plus a b = op (Plus (a, b))
  let times a b = op (Times (a, b))
  let cat s s' = op (Cat (s, s'))
  let len s = op (Len s)
  let let_ ~var ~typ ~value exp = op @@ Let (value, typ, var#.exp)
  let annot exp typ = op (Annot (exp, typ))
end

module Statics = struct
  module Ctx = struct
    include Abt.Var.Map
    (** Context is a map from variables to types *)

    type t = Typ.t Abt.Var.Map.t

    let to_string : t -> string =
     fun t ->
      to_seq t
      |> List.of_seq
      |> List.map (fun (k, v) -> Abt.Var.to_string k ^ " : " ^ Typ.to_string v)
      |> String.concat "\n"
  end

  exception Type_error of Ctx.t * Exp.t * Typ.t * Typ.t
  exception Unbound_var of Abt.Var.t

  let rec check : Ctx.t -> Exp.t -> Typ.t -> unit =
   fun ctx exp typ ->
    let assert_ exp typ =
      let typ' = synthesize ctx exp in
      if Typ.equal typ' typ then
        ()
      else
        raise (Type_error (ctx, exp, typ, typ'))
    in
    match (exp, typ) with
    | Opr (Str _), Opr Str
    | Opr (Num _), Opr Num ->
        ()
    | exp, _ -> assert_ exp typ

  and synthesize : Ctx.t -> Exp.t -> Typ.t =
   fun ctx -> function
    | Bnd (_, _) -> failwith "Invalid term"
    | Var v      -> (
        try Ctx.find v ctx with
        | Not_found -> raise (Unbound_var v))
    | Opr op     ->
    match op with
    | Num _ -> Typ.num
    | Str _ -> Typ.str
    | Plus (e1, e2)
    | Times (e1, e2) ->
        check ctx e1 Typ.num;
        check ctx e2 Typ.num;
        Typ.num
    | Cat (e1, e2) ->
        check ctx e1 Typ.str;
        check ctx e2 Typ.str;
        Typ.str
    | Len e ->
        check ctx e Typ.str;
        Typ.num
    | Let (e1, t1, Bnd (bnd, e2)) ->
        let v = Abt.Var.of_binding bnd in
        check ctx e1 t1;
        let ctx' = Ctx.add v t1 ctx in
        let t2 = synthesize ctx' e2 in
        check ctx' e2 t2;
        t2
    | Let _ -> failwith "Invalid term"
    | Annot (e, t) ->
        check ctx e t;
        t

  (* typechecking succeeds if we can find or construct a type for the expression *)
  let typecheck : Exp.t -> unit =
    fun exp ->
    let _type = synthesize Ctx.empty exp in
    ()
end

(* Section 5.2 *)
module Dynamics = struct
  exception Illformed of Exp.t

  let is_value : Exp.t -> bool = function
    | Opr (Num _)
    | Opr (Str _) ->
        true
    | _ -> false

  let rec eval : Exp.t -> Exp.t =
   fun exp ->
    match exp with
    | Opr (Num _)
    | Opr (Str _) ->
        exp
    | Opr (Plus (n1, n2)) -> plus (eval n1, eval n2)
    | Opr (Times (n1, n2)) -> times (eval n1, eval n2)
    | Opr (Cat (s1, s2)) -> cat (eval s1, eval s2)
    | Opr (Len s) -> len (eval s)
    | Opr (Annot (e, _)) -> eval e
    | Opr (Let (value, _, bnd)) -> eval (let_ (eval value) bnd)
    | Var v -> raise (Statics.Unbound_var v)
    | Bnd (_, _) -> raise (Illformed exp)

  and plus = function
    | Opr (Num n1), Opr (Num n2) -> Exp.num (n1 + n2)
    | Opr (Num _), e
    | e, _ ->
        raise (Illformed e)

  and times = function
    | Opr (Num n1), Opr (Num n2) -> Exp.num (n1 * n2)
    | Opr (Num _), e
    | e, _ ->
        raise (Illformed e)

  and cat = function
    | Opr (Str s1), Opr (Str s2) -> Exp.str (s1 ^ s2)
    | Opr (Num _), e
    | e, _ ->
        raise (Illformed e)

  and len = function
    | Opr (Str s) -> Exp.num (String.length s)
    | e           -> raise (Illformed e)

  and let_ value = function
    | Bnd (bnd, exp) -> Exp.subst ~value bnd exp
    | e              -> raise (Illformed e)
end
