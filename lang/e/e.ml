(** Ex. 4.2 *)

module Typ = struct
  module O = struct
    type 'a t =
      | Bot of 'a (* awkward workaround for map *)
      | Num
      | Str
      | Arr of 'a * 'a
    [@@deriving sexp, fold, eq, map]

    let to_string = function
      | Bot _      -> failwith "impossible"
      | Num        -> "Num"
      | Str        -> "Str"
      | Arr (s, t) -> Printf.sprintf "(%s -> %s)" s t
  end

  include Abt.Make (O)

  let num = op O.Num
  let str = op O.Str
  let arr t1 t2 = op (O.Arr (t1, t2))
end

module Exp = struct
  module O = struct
    open Sexplib.Std

    type 'a t =
      | Num   of int
      | Str   of string
      | Plus  of 'a * 'a
      | Times of 'a * 'a
      | Div   of 'a * 'a (* Sec. 6.3: runtime errors *)
      | Cat   of 'a * 'a
      | Len   of 'a
      | Let   of 'a * Typ.t * 'a
      | Lam   of Typ.t * 'a
      | App   of 'a * 'a
      | Annot of 'a * Typ.t
      | Error
    [@@deriving sexp, fold, eq, map]

    (* https://stackoverflow.com/questions/33777404/how-to-create-the-lambda-char-in-ocaml *)
    let lambda = "\xCE\xBB"

    let to_string = function
      | Num n               -> string_of_int n
      | Str s               -> Printf.sprintf "%S" s
      | Plus (m, n)         -> Printf.sprintf "(%s + %s)" m n
      | Times (m, n)        -> Printf.sprintf "(%s * %s)" m n
      | Div (n, d)          -> Printf.sprintf "(%s / %s)" n d
      | Cat (s, s')         -> Printf.sprintf "(%s ++ %s)" s s'
      | Len s               -> Printf.sprintf "len(%s)" s
      | Let (value, t, bnd) -> Printf.sprintf "let %s : %s =: %s" value (Typ.to_string t) bnd
      | Lam (t, bnd)        -> Printf.sprintf "(%s{%s}%s)" lambda (Typ.to_string t) bnd
      | App (m, n)          -> Printf.sprintf "(%s %s)" m n
      | Annot (e, t)        -> Printf.sprintf "%s : %s" e (Typ.to_string t)
      | Error               -> Printf.sprintf "error"
  end

  include Abt.Make (O)
  open O

  let num n = op (Num n)
  let str s = op (Str s)
  let plus a b = op (Plus (a, b))
  let times a b = op (Times (a, b))
  let div n d = op (Div (n, d))
  let cat s s' = op (Cat (s, s'))
  let len s = op (Len s)
  let let_ ~var ~typ ~value exp = op @@ Let (value, typ, var#.exp)
  let lam ~var ~typ exp = op @@ Lam (typ, var#.exp)
  let app m n = op @@ App (m, n)
  let annot exp typ = op (Annot (exp, typ))
  let error = op Error
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
    | Opr (Num _), Opr Num -> ()
    | Opr Error, _         -> ()
    | exp, _               -> assert_ exp typ

  and synthesize : Ctx.t -> Exp.t -> Typ.t =
    fun ctx -> function
      | Bnd (_, _) -> failwith "Invalid term: bindings cannot be top-level"
      | Var v      -> (
          try Ctx.find v ctx with
          | Not_found -> raise (Unbound_var v))
      | Opr op as exp ->
        match op with
        | Num _ -> Typ.num
        | Str _ -> Typ.str
        | Plus (e1, e2)
        | Times (e1, e2) ->
          check ctx e1 Typ.num;
          check ctx e2 Typ.num;
          Typ.num
        | Div (e1, e2) ->
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
          check ctx e1 t1;
          let v = Abt.Var.of_binding bnd in
          let ctx' = Ctx.add v t1 ctx in
          let t2 = synthesize ctx' e2 in
          (* TODO can this check be removed?  *)
          check ctx' e2 t2;
          t2
        | Lam (t1, Bnd (bnd, e)) ->
          (* Ch. 8.2 *)
          let v = Abt.Var.of_binding bnd in
          let ctx' = Ctx.add v t1 ctx in
          let t2 = synthesize ctx' e in
          Typ.arr t1 t2
        | Let _ | Lam _ -> failwith "Invalid term: must contain a binding"
        | Annot (e, t) -> check ctx e t; t
        | Error -> Typ.num (* Type here shouldn't matter *)
        | App (e1, e2) ->
          match synthesize ctx e1 with
          | Opr (Arr (t2, t)) -> check ctx e2 t2; t
          | invalid_typ ->
            let t2 = synthesize ctx e2 in
            let expected_typ = Typ.(arr t2 (v "b")) in
            raise (Type_error (ctx, exp, expected_typ, invalid_typ))

  (* typechecking succeeds if we can find or construct a type for the expression *)
  let typecheck : Exp.t -> unit =
    fun exp ->
    let _type = synthesize Ctx.empty exp in
    ()
end

(* Section 5.2 *)
module Dynamics = struct
  let is_value : Exp.t -> bool = function
    | Opr (Num _)
    | Opr (Str _) ->
        true
    | _ -> false


  type state =
    | Val of Exp.t
    | Err of Exp.t

  let state_to_string = function
    | Val exp -> Exp.to_string exp
    | Err exp -> "error: " ^ Exp.to_string exp

  exception Illformed of Exp.t

  let div n d = match d with
    | 0 -> Err Exp.(div (num n) (num 0))
    | n -> Val Exp.(num (n / d))

  let rec eval : Exp.t -> state =
    fun exp ->
    match exp with
    | Opr Error -> Err exp
    | Opr (Num _ | Str _ | Lam _ ) -> Val exp
    (* "For example, the dynamics need not check, when performing an addition,
       that its two argument are, in fact numbers ... because the type system
       ensures that this is the case." (p. 51)  *)
    | Opr (Plus (n1, n2))       -> begin
        match eval n1, eval n2 with
        | Val n1, Val n2 -> Val (Exp.num (num n1 + num n2))
        | Err e, _ | _, Err e -> Err e
      end
    | Opr (Times (n1, n2))      -> begin
        match eval n1, eval n2 with
        | Val n1, Val n2 -> Val (Exp.num (num n1 * num n2))
        | Err e, _ | _, Err e -> Err e
      end
    (* "On the other hand, the dynamics for quotient *must* check for a zero
       divisor, because the type system does not rule out the possibility."
       (p. 51) *)
    | Opr (Div (n, d))      -> begin
        match eval n, eval d with
        | Val n, Val d -> div (num n) (num d)
        | Err e, _ | _, Err e -> Err e
      end
    | Opr (Cat (s1, s2))    -> begin
        match eval s1, eval s2 with
        | Val s1, Val s2 -> Val (Exp.str (str s1 ^ str s2))
        | Err e, _ | _, Err e -> Err e
      end
    | Opr (Len s)           -> begin
        match eval s with
        | Val s -> Val (Exp.num (String.length (str s)))
        | err -> err
      end
    | Opr (Annot (e, _))    -> eval e
    | Opr (Let (e, _, bnd)) -> begin
        match eval e with
        | Val v -> eval (let_ v bnd)
        | err   -> err
      end
    | Opr (App (lam, e2))    -> begin
        match eval lam with
        | Val (Opr (Lam (_, bnd))) -> (match eval e2 with | Val e2' -> eval (app bnd e2') | err -> err)
        | err -> err
      end
    | Var v                 -> raise (Statics.Unbound_var v)
    | Bnd (_, _)            -> raise (Illformed exp)

  and value e = match eval e with
    | Val v -> v
    | _ -> raise (Illformed e)

  and let_ value = function
    | Bnd (bnd, exp) -> Exp.subst ~value bnd exp
    | e              -> raise (Illformed e)

  and app m n = match m with
    | Bnd (bnd, exp) -> Exp.subst ~value:n bnd exp
    | e              -> raise (Illformed e)

  and num e = match eval e with
    | Val (Opr (Num n)) -> n
    | _                 -> raise (Illformed e)

  and str e = match eval e with
    | Val (Opr (Str str)) -> str
    | _                   -> raise (Illformed e)
end
