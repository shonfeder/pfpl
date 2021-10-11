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

module Judgment = struct
  module O = struct
    open Sexplib.Std

    type 'a t =
      | Typ of typing
      | Hyp of 'a hypothetical
    [@@deriving sexp, eq, map, fold]

    and typing =
      { exp : Exp.t
      ; typ : Typ.t
      }

    and 'a hypothetical =
      { ant : 'a list
      ; con : 'a
      }

    let to_string = function
      | Typ { exp; typ } -> Exp.to_string exp ^ ": " ^ Typ.to_string typ
      | Hyp { ant; con } -> String.concat ", " ant ^ " |- " ^ con
  end

  include Abt.Make (O)
end

module Rule = struct
  module O = struct
    open Sexplib.Std

    type 'a t =
      | Inf of 'a inference
      | Drv of 'a derivation

    and 'a inference =
      { prem : Judgment.t list
      ; conc : Judgment.t
      }

    and 'a derivation =
      { infs: 'a
      ; }
  end
end
(* module Judgment = struct
 *   type typ = Exp.t * Typ.t
 *
 *   type hyp = typ list -> typ option
 *
 *   type t =
 *     | Typ of typ
 *     | Hyp of hyp
 *
 *   let typ e t = (e, t)
 *
 *   let ( let* ) = Option.bind
 *
 *   let unif (e, t) (e', t') =
 *     let* eu = Exp.Unification.(e =.= e') |> Result.to_option in
 *     let* tu = Typ.Unification.(t =.= t') |> Result.to_option in
 *     Some (eu, tu)
 *
 *   let select : typ list -> typ -> (typ list * typ) option =
 *    fun ctx typ ->
 *     let* typ' = List.find_map (unif typ) ctx in
 *     let ctx' = List.filter (fun x -> not (unif typ' x |> Option.is_some)) ctx in
 *     Some (ctx', typ')
 * end
 *
 * module Rule = struct
 *   (\** [ctx.?{typ}] is [Some ctx'] when a typing judgmnet that matches [type]  by
 *       unification is in [ctx] and [ctx'] is [ctx] without [typ]. This expresses
 *       the idiom used in sequent calculus [G,x:t], which separates [x:t] from the
 *       context [G] *\)
 *   let ( .?{} ) ctx t = Judgment.select ctx t
 *
 *   type t = Judgment.hyp list * Judgment.hyp
 *
 *   type inference =
 *     { premises : Judgment.hyp list
 *     ; conclusion : Judgment.hyp
 *     }
 *
 *   (\* type derivation = {derivs: derivation list; conclusion: } *\)
 *
 *   let id : t =
 *     ( []
 *     , fun ctx ->
 *         let x_t = Judgment.typ (Exp.v "x") (Typ.v "t") in
 *         ctx.?{x_t} |> Option.map snd )
 * end *)
