(* module type Judgement = sig
 *   type t = Abt.Syntax.t list
 *   [@@deriving eq, fold, map]
 *
 *   val v : t
 *
 *   val to_string : t -> string
 * end
 *
 * module O = struct
 *   type judgement = (module Judgement)
 *
 *   let equal_judgement (module J : Judgement) (module K : Judgement with type t = J.t) = J.equal J.v K.v
 *
 *   type 'a inference =
 *     { premises : 'a list
 *     ; conclusion : 'a
 *     }
 *   [@@deriving eq, fold, map]
 *
 *   type 'a t =
 *     | J of judgement
 *     | I of 'a inference
 *   [@@deriving eq, fold, map]
 * end *)

(* module O = struct
 *   type 'a t =
 *     |
 * end *)
