module type Judgement = sig
  type t

  val to_string : t -> string
end

module Rule = struct
  type judgement = (module Judgement)

  type t =
    { premises : judgement list
    ; conclusion : judgement
    }
end
