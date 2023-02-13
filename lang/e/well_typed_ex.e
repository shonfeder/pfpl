let z : Num =
    let x : Num = 2 in
    let y : Num = 3 in
    x + (y + 5)
in

let a : Str = "foo" in

let b : Str = "bar" in

let concat_words : Str -> Str -> Str = λ a : Str . λ b : Str . a ++ " " ++ b in

(concat_words a) b
