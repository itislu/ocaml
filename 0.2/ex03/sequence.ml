let rec int_to_str = function
  | 0 -> "0"
  | 1 -> "1"
  | 2 -> "2"
  | 3 -> "3"
  | 4 -> "4"
  | 5 -> "5"
  | 6 -> "6"
  | 7 -> "7"
  | 8 -> "8"
  | 9 -> "9"
  | n -> int_to_str (n / 10) ^ int_to_str (n mod 10)

let rec lst_to_str = function
  | x :: rest -> int_to_str x ^ lst_to_str rest
  | [] -> ""

let sequence n =
  let rec gen_seq count = function
    | x :: y :: rest when x = y -> gen_seq (count + 1) (y :: rest)
    | x :: rest -> count :: x :: gen_seq 1 rest
    | [] -> []
  in
  let rec loop i seq = if i = 1 then seq else gen_seq 1 seq |> loop (i - 1) in
  if n <= 0 then "" else loop n [ 1 ] |> lst_to_str

let () =
  sequence (-1) |> print_endline;
  sequence 0 |> print_endline;
  sequence 1 |> print_endline;
  sequence 2 |> print_endline;
  sequence 3 |> print_endline;
  sequence 4 |> print_endline;
  sequence 5 |> print_endline;
  sequence 6 |> print_endline;
  sequence 7 |> print_endline;
  sequence 8 |> print_endline;
  sequence 9 |> print_endline;
  sequence 10 |> print_endline
