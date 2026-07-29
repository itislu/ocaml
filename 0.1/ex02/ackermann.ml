let ackermann m n =
  let rec ackermann_nonneg m n =
    if m = 0 then n + 1
    else if n = 0 then ackermann_nonneg (m - 1) 1
    else ackermann_nonneg (m - 1) (ackermann_nonneg m (n - 1))
  in
  if m < 0 || n < 0 then -1 else ackermann_nonneg m n

let () =
  ackermann (-1) 7 |> Printf.printf "%d\n";
  ackermann 0 0 |> Printf.printf "%d\n";
  ackermann 2 3 |> Printf.printf "%d\n";
  flush stdout;
  ackermann 4 1 |> Printf.printf "%d\n";
  ackermann 4 0 |> Printf.printf "%d\n";
  ackermann 0 4 |> Printf.printf "%d\n";
  ackermann 4 (-2) |> Printf.printf "%d\n";
  ackermann (-4) (-2) |> Printf.printf "%d\n"
