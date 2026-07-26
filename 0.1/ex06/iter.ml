let iter f x n =
  let rec iter_nonneg x n = if n = 0 then x else iter_nonneg (f x) (n - 1) in
  if n < 0 then -1 else iter_nonneg x n

let () =
  iter (fun x -> x * x) 2 4 |> Printf.printf "%d\n";
  iter (fun x -> x * 2) 2 4 |> Printf.printf "%d\n";
  iter (fun x -> x * 2) 2 (-4) |> Printf.printf "%d\n";
  iter (fun x -> x * 2) 42 0 |> Printf.printf "%d\n";
  iter (fun x -> x * 2) 42 1 |> Printf.printf "%d\n"
