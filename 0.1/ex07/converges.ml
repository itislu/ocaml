let rec converges f x n =
  if n < 0 then false
  else
    let res = f x in
    if res = x then true else converges f res (n - 1)

let () =
  converges (( * ) 2) 2 5 |> Printf.printf "%b\n";
  converges (fun x -> x / 2) 2 3 |> Printf.printf "%b\n";
  converges (fun x -> x / 2) 2 2 |> Printf.printf "%b\n";
  converges (fun x -> x / 2) 2 1 |> Printf.printf "%b\n";
  converges (fun x -> x / 2) 2 0 |> Printf.printf "%b\n";
  converges (fun x -> x / 2) 2 (-1) |> Printf.printf "%b\n";
  converges (fun x -> x * x) 1 0 |> Printf.printf "%b\n"
