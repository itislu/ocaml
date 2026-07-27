let ft_sum exp lo up =
  let rec sigma sum i = if i > up then sum else sigma (sum +. exp i) (i + 1) in
  if up < lo then nan else sigma 0. lo

let () =
  ft_sum (fun i -> float_of_int (i * i)) 1 10 |> Printf.printf "%F\n";
  ft_sum (fun i -> float_of_int (i * i)) 10 10 |> Printf.printf "%F\n";
  ft_sum (fun i -> float_of_int (i * i)) 11 10 |> Printf.printf "%F\n";
  ft_sum (fun i -> float_of_int (i + i)) (-9) 0 |> Printf.printf "%F\n"
